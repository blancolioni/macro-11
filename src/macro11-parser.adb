with Pdp11;

with Macro11.Parser.Tokens;            use Macro11.Parser.Tokens;
with Macro11.Parser.Lexer;             use Macro11.Parser.Lexer;

with Macro11.Files;

with Macro11.Syntax.Bindings;
with Macro11.Syntax.Constants;
with Macro11.Syntax.Defining_Names;
with Macro11.Syntax.Line_Sequences;
with Macro11.Syntax.Names;
with Macro11.Syntax.Expressions;
with Macro11.Syntax.Expressions.Modes;
with Macro11.Syntax.Statements;

with Macro11.Values.Constants;

package body Macro11.Parser is

   function Parse_Line
      return Macro11.Syntax.Optional_Reference;

   function At_Expression
     return Boolean;

   function At_Mode_Expression
     return Boolean;

   function Parse_Expression
     return Macro11.Syntax.Expressions.Reference;

   function Parse_Mode_Expression
     return Macro11.Syntax.Expressions.Reference;

   -------------------
   -- At_Expression --
   -------------------

   function At_Expression
     return Boolean
   is
   begin
      return Tok in Tok_Identifier | Tok_Integer_Constant
        | Tok_Number_Sign | Tok_Plus_Sign | Tok_Minus_Sign
        | Tok_Left_Parenthesis | Tok_At_Sign;
   end At_Expression;

   ------------------------
   -- At_Mode_Expression --
   ------------------------

   function At_Mode_Expression
     return Boolean
   is
   begin
      return (Tok = Tok_Minus_Sign and then Next_Tok = Tok_Left_Parenthesis)
        or else Tok = Tok_Left_Parenthesis
        or else ((Tok = Tok_Identifier or else Tok = Tok_Integer_Constant)
                 and then Next_Tok = Tok_Left_Parenthesis)
        or else Tok = Tok_Number_Sign or else Tok = Tok_At_Sign;
   end At_Mode_Expression;

   ------------------
   -- Clear_Errors --
   ------------------

   procedure Clear_Errors is
   begin
      Lexer.Clear_Errors;
   end Clear_Errors;

   ----------------
   -- Has_Errors --
   ----------------

   function Has_Errors return Boolean is
   begin
      return Lexer.Has_Errors;
   end Has_Errors;

   ----------
   -- Load --
   ----------

   function Load (Path : String) return Macro11.Syntax.Reference is
   begin
      Open (Path);
      declare
         Source : constant Macro11.Syntax.Line_Sequences.Reference :=
                    Macro11.Syntax.Line_Sequences.Create
                      (Tok_Context);
      begin
         while Tok /= Tok_End_Of_File loop
            declare
               use Macro11.Syntax;
               Line : constant Optional_Reference := Parse_Line;
            begin
               if Line /= null then
                  Source.Append (Line);
               end if;
            end;
         end loop;
         Close;
         return Macro11.Syntax.Reference (Source);
      end;
   end Load;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression
     return Macro11.Syntax.Expressions.Reference
   is
   begin
      if Tok = Tok_Integer_Constant then
         declare
            use Pdp11;
            Value : Word_16 := 0;
            Image : constant String := Tok_Text;
         begin
            for Ch of Image loop
               Value := Value * 8 + Character'Pos (Ch) - 48;
            end loop;

            return Result : constant Macro11.Syntax.Expressions.Reference :=
              Macro11.Syntax.Expressions.Reference
                (Macro11.Syntax.Constants.Create
                   (Tok_Context,
                    Macro11.Values.Constants.Constant_Value (Value)))
            do
               Scan;
            end return;
         end;
      elsif Tok = Tok_Identifier then
         return Result : constant Macro11.Syntax.Expressions.Reference :=
           Macro11.Syntax.Expressions.Reference
             (Macro11.Syntax.Names.Name
                (Tok_Context, Tok_Text))
         do
            Scan;
         end return;
      elsif At_Mode_Expression then
         return Parse_Mode_Expression;
      else
         return Result : constant Macro11.Syntax.Expressions.Reference :=
           Macro11.Syntax.Expressions.Reference
             (Macro11.Syntax.Names.Name
                (Tok_Context, "bad expression: " & Tok_Text))
         do
            Scan;
         end return;
      end if;
   end Parse_Expression;

   ----------------
   -- Parse_Line --
   ----------------

   function Parse_Line
      return Macro11.Syntax.Optional_Reference
   is
      use Macro11.Syntax;
      Context : constant Macro11.Files.File_Context := Tok_Context;
   begin

      if Tok = Tok_Identifier
        and then (Next_Tok = Tok_Equal_Sign
                  or else Next_Tok = Tok_Double_Equal_Sign)
      then
         declare
            Label_Text : constant String := Tok_Text;
            Label      : constant Macro11.Syntax.Defining_Names.Reference :=
                           Macro11.Syntax.Defining_Names.Defining_Name
                             (Tok_Context, Label_Text);
         begin
            Scan;
            Scan;
            declare
               Value : constant Macro11.Syntax.Expressions.Reference :=
                         Parse_Expression;
            begin
               Macro11.Syntax.Set_Context (Context);
               while Tok /= Tok_End_Of_Line
                 and then Tok /= Tok_End_Of_File
               loop
                  Scan;
               end loop;
               Scan;
               return Optional_Reference
                 (Macro11.Syntax.Bindings.Binding (Label, Value));
            end;
         end;
      else
         declare
            Statement : constant Macro11.Syntax.Statements.Reference :=
                          Macro11.Syntax.Statements.Create
                            (Tok_Context);
         begin
            while Tok = Tok_Identifier
              and then (Next_Tok = Tok_Colon
                        or else Next_Tok = Tok_Double_Colon)
            loop
               Statement.Add_Label
                 (Context => Tok_Context,
                  Label   => Tok_Text,
                  Global  =>  Next_Tok = Tok_Double_Colon);
               Scan;
               Scan;
            end loop;

            if Tok = Tok_Identifier then
               Statement.Set_Operator
                 (Tok_Context, Tok_Text);
               Scan;

               while At_Expression loop
                  Statement.Add_Operand (Parse_Expression);

                  if Tok = Tok_Comma then
                     Scan;
                     if not At_Expression then
                        Error ("missing expression");
                     end if;
                  elsif At_Expression then
                     Error ("missing ','");
                  end if;
               end loop;
            end if;

            if Tok = Tok_Comment then
               Statement.Set_Comment (Tok_Context, Tok_Text);
               Scan;
            end if;

            if Tok = Tok_End_Of_Line then
               Scan;
            else
               Error ("expected end of line at " & Tok'Image);
               while Tok /= Tok_End_Of_Line
                 and then Tok /= Tok_End_Of_File
               loop
                  Scan;
               end loop;
               Scan;
            end if;

            return Macro11.Syntax.Optional_Reference (Statement);
         end;
      end if;
   end Parse_Line;

   ---------------------------
   -- Parse_Mode_Expression --
   ---------------------------

   function Parse_Mode_Expression
     return Macro11.Syntax.Expressions.Reference
   is
      Context       : constant Macro11.Files.File_Context := Tok_Context;
      Deferred      : Boolean := False;
      Autoincrement : Boolean := False;
      Autodecrement : Boolean := False;
   begin
      if Tok = Tok_At_Sign then
         Deferred := True;
         Scan;
      end if;
      if Tok = Tok_Number_Sign then
         Scan;
         declare
            Expr : constant Syntax.Expressions.Reference :=
                     Parse_Expression;
         begin
            return Syntax.Expressions.Reference
              (Syntax.Expressions.Modes.Immediate
                 (Context, Expr, Deferred));
         end;
      end if;

      if Tok = Tok_Minus_Sign
        and then Next_Tok = Tok_Left_Parenthesis
      then
         Autodecrement := True;
         Scan;
         Scan;
         declare
            Expr : constant Syntax.Expressions.Reference :=
                     Parse_Expression;
         begin
            if Tok = Tok_Right_Parenthesis then
               Scan;
            else
               Error ("missing ')'");
            end if;

            return Syntax.Expressions.Reference
              (Syntax.Expressions.Modes.Mode
                 (Context       => Context,
                  Expression    => Expr,
                  Deferred      => Deferred,
                  Autodecrement => Autodecrement));
         end;
      elsif Tok = Tok_Left_Parenthesis then
         Scan;
         declare
            Expr : constant Syntax.Expressions.Reference :=
                     Parse_Expression;
         begin
            if Tok = Tok_Right_Parenthesis then
               Scan;
            else
               Error ("missing ')'");
            end if;

            if Tok = Tok_Plus_Sign then
               Autoincrement := True;
               Scan;
            elsif Deferred then
               Error ("double deferral not allowed");
            else
               Deferred := True;
            end if;

            return Syntax.Expressions.Reference
              (Syntax.Expressions.Modes.Mode
                 (Context       => Context,
                  Expression    => Expr,
                  Deferred      => Deferred,
                  Autoincrement => Autoincrement));
         end;
      else
         raise Constraint_Error with
           "Cannot parse a mode starting with '" & Tok_Text & "'";
      end if;
   end Parse_Mode_Expression;

end Macro11.Parser;
