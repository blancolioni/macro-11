with Ada.Characters.Latin_1;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

with Macro11.Names;

package body Macro11.Parser.Lexer is

   type Token_Record is
      record
         Tok    : Token := Tok_End_Of_Line;
         Text   : Macro11.Names.Symbol_Name;
         Line   : Positive;
         Column : Positive;
      end record;

   package Token_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Token_Record);

   Token_List  : Token_Lists.List;
   Current     : Token_Lists.Cursor;
   Source_File : Macro11.Files.Reference;
   Got_Error   : Boolean := False;

   procedure Load (Path : String);

   ------------------
   -- Clear_Errors --
   ------------------

   procedure Clear_Errors is
   begin
      Got_Error := False;
   end Clear_Errors;

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      Token_List.Clear;
      Current := Token_Lists.No_Element;
   end Close;

   -----------
   -- Error --
   -----------

   procedure Error (Message : String) is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         Macro11.Files.Show_Context (Tok_Context)
         & ": " & Message);
      Got_Error := True;
   end Error;

   ----------------
   -- Has_Errors --
   ----------------

   function Has_Errors return Boolean is
   begin
      return Got_Error;
   end Has_Errors;

   ----------
   -- Load --
   ----------

   procedure Load (Path : String) is
      use Ada.Text_IO;
      File        : File_Type;
      Line_Number : Positive := 1;

   begin
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         declare
            Line             : constant String :=
                                 Get_Line (File) & ' ';
            Scanning         : Boolean := False;
            Scanning_Tok     : Token := Tok_None;
            Scanning_Start   : Natural := 0;
            Scanning_Comment : Boolean := False;

            procedure Append
              (Tok    : Token;
               Start  : Positive;
               Finish : Natural);

            ------------
            -- Append --
            ------------

            procedure Append
              (Tok    : Token;
               Start  : Positive;
               Finish : Natural)
            is
            begin
               Token_List.Append
                 (Token_Record'
                    (Tok    => Tok,
                     Text   => Macro11.Names."+" (Line (Start .. Finish)),
                     Line   => Line_Number,
                     Column => Start));
            end Append;

         begin
            for I in Line'Range loop
               declare
                  use Ada.Characters.Latin_1;
                  Ch : constant Character := Line (I);

                  procedure Finish (Tok : Token);

                  procedure Next (Tok : Token);

                  ------------
                  -- Finish --
                  ------------

                  procedure Finish (Tok : Token) is
                  begin
                     if Scanning then
                        Append (Scanning_Tok, Scanning_Start, I - 1);
                     end if;

                     Append (Tok, I, I);
                     Scanning := False;
                  end Finish;

                  ----------
                  -- Next --
                  ----------

                  procedure Next (Tok : Token) is
                  begin
                     if Scanning then
                        if Tok /= Scanning_Tok then
                           Append (Scanning_Tok, Scanning_Start, I - 1);
                           Scanning_Tok := Tok;
                           Scanning_Start := I;
                        end if;
                     else
                        Scanning := True;
                        Scanning_Start := I;
                        Scanning_Tok := Tok;
                     end if;
                  end Next;

               begin
                  if Scanning_Comment then
                     if I = Line'Last then
                        Append (Tok_Comment, Scanning_Start, I - 1);
                        Scanning := False;
                        Scanning_Comment := False;
                     end if;
                  else

                     case Ch is
                        when ' ' | HT =>
                           if Scanning then
                              Append (Scanning_Tok, Scanning_Start, I - 1);
                              Scanning := False;
                           end if;
                        when '0' .. '9' =>
                           if Scanning
                             and then Scanning_Tok = Tok_Identifier
                           then
                              null;
                           else
                              Next (Tok_Integer_Constant);
                           end if;
                        when 'A' .. 'Z' | 'a' .. 'z' | '_' | '.' =>
                           Next (Tok_Identifier);
                        when '$' =>
                           if Scanning
                             and then Scanning_Tok = Tok_Integer_Constant
                           then
                              Scanning_Tok := Tok_Identifier;
                           else
                              Next (Tok_Identifier);
                           end if;

                        when ':' =>
                           if Scanning and then Scanning_Tok = Tok_Colon then
                              Scanning_Tok := Tok_Double_Colon;
                           else
                              Next (Tok_Colon);
                           end if;

                        when '=' =>
                           if Scanning
                             and then Scanning_Tok = Tok_Equal_Sign
                           then
                              Scanning_Tok := Tok_Double_Equal_Sign;
                           else
                              Next (Tok_Equal_Sign);
                           end if;

                        when ';' =>
                           Next (Tok_Comment);
                           Scanning_Comment := True;
                           Scanning_Start := I + 1;

                        when '%' =>
                           Finish (Tok_Percent_Sign);

                        when '#' =>
                           Finish (Tok_Number_Sign);

                        when '@' =>
                           Finish (Tok_At_Sign);

                        when '(' =>
                           Finish (Tok_Left_Parenthesis);

                        when ')' =>
                           Finish (Tok_Right_Parenthesis);

                        when ',' =>
                           Finish (Tok_Comma);

                        when '<' =>
                           Finish (Tok_Left_Angle_Bracket);

                        when '>' =>
                           Finish (Tok_Right_Angle_Bracket);

                        when '+' =>
                           Finish (Tok_Plus_Sign);

                        when '-' =>
                           Finish (Tok_Minus_Sign);

                        when '*' =>
                           Finish (Tok_Asterisk);

                        when '/' =>
                           Finish (Tok_Slash);

                        when '&' =>
                           Finish (Tok_Ampersand);

                        when '!' =>
                           Finish (Tok_Exclamation_Point);

                        when others =>
                           Finish (Tok_Bad_Character);
                     end case;
                  end if;
               end;
            end loop;

            Append (Tok_End_Of_Line, Line'Last + 1, Line'Last);
            Line_Number := Line_Number + 1;

         end;
      end loop;
   end Load;

   --------------
   -- Next_Tok --
   --------------

   function Next_Tok return Token is
      Next : constant Token_Lists.Cursor :=
               Token_Lists.Next (Current);
   begin
      if Token_Lists.Has_Element (Next) then
         return Token_Lists.Element (Next).Tok;
      else
         return Tok_End_Of_File;
      end if;
   end Next_Tok;

   ----------
   -- Open --
   ----------

   procedure Open (Path : String) is
   begin
      Source_File := Macro11.Files.File (Path);
      Load (Path);
      Token_List.Append
        (Token_Record'
           (Tok    => Tok_End_Of_File,
            Text   => Macro11.Names."+" (""),
            Line   => 1,
            Column => 1));
      Current := Token_List.First;
   end Open;

   ----------
   -- Scan --
   ----------

   procedure Scan is
   begin
      Token_Lists.Next (Current);
   end Scan;

   ---------
   -- Tok --
   ---------

   function Tok return Token is
   begin
      return Token_Lists.Element (Current).Tok;
   end Tok;

   ----------------
   -- Tok_Column --
   ----------------

   function Tok_Column return Positive is
   begin
      return Token_Lists.Element (Current).Column;
   end Tok_Column;

   -----------------
   -- Tok_Context --
   -----------------

   function Tok_Context return Macro11.Files.File_Context is
   begin
      return (Tok_File, Tok_Line, Tok_Column);
   end Tok_Context;

   --------------
   -- Tok_File --
   --------------

   function Tok_File return Macro11.Files.Reference is
   begin
      return Source_File;
   end Tok_File;

   --------------
   -- Tok_Line --
   --------------

   function Tok_Line return Positive is
   begin
      return Token_Lists.Element (Current).Line;
   end Tok_Line;

   --------------
   -- Tok_Text --
   --------------

   function Tok_Text return String is
   begin
      return Macro11.Names."-" (Token_Lists.Element (Current).Text);
   end Tok_Text;

end Macro11.Parser.Lexer;
