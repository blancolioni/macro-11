with Ada.Characters.Handling;
with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Macro11.Syntax.Defining_Names;

with Pdp11.Images;
with Pdp11.ISA;

with Macro11.Files;
with Macro11.Paths;
with Macro11.Parser;

with Macro11.Syntax.Bindings;
with Macro11.Syntax.Constants;
with Macro11.Syntax.Line_Sequences;
with Macro11.Syntax.Expressions.Sequences;

with Macro11.Values.Instructions;
with Macro11.Values.Registers;

package body Macro11.Assemblies is

   type Text_File_Writer is
     limited new Macro11.Syntax.Text_Writer with
      record
         Line_Number : Natural := 0;
         File        : Ada.Text_IO.File_Type;
      end record;

   overriding procedure Append
     (This   : in out Text_File_Writer;
      Item   : Macro11.Syntax.Text_Record;
      Target : Macro11.Bytecode.Instance'Class);

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (This   : in out Text_File_Writer;
      Item   : Macro11.Syntax.Text_Record;
      Target : Macro11.Bytecode.Instance'Class)
   is
      use Ada.Text_IO;
   begin
      This.Line_Number := This.Line_Number + 1;
      Ada.Integer_Text_IO.Put (This.File, This.Line_Number, 4);
      Set_Col (This.File, 6);
      if Item.Has_Address then
         Put (This.File,
              Pdp11.Images.Octal_Image (Pdp11.Word_16 (Item.Address)));
      else
         Put (This.File,
              "      ");
      end if;

      declare
         use type Pdp11.Address_Type;
         Address : Pdp11.Address_Type := Item.Address;
      begin
         for I in 1 .. Item.Num_Words loop
            Put (This.File, " ");
            Put (This.File,
                 Pdp11.Images.Octal_Image (Target.Get_Word_16 (Address)));
            Address := Address + 2;
         end loop;

         if Item.Num_Bytes > 0 then
            for I in 1 .. Item.Num_Bytes loop
               Put (This.File, " ");
               Put (This.File,
                    Pdp11.Images.Octal_Image (Target.Get_Word_8 (Address)));
               Address := Address + 1;
            end loop;
         end if;
      end;

      Set_Col (This.File, 36);

      if Item.Binding /= null then
         pragma Assert (Item.Label /= null);
         Put (This.File, Item.Label.To_String & "=" & Item.Binding.To_String);
      else
         if Item.Label /= null then
            Put (This.File, Item.Label.To_String & ":");
         end if;
         if Item.Operator /= null then
            Set_Col (This.File, 44);
            Put (This.File, Item.Operator.To_String);
         end if;
         if Item.Operands /= null then
            Set_Col (This.File, 52);
            declare
               function Image (E : Syntax.Expressions.Reference) return String
               is (E.To_String);
            begin
               Put (This.File,
                    Syntax.Expressions.Sequences.Reference (Item.Operands)
                    .Join (Image'Access, ','));
            end;
         end if;
         if Item.Comment /= null then
            if Item.Label /= null
              or else Item.Operator /= null
              or else Item.Operands /= null
            then
               Set_Col (This.File, 72);
            end if;
            Put (This.File, ";" & Item.Comment.To_String);
         end if;
      end if;

      New_Line (This.File);

   end Append;

   -------------
   -- Compile --
   -------------

   procedure Compile (This : in out Instance'Class) is
   begin
      for Ref of This.Files loop
         Ref.Check (This.Env);
      end loop;

      declare
         PC : Pdp11.Address_Type := 0;
      begin
         for Ref of This.Files loop
            Ref.Allocate (PC);
         end loop;
      end;

      declare
         Context : Macro11.Syntax.Translation_Context;
      begin
         for Ref of This.Files loop
            Ref.Translate (Context, This.Target);
         end loop;
      end;

   end Compile;

   ------------
   -- Create --
   ------------

   function Create return Reference is
      Std  : constant Macro11.Files.Reference :=
               Macro11.Files.File
                 (Macro11.Paths.Config_File ("primitives"));
      Line : Natural := 0;

   begin
      Macro11.Syntax.Set_Context (Std, 1, 1);

      return This : constant Reference := new Instance'
        (Env    => Macro11.Environments.Top_Level,
         Files  => <>,
         Target => <>)
      do
         declare
            Primitives : constant Syntax.Line_Sequences.Reference :=
                           Syntax.Line_Sequences.Create
                             ((Std, 1, 1));

            procedure Bind (Name : String;
                            Value : not null access constant
                              Macro11.Values.Instance'Class);

            procedure Bind (Name        : String;
                            Instruction : Pdp11.ISA.Instruction_Type);

            procedure Register
              (Name     : String;
               Index : Pdp11.ISA.Register_Index);

            ----------
            -- Bind --
            ----------

            procedure Bind (Name  : String;
                            Value : not null access constant
                              Macro11.Values.Instance'Class)
            is
            begin
               Line := Line + 1;
               Macro11.Syntax.Set_Context (Std, Line, 1);

               Primitives.Append
                 (Macro11.Syntax.Bindings.Binding
                    (Macro11.Syntax.Defining_Names.Defining_Name
                         ((Std, Line, 1), Name),
                     Macro11.Syntax.Constants.Create
                       ((Std, Line, 1), Value)));
            end Bind;

            ----------
            -- Bind --
            ----------

            procedure Bind (Name        : String;
                            Instruction : Pdp11.ISA.Instruction_Type)
            is
               use Pdp11.ISA;
            begin
               Bind (Name,
                     Macro11.Values.Instructions.Instruction_Value
                       (Instruction));
               if (Instruction in Double_Operand_Instruction
                   and then Instruction /= I_ADD
                   and then Instruction /= I_SUB)
                 or else Instruction in Sized_Single_Operand_Instruction
               then
                  Bind (Name & "b",
                        Macro11.Values.Instructions.Instruction_Value
                          (Instruction, Byte_Size => True));
               end if;
            end Bind;

            --------------
            -- Register --
            --------------

            procedure Register
              (Name     : String;
               Index    : Pdp11.ISA.Register_Index)
            is
            begin
               Bind (Name, Macro11.Values.Registers.Register (Index));
            end Register;

         begin

            for Instr in Pdp11.ISA.Instruction_Type loop
               declare
                  Image : constant String :=
                            Ada.Characters.Handling.To_Lower
                              (Instr'Image);
                  Name  : constant String :=
                            Image (Image'First + 2 .. Image'Last);
               begin
                  Bind (Name, Instr);
               end;
            end loop;

            for Ch in Character range '0' .. '7' loop
               Register (('r', Ch),
                         Pdp11.ISA.Register_Index'Value ((1 => Ch)));
            end loop;

            Register ("sp", 6);
            Register ("pc", 7);

            This.Files.Append (Syntax.Reference (Primitives));
         end;
      end return;
   end Create;

   ----------------
   -- Has_Errors --
   ----------------

   function Has_Errors (This : Instance'Class) return Boolean is
   begin
      return (for some Ref of This.Files => Ref.Has_Errors);
   end Has_Errors;

   ----------
   -- List --
   ----------

   procedure List (This : Instance'Class) is
   begin
      for Ref of This.Files loop
         declare
            Writer : Text_File_Writer;
         begin
            Ada.Text_IO.Create
              (Writer.File, Ada.Text_IO.Out_File,
               Ref.Base_File_Name & ".lst");

            Ref.List (This.Target, Writer);

            Ada.Text_IO.Close (Writer.File);
         end;
      end loop;
   end List;

   ----------
   -- Read --
   ----------

   procedure Read (This : in out Instance'Class;
                   Path : String)
   is
   begin
      Macro11.Parser.Clear_Errors;

      declare
         File : constant Macro11.Syntax.Reference :=
                  Macro11.Parser.Load (Path);
      begin
         if not Macro11.Parser.Has_Errors then
            This.Files.Append (File);
         end if;
      end;

   end Read;

   -------------------
   -- Report_Errors --
   -------------------

   procedure Report_Errors (This : Instance'Class) is

      procedure Report
        (File_Name    : String;
         Line, Column : Positive;
         Message      : String);

      ------------
      -- Report --
      ------------

      procedure Report
        (File_Name    : String;
         Line, Column : Positive;
         Message      : String)
      is
         Line_Image : constant String := Line'Image;
         Column_Image : constant String := Column'Image;
      begin
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            File_Name & ":"
            & Line_Image (2 .. Line_Image'Last) & ":"
            & Column_Image (2 .. Column_Image'Last) & ": "
            & Message);
      end Report;

   begin
      This.Scan_Errors (Report'Access);
   end Report_Errors;

   -----------------
   -- Scan_Errors --
   -----------------

   procedure Scan_Errors
     (This    : Instance'Class;
      Process : not null access procedure
        (File_Name : String;
         Line, Column : Positive;
         Message : String))
   is
   begin
      for Ref of This.Files loop
         Ref.Scan_Errors (Process);
      end loop;
   end Scan_Errors;

   -----------
   -- Write --
   -----------

   procedure Write
     (This   : Instance'Class;
      Path   : String;
      Format : Output_Format)
   is
      pragma Unreferenced (Format);
   begin
      This.Target.Write (Path);
   end Write;

end Macro11.Assemblies;
