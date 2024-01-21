with Pdp11.ISA;

package body Macro11.Syntax.Statements is

   ---------------
   -- Add_Label --
   ---------------

   procedure Add_Label
     (This    : in out Instance'Class;
      Context : Macro11.Files.File_Context;
      Label   : String;
      Global  : Boolean)
   is
   begin
      if This.Label_Child = null then
         This.Label_Child :=
           Optional_Labels (Label_Sequences.Create (Context));
      end if;
      This.Label_Child.Append
        (Syntax.Labels.Label
           (Context, Label, Global));
   end Add_Label;

   -----------------
   -- Add_Operand --
   -----------------

   procedure Add_Operand
     (This    : in out Instance'Class;
      Operand : Macro11.Syntax.Expressions.Reference)
   is
   begin
      if This.Operands_Child = null then
         This.Operands_Child :=
           Optional_Operands
             (Syntax.Expressions.Sequences.Create (Operand.Context));
      end if;
      This.Operands_Child.Append (Operand);
   end Add_Operand;

   -----------------------
   -- Allocate_Instance --
   -----------------------

   overriding procedure Allocate_Instance
     (This   : in out Instance;
      Offset : in out Pdp11.Address_Type)
   is
      use type Pdp11.Address_Type;
   begin
      This.Start_Address := Offset;
      for Child of Dispatch (This).Children loop
         Child.Allocate (Offset);
      end loop;
      This.Length := Offset - This.Start_Address;
   end Allocate_Instance;

   --------------------
   -- Check_Instance --
   --------------------

   overriding procedure Check_Instance
     (This : in out Instance)
   is
      Is_Branch : Boolean := False;
      Is_Float  : Boolean := False;
   begin
      for Child of Dispatch (This).Children loop
         if Is_Branch then
            Child.Set_Property (No_Allocation);
         elsif Is_Float then
            Child.Set_Property (Double_Allocation);
         end if;

         Child.Check (This.Env);

         if Child.Has_Property (Expressions.Branch_Instruction) then
            Is_Branch := True;
         elsif Child.Has_Property (Expressions.Floating_Point_Context) then
            Is_Float := True;
         end if;
      end loop;
   end Check_Instance;

   --------------
   -- Children --
   --------------

   overriding function Children (This : Instance) return Reference_Array is
      Label : constant Reference_Array :=
                (if This.Label_Child = null then Empty_Reference_Array
                 else [1 => Syntax.Reference (This.Label_Child)]);
      Operator : constant Reference_Array :=
                   (if This.Operator_Child = null then Empty_Reference_Array
                    else [Syntax.Reference (This.Operator_Child)]);
      Operands : constant Reference_Array :=
                   (if This.Operands_Child = null then Empty_Reference_Array
                    else [Syntax.Reference (This.Operands_Child)]);
      Comment  : constant Reference_Array :=
                (if This.Comment_Child = null then Empty_Reference_Array
                 else [Syntax.Reference (This.Comment_Child)]);
   begin
      return Label & Operator & Operands & Comment;
   end Children;

   ------------
   -- Create --
   ------------

   function Create
     (Context : Macro11.Files.File_Context)
      return Reference
   is
   begin
      return This : constant Reference := new Instance do
         This.Context := Context;
      end return;
   end Create;

   -------------------
   -- List_Instance --
   -------------------

   overriding procedure List_Instance
     (This   : Instance;
      Target : Macro11.Bytecode.Instance'Class;
      Writer : in out Text_Writer'Class)
   is
   begin
      Writer.Append
        (Macro11.Syntax.Text_Record'
           (Has_Address => This.Label_Child /= null
            or else This.Operator_Child /= null
            or else This.Operands_Child /= null,
            Address     => This.Start_Address,
            Num_Words   => Natural (This.Length) / 2,
            Label       => This.Label_Child,
            Operator    => This.Operator_Child,
            Operands    => This.Operands_Child,
            Comment     => This.Comment_Child,
            others      => <>),
        Target);
   end List_Instance;

   -----------------
   -- Set_Comment --
   -----------------

   procedure Set_Comment
     (This    : in out Instance'Class;
      Context : Macro11.Files.File_Context;
      Comment : String)
   is
      pragma Unreferenced (Context);
   begin
      This.Comment_Child :=
        Optional_Comment
          (Syntax.Comments.Comment (Comment));
   end Set_Comment;

   ------------------
   -- Set_Operator --
   ------------------

   procedure Set_Operator
     (This     : in out Instance'Class;
      Context  : Macro11.Files.File_Context;
      Operator : String)
   is
   begin
      This.Operator_Child :=
        Optional_Operator
          (Syntax.Names.Name (Context, Operator));
   end Set_Operator;

   ------------------------
   -- Translate_Instance --
   ------------------------

   overriding procedure Translate_Instance
     (This    : in out Instance;
      Context : in out Translation_Context;
      Target  : in out Macro11.Bytecode.Instance'Class)
   is
   begin

      Context.Address := This.Start_Address;
      for Child of Dispatch (This).Children loop
         Child.Translate (Context, Target);
      end loop;

      if Context.Next_State = Write_Opcodes then
         Target.Append
           (Pdp11.ISA.Encode (Context.Instruction));
         if Pdp11.ISA.Has_Source_Operand (Context.Instruction.Instruction)
           and then Pdp11.ISA.Has_Immediate_Word (Context.Instruction.Src)
         then
            if Pdp11.ISA.Is_Immediate_Operand (Context.Instruction.Src)
              and then Context.Instruction.Instruction
            in Pdp11.ISA.Floating_Point_Instruction
            then
               Target.Append (Context.D_Word);
            else
               Target.Append (Context.Src_Word);
            end if;
         end if;
         if Pdp11.ISA.Has_Destination_Operand
           (Context.Instruction.Instruction)
           and then Pdp11.ISA.Has_Immediate_Word (Context.Instruction.Dst)
         then
            Target.Append (Context.Dst_Word);
         end if;
      end if;

      Context := (others => <>);
   end Translate_Instance;

end Macro11.Syntax.Statements;
