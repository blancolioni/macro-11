with Ada.Text_IO;
with Macro11.Values.Registers;

package body Macro11.Syntax.Expressions.Modes is

   function To_Operand
     (This : Instance'Class)
      return Pdp11.ISA.Operand_Type;

   -----------------------
   -- Allocate_Instance --
   -----------------------

   overriding procedure Allocate_Instance
     (This : in out Instance; Offset : in out Pdp11.Address_Type)
   is
      use type Pdp11.Address_Type;
      Size : constant Natural :=
               (if This.Immediate and then not This.Deferred
                and then This.Has_Property (Double_Allocation)
                then 4
                elsif This.Immediate or else This.Indexed
                then 2
                else This.Expression.Value.Size);
   begin
      Offset := Offset + Pdp11.Address_Type (Size);
   end Allocate_Instance;

   --------------------
   -- Check_Instance --
   --------------------

   overriding procedure Check_Instance (This : in out Instance) is
   begin
      if This.Indexed then
         This.Index.Check (This.Env);
      end if;
      This.Expression.Check (This.Env);
   end Check_Instance;

   ---------------
   -- Immediate --
   ---------------

   function Immediate
     (Context    : Macro11.Files.File_Context;
      Expression : not null access Parent'Class;
      Deferred   : Boolean := False)
      return Reference
   is
   begin
      return new Instance'
        (Context    => Context,
         Deferred   => Deferred,
         Immediate  => True,
         Expression => Syntax.Expressions.Reference (Expression),
         others     => <>);
   end Immediate;

   -------------
   -- Indexed --
   -------------

   function Indexed
     (Context    : Macro11.Files.File_Context;
      Expression : not null access Parent'Class;
      Index      : not null access Parent'Class;
      Deferred   : Boolean := False)
      return Reference
   is
   begin
      return new Instance'
        (Context    => Context,
         Deferred   => Deferred,
         Indexed    => True,
         Expression => Syntax.Expressions.Reference (Expression),
         Index      => Optional_Reference (Index),
         others     => <>);
   end Indexed;

   ----------
   -- Mode --
   ----------

   function Mode
     (Context       : Macro11.Files.File_Context;
      Expression    : not null access Parent'Class;
      Deferred      : Boolean := False;
      Autoincrement : Boolean := False;
      Autodecrement : Boolean := False)
      return Reference
   is
   begin
      return new Instance'
        (Context       => Context,
         Deferred      => Deferred,
         Autodecrement => Autodecrement,
         Autoincrement => Autoincrement,
         Expression    => Syntax.Expressions.Reference (Expression),
         others        => <>);
   end Mode;

   ----------------
   -- To_Operand --
   ----------------

   function To_Operand
     (This : Instance'Class)
      return Pdp11.ISA.Operand_Type
   is
      use Pdp11.ISA;
      R : constant Register_Index :=
            (if This.Expression.Value.Is_Register
             then Macro11.Values.Registers.Get_Register (This.Expression.Value)
             elsif This.Immediate or else This.Relative
             then 7
             else raise Constraint_Error
               with "unknown register in " & This.Expression.To_String);
   begin
      if R = 0 then
         Ada.Text_IO.Put_Line ("r0: " & This.Expression.To_String);
      end if;

      return (if This.Immediate
              then (Autoincrement_Mode, This.Deferred, 7)
              elsif This.Relative
              then (Index_Mode, This.Deferred, 7)
              elsif This.Autoincrement
              then (Autoincrement_Mode, This.Deferred, R)
              elsif This.Autodecrement
              then (Autodecrement_Mode, This.Deferred, R)
              elsif This.Indexed
              then (Index_Mode, This.Deferred, R)
              else (Register_Mode, This.Deferred, R));
   end To_Operand;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String (This : Instance) return String is
      Value_Image : constant String := This.Expression.To_String;
      Base_Image  : constant String :=
                      (if This.Immediate
                       then '#' & Value_Image
                       elsif This.Relative
                       then Value_Image
                       elsif This.Autoincrement
                       then "(" & Value_Image & ")+"
                       elsif This.Autodecrement
                       then "-(" & Value_Image & ")"
                       elsif This.Indexed
                       then This.Index.To_String & "(" & Value_Image & ")"
                       else Value_Image);
   begin
      return (if This.Deferred then "@" else "") & Base_Image;
   end To_String;

   ------------------------
   -- Translate_Instance --
   ------------------------

   overriding procedure Translate_Instance
     (This   : in out Instance; Context : in out Translation_Context;
      Target : in out Macro11.Bytecode.Instance'Class)
   is
      Op  : constant Pdp11.ISA.Operand_Type :=
              This.To_Operand;
      Imm : constant Pdp11.Word_16 :=
              (if This.Indexed
               and then This.Index.Has_Word_Value
               then This.Index.To_Word_Value
               elsif Pdp11.ISA.Has_Immediate_Word (Op)
               then This.Expression.Value.To_Word_Value
               else 0);
   begin
      if Context.Next_State = Encode_Src then
         Context.Instruction.Src := Op;
         Context.Src_Word := Imm;
         Context.Next_State := Encode_Dst;
      elsif Context.Next_State = Encode_Dst then
         Context.Instruction.Dst := Op;
         Context.Dst_Word := Imm;
         Context.Next_State := Write_Opcodes;
      end if;
      if This.Has_Property (Double_Allocation)
        and Then Pdp11.ISA.Is_Immediate_Operand (Op)
      then
         if This.Expression.Value.Has_Word_Value then
            Context.D_Word := This.Expression.Value.To_DWord_Value;
         end if;
      end if;

   end Translate_Instance;

end Macro11.Syntax.Expressions.Modes;
