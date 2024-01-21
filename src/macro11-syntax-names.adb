with Macro11.Values.Instructions;
with Macro11.Values.Registers;

package body Macro11.Syntax.Names is

   -----------------------
   -- Allocate_Instance --
   -----------------------

   overriding procedure Allocate_Instance
     (This   : in out Instance;
      Offset : in out Pdp11.Address_Type)
   is
      use type Pdp11.Address_Type;
      use type Macro11.Values.Reference;
      Name : constant String := Macro11.Names."-" (This.Name);
   begin
      if not This.Has_Entry then
         This.Add_Error ("undefined: " & Name);
      elsif This.Get_Entry.Value = null then
         Offset := Offset + 2;
      else
         Offset := Offset + Pdp11.Address_Type (This.Get_Entry.Value.Size);
      end if;
   end Allocate_Instance;

   --------------------
   -- Check_Instance --
   --------------------

   overriding procedure Check_Instance
     (This : in out Instance)
   is
      Name : constant String := Macro11.Names."-" (This.Name);
   begin
      if This.Env.Contains (Name) then
         This.Item := This.Env.Get (Name);

         if This.Item.Defined then
            if This.Item.Value.Is_Instruction then
               case Values.Instructions.Get_Instruction (This.Item.Value) is
                  when Pdp11.ISA.Branch_Instruction =>
                     This.Set_Property (Expressions.Branch_Instruction);
                  when Pdp11.ISA.Floating_Point_Instruction =>
                     This.Set_Property (Expressions.Floating_Point_Context);
               when others =>
                  null;
               end case;
            end if;
         end if;
      else
         This.Item := Macro11.Entries.Create (Name);
         This.Env.Set (This.Item);
      end if;
   end Check_Instance;

   ----------
   -- Name --
   ----------

   function Name
     (Context : Macro11.Files.File_Context;
      Text    : String)
      return Reference
   is
   begin
      return This : constant Reference := new Instance'
        (Syntax.Expressions.Instance with
           Name     => Macro11.Names."+" (Text),
         Item     => null)
      do
         This.Context := Context;
      end return;
   end Name;

   ------------------------
   -- Translate_Instance --
   ------------------------

   overriding procedure Translate_Instance
     (This    : in out Instance;
      Context : in out Translation_Context;
      Target  : in out Macro11.Bytecode.Instance'Class)
   is
      use Pdp11, Pdp11.ISA;
      Value : constant Macro11.Values.Reference :=
                This.Item.Value;
   begin
      if Value.Is_Instruction then
         declare
            Op : constant Instruction_Type :=
                   Macro11.Values.Instructions.Get_Instruction (Value);
            Byte : constant Boolean :=
                     Macro11.Values.Instructions.Is_Byte_Instruction (Value);
         begin
            if Has_Source_Operand (Op) then
               Context.Next_State := Encode_Src;
            elsif Has_Source_Register (Op) then
               Context.Next_State := Encode_Src;
            elsif Has_Destination_Operand (Op) then
               Context.Next_State := Encode_Dst;
            else
               Context.Next_State := Write_Opcodes;
            end if;
            Context.Instruction.Instruction := Op;
            Context.Instruction.Word := not Byte;
         end;
      elsif Value.Is_Register then
         if Context.Next_State = Encode_Src then
            Context.Instruction.Src := (Register_Mode, False,
                                        Values.Registers.Get_Register
                                          (Value));
            if Has_Destination_Operand (Context.Instruction.Instruction) then
               Context.Next_State := Encode_Dst;
            else
               Context.Next_State := Write_Opcodes;
            end if;
         elsif Context.Next_State = Encode_Dst then
            Context.Instruction.Dst := (Register_Mode, False,
                                        Values.Registers.Get_Register
                                          (Value));
            Context.Next_State := Write_Opcodes;
         end if;
      elsif Has_Branch_Operand
        (Context.Instruction.Instruction)
      then
         if not This.Item.Defined then
            This.Add_Error ("undefined label");
         elsif not Value.Has_Word_Value then
            This.Add_Error ("invalid branch destination");
         else
            declare
               Destination : constant Address_Type :=
                               Address_Type (Value.To_Word_Value);
               Source      : constant Address_Type :=
                               Context.Address + 2;
               Forward     : constant Boolean := Destination >= Source;
               Distance    : constant Address_Type :=
                               (if Forward
                                then Destination - Source
                                else Source - Destination)
                               / 2;
            begin
               if Distance > 128
                 or else (Forward and then Distance > 127)
               then
                  This.Add_Error ("branch too far");
               else
                  Context.Instruction.Offset :=
                    (if Forward
                     then Word_8 (Distance)
                     else Word_8 (256 - Distance));
               end if;
            end;
         end if;
      elsif Value.Has_Word_Value then
         if Context.Next_State = Encode_Src then
            Context.Instruction.Src := (Index_Mode, False, 7);
            Context.Src_Word :=
              Value.To_Word_Value - Word_16 (Context.Address) - 4;
            Context.Next_State := Encode_Dst;
         elsif Context.Next_State = Encode_Dst then
            Context.Instruction.Dst := (Index_Mode, False, 7);
            Context.Dst_Word :=
              Value.To_Word_Value - Word_16 (Context.Address) - 4
              - (if Has_Source_Operand (Context.Instruction.Instruction)
                 then 2 else 0);
            Context.Next_State := Write_Opcodes;
         end if;
      end if;
   end Translate_Instance;

end Macro11.Syntax.Names;
