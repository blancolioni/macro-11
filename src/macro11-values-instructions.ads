with Pdp11.ISA;

package Macro11.Values.Instructions is

   subtype Parent is Macro11.Values.Instance;
   type Instance is new Parent with private;
   type Reference is access constant Instance'Class;

   function Instruction_Value
     (Instruction : Pdp11.ISA.Instruction_Type;
      Byte_Size   : Boolean := False)
      return Reference;

   function Get_Instruction
     (This : Values.Reference)
      return Pdp11.ISA.Instruction_Type
     with Pre => This.all in Instance'Class;

   function Is_Byte_Instruction
     (This : Values.Reference)
      return Boolean
     with Pre => This.all in Instance'Class;

private

   type Instance is new Parent with
      record
         Instruction : Pdp11.ISA.Instruction_Type;
         Byte_Size   : Boolean;
      end record;

   overriding function To_String (This : Instance) return String;
   overriding function Size (This : Instance) return Natural is (2);
   overriding function Is_Instruction (This : Instance) return Boolean
   is (True);

   function Get_Instruction
     (This : Values.Reference)
      return Pdp11.ISA.Instruction_Type
   is (Reference (This).Instruction);

   function Is_Byte_Instruction
     (This : Values.Reference)
      return Boolean
   is (Reference (This).Byte_Size);

end Macro11.Values.Instructions;
