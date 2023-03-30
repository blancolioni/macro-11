package body Macro11.Values.Instructions is

   -----------------------
   -- Instruction_Value --
   -----------------------

   function Instruction_Value
     (Instruction : Pdp11.ISA.Instruction_Type;
      Byte_Size   : Boolean := False)
      return Reference
   is
   begin
      return new Instance'(Instruction => Instruction, Byte_Size => Byte_Size);
   end Instruction_Value;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String (This : Instance) return String is
      Img : constant String := This.Instruction'Image;
   begin
      return Img (3 .. Img'Last);
   end To_String;

end Macro11.Values.Instructions;
