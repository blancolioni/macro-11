package body Macro11.Values.Registers is

   --------------
   -- Register --
   --------------

   function Register (Index : Pdp11.ISA.Register_Index) return Reference is
   begin
      return new Instance'(Index => Index);
   end Register;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String (This : Instance) return String is
   begin
      return ['%', Character'Val (48 + Natural (This.Index))];
   end To_String;

end Macro11.Values.Registers;
