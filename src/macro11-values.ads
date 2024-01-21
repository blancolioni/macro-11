with Pdp11;

package Macro11.Values is

   type Instance is abstract tagged private;
   type Reference is access constant Instance'Class;

   function To_String (This : Instance) return String is abstract;

   function Has_Word_Value (This : Instance) return Boolean;
   function To_Word_Value (This : Instance) return Pdp11.Word_16
     with Pre'Class => This.Has_Word_Value;

   function To_DWord_Value (This : Instance) return Pdp11.Word_32;
     --  with Pre'Class => This.Has_Word_Value;

   function Size (This : Instance) return Natural;

   function Is_Instruction (This : Instance) return Boolean;
   function Is_Register (This : Instance) return Boolean;

private

   type Instance is abstract tagged null record;

   function Has_Word_Value (This : Instance) return Boolean is (False);
   function To_Word_Value (This : Instance) return Pdp11.Word_16 is (0);
   function To_DWord_Value (This : Instance) return Pdp11.Word_32
   is (Pdp11.Word_32 (Instance'Class (This).To_Word_Value));

   function Size (This : Instance) return Natural is (0);

   function Is_Instruction (This : Instance) return Boolean is (False);
   function Is_Register (This : Instance) return Boolean is (False);

end Macro11.Values;
