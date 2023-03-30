with Pdp11.Images;

package body Macro11.Values.Constants is

   --------------------
   -- Constant_Value --
   --------------------

   function Constant_Value (From : Pdp11.Word_16)
                            return Reference
   is
   begin
      return new Instance'(Word_Value => From);
   end Constant_Value;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String (This : Instance) return String is
   begin
      return Pdp11.Images.Octal_Image (This.Word_Value);
   end To_String;

end Macro11.Values.Constants;
