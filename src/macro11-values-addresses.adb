with Pdp11.Images;

package body Macro11.Values.Addresses is

   -------------
   -- Address --
   -------------

   function Address
     (From    : Pdp11.Address_Type)
      return Reference
   is
   begin
      return new Instance'
        (Address => From, Defined => True);
   end Address;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String (This : Instance) return String is
   begin
      return Pdp11.Images.Octal_Image (Pdp11.Word_16 (This.Address));
   end To_String;

end Macro11.Values.Addresses;
