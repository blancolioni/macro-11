with Pdp11;

package Macro11.Values.Addresses is

   subtype Parent is Macro11.Values.Instance;
   type Instance is new Parent with private;
   type Reference is access constant Instance'Class;

   function Address
     (From     : Pdp11.Address_Type)
      return Reference;

private

   type Instance is new Parent with
      record
         Address  : Pdp11.Address_Type;
         Defined  : Boolean;
      end record;

   overriding function To_String (This : Instance) return String;

   overriding function Has_Word_Value (This : Instance) return Boolean
   is (True);

   overriding function To_Word_Value (This : Instance) return Pdp11.Word_16
   is (Pdp11.Word_16 (This.Address));

end Macro11.Values.Addresses;
