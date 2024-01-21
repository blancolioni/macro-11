with Pdp11;

package Macro11.Values.Constants is

   subtype Parent is Macro11.Values.Instance;
   type Instance is new Parent with private;
   type Reference is access constant Instance'Class;

   function Constant_Value
     (From : Pdp11.Word_32)
      return Reference;

private

   type Instance is new Parent with
      record
         Word_Value : Pdp11.Word_32;
      end record;

   overriding function To_String (This : Instance) return String;

   overriding function Has_Word_Value (This : Instance) return Boolean
   is (True);

   overriding function To_Word_Value (This : Instance) return Pdp11.Word_16
   is (Pdp11.Word_16 (Pdp11."mod" (This.Word_Value, 65536)));

   overriding function To_DWord_Value (This : Instance) return Pdp11.Word_32
   is (This.Word_Value);

end Macro11.Values.Constants;
