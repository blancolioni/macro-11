with Macro11.Values;

package Macro11.Syntax.Expressions is

   subtype Parent is Syntax.Instance;
   type Instance is abstract new Parent with private;
   type Reference is not null access all Instance'Class;

   function Value
     (This : Instance)
      return Macro11.Values.Reference
      is abstract;

   Branch_Instruction     : constant Property_Type;
   Floating_Point_Context : constant Property_Type;

private

   subtype Dispatch is Instance'Class;

   type Instance is abstract new Parent with
      record
         null;
      end record;

   overriding function Has_Word_Value (This : Instance) return Boolean
   is (Dispatch (This).Value.Has_Word_Value);

   overriding function To_Word_Value (This : Instance) return Pdp11.Word_16
   is (Dispatch (This).Value.To_Word_Value);

   Branch_Instruction     : constant Property_Type := "branch-instruction";
   Floating_Point_Context : constant Property_Type := "floating-point-context";

end Macro11.Syntax.Expressions;
