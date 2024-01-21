package body Macro11.Syntax.Expressions.Values is

   -----------------------
   -- Allocate_Instance --
   -----------------------

   overriding procedure Allocate_Instance
     (This : in out Instance; Offset : in out Pdp11.Address_Type)
   is
   begin
      null;
   end Allocate_Instance;

   --------------------
   -- Check_Instance --
   --------------------

   overriding procedure Check_Instance (This : in out Instance) is
   begin
      null;
   end Check_Instance;

   ----------------------
   -- Value_Expression --
   ----------------------

   function Value_Expression
     (Context : Macro11.Files.File_Context;
      Value   : not null access constant Macro11.Values.Instance'Class)
      return Expressions.Reference
   is
   begin
      return new Instance'
        (Context    => Context,
         Value      => Macro11.Values.Reference (Value),
         others     => <>);
   end Value_Expression;

end Macro11.Syntax.Expressions.Values;
