with Macro11.Values;

package Macro11.Syntax.Expressions.Values is

   subtype Parent is Expressions.Instance;
   type Instance is new Parent with private;
   type Reference is access all Instance'Class;

   function Value_Expression
     (Context : Macro11.Files.File_Context;
      Value   : not null access constant Macro11.Values.Instance'Class)
      return Expressions.Reference;

private

   type Instance is new Parent with
      record
         Value : Macro11.Values.Reference;
      end record;

   overriding function Class_Name
     (This : Instance)
      return String
   is ("value");

   overriding function Value
     (This : Instance)
      return Macro11.Values.Reference
   is (This.Value);

   overriding function To_String
     (This : Instance)
      return String
   is (This.Value.To_String);

   overriding procedure Check_Instance
     (This : in out Instance);

   overriding procedure Allocate_Instance
     (This   : in out Instance;
      Offset : in out Pdp11.Address_Type);

   overriding function Has_Entry
     (This : Instance)
      return Boolean
   is (False);

   overriding function Get_Entry
     (This : Instance)
      return Macro11.Entries.Reference
   is (raise Constraint_Error with This.To_String & ": value has no entry");

   function Get_Name
     (This : Instance)
      return String
   is (This.Value.To_String);

end Macro11.Syntax.Expressions.Values;
