with Macro11.Values;
with Macro11.Syntax.Expressions;

package Macro11.Syntax.Constants is

   subtype Parent is Syntax.Expressions.Instance;
   type Instance is new Parent with private;
   type Reference is access all Instance'Class;

   function Create
     (Context : Macro11.Files.File_Context;
      Value : not null access constant Macro11.Values.Instance'Class)
      return Reference;

private

   type Instance is new Parent with
      record
         Value : Macro11.Values.Reference;
      end record;

   overriding function Class_Name
     (This : Instance)
      return String
   is ("constant");

   overriding function Value
     (This : Instance)
      return Macro11.Values.Reference
   is (This.Value);

   overriding function To_String
     (This : Instance)
      return String
   is (This.Value.To_String);

   overriding procedure Check_Instance
     (This : in out Instance)
   is null;

end Macro11.Syntax.Constants;
