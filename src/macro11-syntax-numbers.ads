with Macro11.Syntax.Values;

package Macro11.Syntax.Numbers is

   subtype Parent is Syntax.Values.Instance;
   type Instance is new Parent with private;
   type Reference is access all Instance'Class;

   function Create
     (Value : Integer)
      return Reference;

private

   type Instance is new Parent with
      record
         null;
      end record;

   overriding function Class_Name
     (This : Instance)
      return String
   is ("number");

   overriding function To_String
     (This : Instance)
      return String
   is (This.Value.To_String);

   overriding procedure Check_Instance
     (This : in out Instance)
   is null;

end Macro11.Syntax.Numbers;
