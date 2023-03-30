private with Macro11.Entries;
private with Macro11.Names;

with Macro11.Syntax.Defining_Names;
with Macro11.Syntax.Expressions;
with Macro11.Values;

package Macro11.Syntax.Bindings is

   subtype Parent is Syntax.Instance;
   type Instance is new Parent with private;
   type Reference is access all Instance'Class;

   function Binding
     (Name  : not null access Macro11.Syntax.Defining_Names.Instance'Class;
      Value : not null access Macro11.Syntax.Expressions.Instance'Class)
      return Reference;

private

   type Instance is new Parent with
      record
         Name_Child  : Macro11.Syntax.Defining_Names.Reference;
         Value_Child : Macro11.Syntax.Expressions.Reference;
         Bound_Name  : Macro11.Names.Symbol_Name;
         Bound_Entry : Macro11.Entries.Reference;
         Bound_Value : Macro11.Values.Reference;
      end record;

   overriding function Children
     (This : Instance)
      return Reference_Array
   is (Macro11.Syntax.Reference (This.Name_Child),
       Macro11.Syntax.Reference (This.Value_Child));

   overriding function Class_Name
     (This : Instance)
      return String
   is ("binding");

   overriding procedure Check_Instance
     (This : in out Instance);

   overriding procedure List_Instance
     (This   : Instance;
      Target : Macro11.Bytecode.Instance'Class;
      Writer : in out Text_Writer'Class);

end Macro11.Syntax.Bindings;
