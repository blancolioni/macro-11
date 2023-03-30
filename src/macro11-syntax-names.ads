private with Macro11.Entries;
private with Macro11.Names;

with Macro11.Syntax.Expressions;
with Macro11.Values;

package Macro11.Syntax.Names is

   subtype Parent is Syntax.Expressions.Instance;
   type Instance is new Parent with private;
   type Reference is access all Instance'Class;

   function Name
     (Context : Macro11.Files.File_Context;
      Text    : String)
      return Reference;

   function Get_Name
     (This : Instance)
      return String;

private

   type Instance is new Parent with
      record
         Name : Macro11.Names.Symbol_Name;
         Item : Macro11.Entries.Reference;
      end record;

   overriding function Class_Name
     (This : Instance)
      return String
   is ("name");

   overriding function Value
     (This : Instance)
      return Macro11.Values.Reference
   is (This.Item.Value);

   overriding function To_String
     (This : Instance)
      return String
   is (Macro11.Names."-" (This.Name));

   overriding procedure Check_Instance
     (This : in out Instance);

   overriding procedure Allocate_Instance
     (This   : in out Instance;
      Offset : in out Pdp11.Address_Type);

   overriding procedure Translate_Instance
     (This    : in out Instance;
      Context : in out Translation_Context;
      Target  : in out Macro11.Bytecode.Instance'Class);

   overriding function Has_Entry
     (This : Instance)
      return Boolean
   is (Macro11.Entries."/=" (This.Item, null));

   overriding function Get_Entry
     (This : Instance)
      return Macro11.Entries.Reference
   is (This.Item);

   function Get_Name
     (This : Instance)
      return String
   is (Macro11.Names."-" (This.Name));

end Macro11.Syntax.Names;
