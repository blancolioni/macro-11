private with Macro11.Entries;
private with Macro11.Names;

package Macro11.Syntax.Labels is

   subtype Parent is Syntax.Instance;
   type Instance is new Parent with private;
   type Reference is not null access all Instance'Class;

   function Label
     (Context : Macro11.Files.File_Context;
      Text    : String;
      Global  : Boolean)
      return Reference;

private

   type Instance is new Parent with
      record
         Name   : Macro11.Names.Symbol_Name;
         Global : Boolean;
         Label  : Macro11.Entries.Reference;
      end record;

   overriding function Class_Name
     (This : Instance)
      return String
   is ("label");

   overriding function To_String
     (This : Instance)
      return String
   is (Macro11.Names."-" (This.Name));

   overriding procedure Check_Instance
     (This : in out Instance);

   overriding procedure Allocate_Instance
     (This   : in out Instance;
      Offset : in out Pdp11.Address_Type);

end Macro11.Syntax.Labels;
