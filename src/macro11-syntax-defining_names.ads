private with Macro11.Entries;
private with Macro11.Names;

package Macro11.Syntax.Defining_Names is

   subtype Parent is Syntax.Instance;
   type Instance is new Parent with private;
   type Reference is not null access all Instance'Class;

   function Defining_Name
     (Context : Macro11.Files.File_Context;
      Text    : String)
      return Reference;

   function Get_Name
     (This : Instance)
      return String;

private

   type Instance is new Parent with
      record
         Name     : Macro11.Names.Symbol_Name;
         Item     : Macro11.Entries.Reference;
      end record;

   overriding function Class_Name
     (This : Instance)
      return String
   is ("defining-name");

   overriding function To_String
     (This : Instance)
      return String
   is (Macro11.Names."-" (This.Name));

   overriding procedure Check_Instance
     (This : in out Instance);

   function Get_Name
     (This : Instance)
      return String
   is (Macro11.Names."-" (This.Name));

end Macro11.Syntax.Defining_Names;
