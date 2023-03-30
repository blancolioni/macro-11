private with Ada.Containers.Doubly_Linked_Lists;

generic
   Element_Class : String;
   type Element_Instance is abstract new Syntax.Instance with private;
   type Element_Reference is not null access all Element_Instance'Class;
package Macro11.Syntax.Sequences is

   subtype Parent is Syntax.Instance;
   type Instance is new Parent with private;
   type Reference is access all Instance'Class;

   function Create
     (Context : Macro11.Files.File_Context)
      return Reference;

   function Create
     (Context : Macro11.Files.File_Context;
      Child   : not null access Element_Instance'Class)
      return Reference;

   function Create
     (Context          : Macro11.Files.File_Context;
      Child_1, Child_2 : not null access Element_Instance'Class)
      return Reference;

   procedure Append
     (This : in out Instance'Class;
      Child : not null access Element_Instance'Class);

   function Join
     (This      : Instance'Class;
      To_String : not null access
        function (Element : Element_Reference)
      return String;
      Separator : Character)
      return String;

private

   package Element_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Reference);

   type Instance is new Parent with
      record
         List : Element_Lists.List;
      end record;

   overriding function Children
     (This : Instance)
      return Reference_Array;

   overriding function To_String
     (This : Instance)
      return String;

   overriding function Class_Name
     (This : Instance)
      return String
   is (Element_Class & "-sequence");

   overriding procedure Check_Instance
     (This : in out Instance);

end Macro11.Syntax.Sequences;
