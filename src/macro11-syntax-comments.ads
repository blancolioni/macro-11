private with Macro11.Names;

package Macro11.Syntax.Comments is

   subtype Parent is Syntax.Instance;
   type Instance is new Parent with private;
   type Reference is access all Instance'Class;

   function Comment
     (Text  : String)
      return Reference;

private

   type Instance is new Parent with
      record
         Comment  : Macro11.Names.Symbol_Name;
      end record;

   overriding function Class_Name
     (This : Instance)
      return String
   is ("comment");

   overriding function To_String
     (This : Instance)
      return String
   is (Macro11.Names."-" (This.Comment));

   overriding procedure Check_Instance
     (This : in out Instance)
   is null;

   function Comment
     (Text  : String)
      return Reference
   is (new Instance'(Parent with Comment => Macro11.Names."+" (Text)));

end Macro11.Syntax.Comments;
