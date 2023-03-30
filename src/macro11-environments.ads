private with WL.String_Maps;

with Macro11.Entries;

private package Macro11.Environments is

   type Instance is tagged limited private;
   type Reference is not null access all Instance;

   function Contains
     (This : Instance'Class;
      Name : String)
      return Boolean;

   function Locally_Contains
     (This : Instance'Class;
      Name : String)
      return Boolean;

   function Get
     (This : Instance'Class;
      Name : String)
      return not null Macro11.Entries.Reference
     with Pre => This.Contains (Name);

   procedure Set
     (This : in out Instance'Class;
      Item : not null access Macro11.Entries.Instance'Class)
     with Post => This.Contains (Item.Name)
     and then Macro11.Entries."="
       (This.Get (Item.Name), Macro11.Entries.Reference (Item));

   function Top_Level return Reference;

   function Create_Child
     (Parent : not null access constant Instance'Class)
      return Reference;

private

   package Entry_Maps is
     new WL.String_Maps (Macro11.Entries.Reference,
                         Macro11.Entries."=");

   type Parent_Reference is access constant Instance'Class;

   type Instance is tagged limited
      record
         Parent : Parent_Reference;
         Map    : Entry_Maps.Map;
      end record;

   function Contains
     (This : Instance'Class;
      Name : String)
      return Boolean
   is (This.Map.Contains (Name)
       or else (This.Parent /= null and then This.Parent.Contains (Name)));

   function Locally_Contains
     (This : Instance'Class;
      Name : String)
      return Boolean
   is (This.Map.Contains (Name));

   function Get
     (This : Instance'Class;
      Name : String)
      return not null Macro11.Entries.Reference
   is (if This.Map.Contains (Name)
       then This.Map.Element (Name)
       else This.Parent.Get (Name));

end Macro11.Environments;
