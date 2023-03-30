package body Macro11.Environments is

   Local_Top_Level : aliased Instance;

   ------------------
   -- Create_Child --
   ------------------

   function Create_Child
     (Parent : not null access constant Instance'Class)
      return Reference
   is
   begin
      return new Instance'
        (Parent => Parent_Reference (Parent),
         Map    => <>);
   end Create_Child;

   ---------
   -- Set --
   ---------

   procedure Set
     (This : in out Instance'Class;
      Item :        not null access Macro11.Entries.Instance'Class)
   is
   begin
      if This.Map.Contains (Item.Name) then
         This.Map.Replace (Item.Name, Macro11.Entries.Reference (Item));
      else
         This.Map.Insert (Item.Name, Macro11.Entries.Reference (Item));
      end if;
   end Set;

   ---------------
   -- Top_Level --
   ---------------

   function Top_Level return Reference is
   begin
      return Local_Top_Level'Access;
   end Top_Level;

end Macro11.Environments;
