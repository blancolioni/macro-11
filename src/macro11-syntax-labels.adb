with Macro11.Values.Addresses;

package body Macro11.Syntax.Labels is

   -----------------------
   -- Allocate_Instance --
   -----------------------

   overriding procedure Allocate_Instance
     (This   : in out Instance;
      Offset : in out Pdp11.Address_Type)
   is
   begin
      if This.Label.Defined then
         This.Add_Error ("redefinition of " & This.Label.Name);
      else
         This.Label.Define
           (Macro11.Values.Addresses.Address
              (From => Offset));
      end if;
   end Allocate_Instance;

   --------------------
   -- Check_Instance --
   --------------------

   overriding procedure Check_Instance (This : in out Instance) is
      Name : constant String := Macro11.Names."-" (This.Name);
   begin
      if This.Env.Contains (Name) then
         declare
            Item : constant Macro11.Entries.Reference :=
                     This.Env.Get (Name);
         begin
            if Item.Defined then
               This.Add_Error ("redefinition of '" & Name & "'");
            else
               This.Label := Item;
            end if;
         end;
      else
         This.Label :=
           Macro11.Entries.Create
             (Name  => Macro11.Names."-" (This.Name));
         This.Env.Set (This.Label);
      end if;
   end Check_Instance;

   -----------
   -- Label --
   -----------

   function Label
     (Context : Macro11.Files.File_Context;
      Text    : String;
      Global  : Boolean)
      return Reference
   is
   begin
      return new Instance'
        (Context => Context,
         Name    => Macro11.Names."+" (Text),
         Global  => Global,
         others  => <>);
   end Label;

end Macro11.Syntax.Labels;
