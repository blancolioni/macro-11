package body Macro11.Syntax.Defining_Names is

   --------------------
   -- Check_Instance --
   --------------------

   overriding procedure Check_Instance
     (This : in out Instance)
   is
      Name : constant String := Macro11.Names."-" (This.Name);
   begin
      if This.Env.Contains (Name) then
         This.Add_Error ("redefinition of '" & Name & "'");
      end if;
   end Check_Instance;

   ----------
   -- Name --
   ----------

   function Defining_Name
     (Context : Macro11.Files.File_Context;
      Text    : String)
      return Reference
   is
   begin
      return This : constant Reference := new Instance'
        (Parent with
           Name     => Macro11.Names."+" (Text),
         Item     => null)
      do
         This.Context := Context;
      end return;
   end Defining_Name;

end Macro11.Syntax.Defining_Names;
