package body Macro11.Syntax.Constants is

   ------------
   -- Create --
   ------------

   function Create
     (Context : Macro11.Files.File_Context;
      Value : not null access constant Macro11.Values.Instance'Class)
      return Reference
   is
   begin
      return This : constant Reference := new Instance'
        (Parent with Value => Macro11.Values.Reference (Value))
      do
         This.Context := Context;
      end return;
   end Create;

end Macro11.Syntax.Constants;
