package body Macro11.Entries is

   ------------
   -- Create --
   ------------

   function Create (Name : String) return Reference is
   begin
      return new Instance'
        (Name  => Macro11.Names."+" (Name),
         Value => null);
   end Create;

   -----------------------
   -- Create_With_Value --
   -----------------------

   function Create_With_Value
     (Name  : String;
      Value : not null access constant Macro11.Values.Instance'Class)
      return Reference
   is
   begin
      return new Instance'
        (Name  => Macro11.Names."+" (Name),
         Value => Macro11.Values.Reference (Value));
   end Create_With_Value;

   ------------
   -- Define --
   ------------

   procedure Define
     (This  : in out Instance'Class;
      Value :        not null access constant Macro11.Values.Instance'Class)
   is
   begin
      This.Value := Macro11.Values.Reference (Value);
   end Define;

   -------------
   -- Defined --
   -------------

   function Defined (This : Instance'Class) return Boolean is
      use type Macro11.Values.Reference;
   begin
      return This.Value /= null;
   end Defined;

end Macro11.Entries;
