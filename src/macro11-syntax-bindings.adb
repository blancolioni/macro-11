package body Macro11.Syntax.Bindings is

   -------------
   -- Binding --
   -------------

   function Binding
     (Name  : not null access Macro11.Syntax.Defining_Names.Instance'Class;
      Value : not null access Macro11.Syntax.Expressions.Instance'Class)
      return Reference
   is
   begin
      return new Instance'
        (Context => Current_Context,
         Name_Child => Syntax.Defining_Names.Reference (Name),
         Value_Child => Syntax.Expressions.Reference (Value),
         others      => <>);
   end Binding;

   --------------------
   -- Check_Instance --
   --------------------

   overriding procedure Check_Instance
     (This : in out Instance)
   is
      Name : constant String :=
               This.Name_Child.Get_Name;
   begin

      This.Name_Child.Check (This.Env);
      This.Value_Child.Check (This.Env);

      This.Bound_Name := Macro11.Names."+" (Name);
      This.Bound_Entry :=
        Macro11.Entries.Create_With_Value (Name, This.Value_Child.Value);
      This.Env.Set (This.Bound_Entry);
   end Check_Instance;

   -------------------
   -- List_Instance --
   -------------------

   overriding procedure List_Instance
     (This   : Instance;
      Target : Macro11.Bytecode.Instance'Class;
      Writer : in out Text_Writer'Class)
   is
   begin
      Writer.Append
        (Macro11.Syntax.Text_Record'
           (Has_Address => This.Value_Child.Has_Word_Value,
            Address     => Pdp11.Address_Type
              (if This.Value_Child.Has_Word_Value
               then This.Value_Child.To_Word_Value
               else 0),
            Label     => This.Name_Child,
            Binding => This.Value_Child,
            others    => <>),
         Target);

   end List_Instance;

end Macro11.Syntax.Bindings;
