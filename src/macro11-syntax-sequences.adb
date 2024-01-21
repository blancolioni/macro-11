package body Macro11.Syntax.Sequences is

   ------------
   -- Append --
   ------------

   procedure Append
     (This  : in out Instance'Class;
      Child : not null access Element_Instance'Class)
   is
   begin
      This.List.Append (Element_Reference (Child));
   end Append;

   --------------------
   -- Check_Instance --
   --------------------

   overriding procedure Check_Instance
     (This : in out Instance)
   is
   begin
      for Child of This.List loop
         Child.Check (This.Env);
      end loop;
   end Check_Instance;

   --------------
   -- Children --
   --------------

   overriding function Children (This : Instance) return Reference_Array is

      function To_Array
        (Position : Element_Lists.Cursor)
         return Reference_Array;

      --------------
      -- To_Array --
      --------------

      function To_Array
        (Position : Element_Lists.Cursor)
         return Reference_Array
      is
      begin
         if not Element_Lists.Has_Element (Position) then
            return Empty_Reference_Array;
         else
            return [Syntax.Reference (Element_Lists.Element (Position))]
              & To_Array (Element_Lists.Next (Position));
         end if;
      end To_Array;

   begin
      return To_Array (This.List.First);
   end Children;

   ------------
   -- Create --
   ------------

   function Create
     (Context : Macro11.Files.File_Context)
      return Reference
   is
   begin
      return new Instance'
        (Context => Context,
         others  => <>);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Context : Macro11.Files.File_Context;
      Child   : not null access Element_Instance'Class)
      return Reference
   is
   begin
      return This : constant Reference := Create (Context) do
         This.Append (Child);
      end return;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Context          : Macro11.Files.File_Context;
      Child_1, Child_2 : not null access Element_Instance'Class)
      return Reference
   is
   begin
      return This : constant Reference := Create (Context) do
         This.Append (Child_1);
         This.Append (Child_2);
      end return;
   end Create;

   ----------
   -- Join --
   ----------

   function Join
     (This      : Instance'Class;
      To_String : not null access
        function (Element : Element_Reference)
      return String;
      Separator : Character)
      return String
   is
      function Join (Position : Element_Lists.Cursor) return String
      is (if Element_Lists.Has_Element (Position)
          then Separator & To_String (Element_Lists.Element (Position))
            & Join (Element_Lists.Next (Position))
          else "");
      Result : constant String := Join (This.List.First);
   begin
      return Result (Result'First + 1 .. Result'Last);
   end Join;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String
     (This : Instance)
      return String
   is
   begin
      if Natural (This.List.Length) = 1 then
         return This.List.First_Element.To_String;
      else
         return Parent (This).To_String;
      end if;
   end To_String;

end Macro11.Syntax.Sequences;
