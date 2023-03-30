private with Macro11.Names;

with Macro11.Values;

private package Macro11.Entries is

   type Instance is tagged limited private;
   type Reference is access all Instance'Class;

   function Name (This : Instance'Class) return String;
   function Value (This : Instance'Class) return Macro11.Values.Reference;
   function Defined (This : Instance'Class) return Boolean;

   function Create_With_Value
     (Name  : String;
      Value : not null access constant Macro11.Values.Instance'Class)
      return Reference;

   function Create
     (Name  : String)
      return Reference;

   procedure Define
     (This  : in out Instance'Class;
      Value : not null access constant Macro11.Values.Instance'Class)
     with Pre => not This.Defined,
       Post => This.Defined;

private

   type Instance is tagged limited
      record
         Name : Macro11.Names.Symbol_Name;
         Value : Macro11.Values.Reference;
      end record;

   function Name (This : Instance'Class) return String
   is (Macro11.Names."-" (This.Name));

   function Value (This : Instance'Class) return Macro11.Values.Reference
   is (This.Value);

end Macro11.Entries;
