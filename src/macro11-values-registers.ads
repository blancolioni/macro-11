with Pdp11.ISA;

package Macro11.Values.Registers is

   subtype Parent is Macro11.Values.Instance;
   type Instance is new Parent with private;
   type Reference is access constant Instance'Class;

   function Register (Index : Pdp11.ISA.Register_Index) return Reference;

   function Get_Register
     (This : Values.Reference)
      return Pdp11.ISA.Register_Index
     with Pre => This.all in Instance'Class;

private

   type Instance is new Parent with
      record
         Index : Pdp11.ISA.Register_Index;
      end record;

   overriding function To_String (This : Instance) return String;
   overriding function Is_Register (This : Instance) return Boolean is (True);

   function Get_Register
     (This : Values.Reference)
      return Pdp11.ISA.Register_Index
   is (Reference (This).Index);

end Macro11.Values.Registers;
