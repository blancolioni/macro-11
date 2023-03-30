private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;

with Pdp11;

package Macro11.Bytecode is

   type Instance is tagged private;
   type Reference is access all Instance'Class;

   function Is_Allocated
     (This    : Instance'Class;
      Address : Pdp11.Address_Type)
      return Boolean;

   function Current_Address
     (This : Instance'Class)
      return Pdp11.Address_Type;

   procedure Set_Current_Address
     (This    : in out Instance'Class;
      Address : Pdp11.Address_Type)
     with Pre => not This.Is_Allocated (Address);

   function Get_Word_8
     (This    : Instance'Class;
      Address : Pdp11.Address_Type)
      return Pdp11.Word_8
     with Pre => This.Is_Allocated (Address);

   function Get_Word_16
     (This    : Instance'Class;
      Address : Pdp11.Address_Type)
      return Pdp11.Word_16
     with Pre => This.Is_Allocated (Address);

   procedure Append
     (This  : in out Instance'Class;
      Value : Pdp11.Word_8);

   procedure Append
     (This  : in out Instance'Class;
      Value : Pdp11.Word_16);

   procedure Write
     (This : Instance'Class;
      Path : String);

private

   package Byte_Vectors is
     new Ada.Containers.Vectors (Natural, Pdp11.Word_8, Pdp11."=");

   type Allocation is
      record
         Base  : Pdp11.Address_Type;
         Bound : Pdp11.Address_Type;
         Bytes : Byte_Vectors.Vector;
      end record;

   package Allocation_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Allocation);

   type Instance is tagged
      record
         Current_Address    : Pdp11.Address_Type := 0;
         Current_Allocation : Allocation_Lists.Cursor;
         Allocated          : Allocation_Lists.List;
      end record;

   function Current_Address
     (This : Instance'Class)
      return Pdp11.Address_Type
   is (This.Current_Address);

end Macro11.Bytecode;
