private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with WL.String_Sets;
private with Macro11.Files;

with Pdp11.ISA;

with Macro11.Bytecode;
with Macro11.Entries;
with Macro11.Environments;

private package Macro11.Syntax is

   type Instance is abstract tagged limited private;
   type Reference is not null access all Instance'Class;
   type Optional_Reference is access all Instance'Class;

   type Reference_Array is array (Positive range <>) of Reference;
   Empty_Reference_Array : constant Reference_Array (1 .. 0) := (others => <>);

   function Children
     (This : Instance)
      return Reference_Array;

   function Class_Name
     (This : Instance)
      return String
   is abstract;

   function To_String (This : Instance) return String;

   function Has_Word_Value (This : Instance) return Boolean;
   function To_Word_Value (This : Instance) return Pdp11.Word_16
     with Pre'Class => This.Has_Word_Value;

   procedure Check
     (This : in out Instance'Class;
      Env  : not null access Macro11.Environments.Instance'Class);

   procedure Allocate
     (This   : in out Instance'Class;
      Offset : in out Pdp11.Address_Type);

   procedure Check_Instance
     (This : in out Instance)
   is abstract;

   procedure Allocate_Instance
     (This   : in out Instance;
      Offset : in out Pdp11.Address_Type);

   type Translation_Context is private;

   procedure Translate
     (This    : in out Instance'Class;
      Context : in out Translation_Context;
      Target  : in out Macro11.Bytecode.Instance'Class);

   procedure Translate_Instance
     (This   : in out Instance;
      Context : in out Translation_Context;
      Target  : in out Macro11.Bytecode.Instance'Class);

   type Text_Writer is limited interface;

   type Word_Array is array (1 .. 4) of Pdp11.Word_16;
   type Byte_Array is array (1 .. 4) of Pdp11.Word_8;

   type Text_Record is
      record
         Has_Address : Boolean := False;
         Num_Words   : Natural := 0;
         Num_Bytes   : Natural := 0;
         Address     : Pdp11.Address_Type := 0;
         Label       : access Instance'Class;
         Binding     : access Instance'Class;
         Operator    : access Instance'Class;
         Operands    : access Instance'Class;
         Comment     : access Instance'Class;
      end record;

   procedure Append
     (This   : in out Text_Writer;
      Item   : Text_Record;
      Target : Macro11.Bytecode.Instance'Class)
   is abstract;

   procedure List
     (This   : Instance'Class;
      Target : Macro11.Bytecode.Instance'Class;
      Writer : in out Text_Writer'Class);

   procedure List_Instance
     (This   : Instance;
      Target : Macro11.Bytecode.Instance'Class;
      Writer : in out Text_Writer'Class);

   function Has_Entry
     (This : Instance)
      return Boolean;

   function Get_Entry
     (This : Instance)
      return Macro11.Entries.Reference;

   function Has_Errors
     (This : Instance'Class)
      return Boolean;

   procedure Scan_Errors
     (This    : Instance'Class;
      Process : not null access
        procedure (File_Name : String;
                   Line, Column : Positive;
                   Message : String));

   procedure Add_Error
     (This    : in out Instance'Class;
      Message : String);

   procedure Set_Context
     (File    : Macro11.Files.Reference;
      Line   : Positive;
      Column : Positive);

   function Base_File_Name
     (This : Instance'Class)
      return String;

   function Show_Context
     (This : Instance'Class)
      return String;

   procedure Set_Context
     (Context : Macro11.Files.File_Context);

   type Property_Type (<>) is private;

   procedure Set_Property
     (This : in out Instance'Class;
      Property : Property_Type);

   procedure Clear_Property
     (This     : in out Instance'Class;
      Property : Property_Type);

   function Has_Property
     (This     : Instance'Class;
      Property : Property_Type)
      return Boolean;

   No_Allocation      : constant Property_Type;

private

   type Property_Type is new String;

   subtype Dispatch is Instance'Class;

   Current_Context : Macro11.Files.File_Context;

   package Error_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Instance is abstract tagged limited
      record
         Context    : Macro11.Files.File_Context := Current_Context;
         Errors     : Error_Lists.List;
         Env        : access Macro11.Environments.Instance'Class;
         Properties : WL.String_Sets.Set;
      end record;

   function Children
     (This : Instance)
      return Reference_Array
   is (Empty_Reference_Array);

   function Base_File_Name
     (This : Instance'Class)
      return String
   is (This.Context.File.Base_Name);

   function Has_Word_Value (This : Instance) return Boolean is (False);

   function To_Word_Value (This : Instance) return Pdp11.Word_16 is (0);

   procedure Log
     (This : Instance'Class;
      Message : String);

   function Has_Property
     (This     : Instance'Class;
      Property : Property_Type)
      return Boolean
   is (This.Properties.Contains (String (Property)));

   type Context_State is
     (Start,
      Encode_Src, Encode_Dst, Write_Opcodes
     );

   type Translation_Context is
      record
         Next_State  : Context_State := Start;
         Address     : Pdp11.Address_Type := 0;
         Instruction : Pdp11.ISA.Instruction_Record;
         Src_Word    : Pdp11.Word_16 := 0;
         Dst_Word    : Pdp11.Word_16 := 0;
      end record;

   No_Allocation      : constant Property_Type := "no-allocation";

end Macro11.Syntax;
