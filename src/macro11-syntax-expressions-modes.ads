package Macro11.Syntax.Expressions.Modes is

   subtype Parent is Expressions.Instance;
   type Instance is new Parent with private;
   type Reference is not null access all Instance'Class;

   function Immediate
     (Context    : Macro11.Files.File_Context;
      Expression : not null access Parent'Class;
      Deferred   : Boolean := False)
      return Reference;

   function Mode
     (Context       : Macro11.Files.File_Context;
      Expression    : not null access Parent'Class;
      Deferred      : Boolean := False;
      Autoincrement : Boolean := False;
      Autodecrement : Boolean := False)
      return Reference
     with Pre => not (Autodecrement and then Autoincrement);

   function Indexed
     (Context       : Macro11.Files.File_Context;
      Expression    : not null access Parent'Class;
      Index         : not null access Parent'Class;
      Deferred      : Boolean := False)
      return Reference;

private

   type Instance is new Parent with
      record
         Deferred      : Boolean := False;
         Immediate     : Boolean := False;
         Relative      : Boolean := False;
         Autoincrement : Boolean := False;
         Autodecrement : Boolean := False;
         Indexed       : Boolean := False;
         Expression    : Expressions.Reference;
         Index         : Optional_Reference;
      end record;

   overriding function Class_Name
     (This : Instance)
      return String
   is ("mode-expression");

   overriding procedure Check_Instance
     (This : in out Instance);

   overriding procedure Allocate_Instance
     (This   : in out Instance;
      Offset : in out Pdp11.Address_Type);

   overriding procedure Translate_Instance
     (This    : in out Instance;
      Context : in out Translation_Context;
      Target  : in out Macro11.Bytecode.Instance'Class);

   overriding function Value
     (This : Instance)
      return Macro11.Values.Reference
   is (This.Expression.Value);

   overriding function To_String
     (This : Instance)
      return String;

   overriding function Has_Word_Value (This : Instance) return Boolean
   is (This.Expression.Has_Word_Value);

   overriding function To_Word_Value (This : Instance) return Pdp11.Word_16
   is (This.Expression.To_Word_Value);

end Macro11.Syntax.Expressions.Modes;
