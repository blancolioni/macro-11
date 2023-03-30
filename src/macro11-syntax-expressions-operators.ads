package Macro11.Syntax.Expressions.Operators is

   type Operator_Type is
     (Unary_Plus, Unary_Minus,
      Op_Add, Op_Subtract, Op_Multiply, Op_Divide,
      Op_And, Op_Or);

   subtype Unary_Operator_Type is
     Operator_Type range Unary_Plus .. Unary_Minus;

   subtype Binary_Operator_Type is
     Operator_Type range Op_Add .. Op_Or;

   subtype Parent is Expressions.Instance;
   type Instance (<>) is new Parent with private;
   type Reference is not null access all Instance'Class;

   function Unary
     (Context    : Macro11.Files.File_Context;
      Operator   : Unary_Operator_Type;
      Expression : not null access Parent'Class)
      return Reference;

   function Binary
     (Context       : Macro11.Files.File_Context;
      Operator      : Binary_Operator_Type;
      Left, Right   : not null access Parent'Class)
      return Reference;

private

   type Instance (Operator : Operator_Type) is new Parent with
      record
         case Operator is
            when Unary_Operator_Type =>
               Expression : Expressions.Reference;
            when Binary_Operator_Type =>
               Left, Right : Expressions.Reference;
         end case;
      end record;

   overriding function Class_Name
     (This : Instance)
      return String
   is ("operator-expression");

   overriding procedure Check_Instance
     (This : in out Instance);

   overriding procedure Allocate_Instance
     (This   : in out Instance;
      Offset : in out Pdp11.Address_Type);

   overriding procedure Translate_Instance
     (This    : in out Instance;
      Context : in out Translation_Context;
      Target  : in out Macro11.Bytecode.Instance'Class);

   overriding function Children
     (This : Instance)
      return Reference_Array;

   overriding function Value
     (This : Instance)
      return Macro11.Values.Reference;

   overriding function To_String
     (This : Instance)
      return String;

   overriding function Has_Word_Value (This : Instance) return Boolean;

   overriding function To_Word_Value (This : Instance) return Pdp11.Word_16;

end Macro11.Syntax.Expressions.Operators;
