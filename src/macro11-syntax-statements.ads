with Macro11.Syntax.Comments;
with Macro11.Syntax.Labels;
with Macro11.Syntax.Names;
with Macro11.Syntax.Expressions.Sequences;
with Macro11.Syntax.Sequences;

package Macro11.Syntax.Statements is

   subtype Parent is Syntax.Instance;
   type Instance is new Parent with private;
   type Reference is access all Instance'Class;

   function Create
     (Context : Macro11.Files.File_Context)
      return Reference;

   procedure Add_Label
     (This    : in out Instance'Class;
      Context : Macro11.Files.File_Context;
      Label   : String;
      Global  : Boolean);

   procedure Set_Operator
     (This     : in out Instance'Class;
      Context  : Macro11.Files.File_Context;
      Operator : String);

   procedure Add_Operand
     (This    : in out Instance'Class;
      Operand : Macro11.Syntax.Expressions.Reference);

   procedure Set_Comment
     (This    : in out Instance'Class;
      Context : Macro11.Files.File_Context;
      Comment : String);

private

   package Label_Sequences is
     new Macro11.Syntax.Sequences
       ("label",
        Syntax.Labels.Instance,
        Syntax.Labels.Reference);

   type Optional_Labels is access all Label_Sequences.Instance'Class;

   type Optional_Operator is access all Syntax.Names.Instance'Class;

   type Optional_Operands is
     access all Syntax.Expressions.Sequences.Instance'Class;

   type Optional_Comment is access all Syntax.Comments.Instance'Class;

   type Instance is new Parent with
      record
         Label_Child    : Optional_Labels;
         Operator_Child : Optional_Operator;
         Operands_Child : Optional_Operands;
         Comment_Child  : Optional_Comment;
         Start_Address  : Pdp11.Address_Type;
         Length         : Pdp11.Address_Type;
      end record;

   overriding function Children
     (This : Instance)
      return Reference_Array;

   overriding function Class_Name
     (This : Instance)
      return String
   is ("statement");

   overriding procedure Check_Instance
     (This : in out Instance);

   overriding procedure Allocate_Instance
     (This   : in out Instance;
      Offset : in out Pdp11.Address_Type);

   overriding procedure Translate_Instance
     (This    : in out Instance;
      Context : in out Translation_Context;
      Target  : in out Macro11.Bytecode.Instance'Class);

   overriding procedure List_Instance
     (This   : Instance;
      Target : Macro11.Bytecode.Instance'Class;
      Writer : in out Text_Writer'Class);

end Macro11.Syntax.Statements;
