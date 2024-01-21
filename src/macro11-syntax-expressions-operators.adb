with Macro11.Values.Constants;

package body Macro11.Syntax.Expressions.Operators is

   -----------------------
   -- Allocate_Instance --
   -----------------------

   overriding procedure Allocate_Instance
     (This : in out Instance; Offset : in out Pdp11.Address_Type)
   is
   begin
      case This.Operator is
         when Unary_Operator_Type =>
            This.Expression.Allocate (Offset);
         when Binary_Operator_Type =>
            This.Left.Allocate (Offset);
            This.Right.Allocate (Offset);
      end case;
   end Allocate_Instance;

   ------------
   -- Binary --
   ------------

   function Binary
     (Context     : Macro11.Files.File_Context;
      Operator    : Binary_Operator_Type;
      Left, Right : not null access Parent'Class)
      return Reference
   is
   begin
      return This : constant Reference := new Instance (Operator) do
         This.Context := Context;
         This.Left := Expressions.Reference (Left);
         This.Right := Expressions.Reference (Right);
      end return;
   end Binary;

   --------------------
   -- Check_Instance --
   --------------------

   overriding procedure Check_Instance (This : in out Instance) is
   begin
      case This.Operator is
         when Unary_Operator_Type =>
            This.Expression.Check (This.Env);
         when Binary_Operator_Type =>
            This.Left.Check (This.Env);
            This.Right.Check (This.Env);
      end case;
   end Check_Instance;

   --------------
   -- Children --
   --------------

   overriding function Children
     (This : Instance)
      return Reference_Array
   is
   begin
      return Parent (This).Children
        & (case This.Operator is
              when Unary_Operator_Type =>
             [Syntax.Reference (This.Expression)],
              when Binary_Operator_Type =>
             [Syntax.Reference (This.Left),
              Syntax.Reference (This.Right)]);
   end Children;

   --------------------
   -- Has_Word_Value --
   --------------------

   overriding function Has_Word_Value (This : Instance) return Boolean is
   begin
      case This.Operator is
         when Unary_Operator_Type =>
            return This.Expression.Has_Word_Value;
         when Binary_Operator_Type =>
            return This.Left.Has_Word_Value
              and then This.Right.Has_Word_Value;
      end case;
   end Has_Word_Value;

   ---------------
   -- To_String --
   ---------------

   overriding function To_String (This : Instance) return String is
      Ops : constant String := "+-+-*/&|";
   begin
      case This.Operator is
         when Unary_Operator_Type =>
            return Ops (Operator_Type'Pos (This.Operator) + 1)
              & This.Expression.To_String;
         when Binary_Operator_Type =>
            return This.Left.To_String
              & Ops (Operator_Type'Pos (This.Operator) + 1)
              & This.Right.To_String;
      end case;
   end To_String;

   -------------------
   -- To_Word_Value --
   -------------------

   overriding function To_Word_Value (This : Instance) return Pdp11.Word_16 is
      use Pdp11;

      function Left return Word_16
      is (if This.Operator in Unary_Operator_Type
          then This.Expression.Value.To_Word_Value
          else This.Left.Value.To_Word_Value);

      function Right return Word_16
      is (This.Right.Value.To_Word_Value);

      W : constant Word_16 :=
            (case This.Operator is
                when Unary_Plus  => Left,
                when Unary_Minus => (not Left) + 1,
                when Op_Add      => Left + Right,
                when Op_Subtract => Left - Right,
                when Op_Multiply => Left * Right,
                when Op_Divide   => Left / Right,
                when Op_And      => Left and Right,
                when Op_Or       => Left or Right);
   begin
      return W;
   end To_Word_Value;

   ------------------------
   -- Translate_Instance --
   ------------------------

   overriding procedure Translate_Instance
     (This   : in out Instance; Context : in out Translation_Context;
      Target : in out Macro11.Bytecode.Instance'Class)
   is
   begin
      case This.Operator is
         when Unary_Operator_Type =>
            This.Expression.Translate (Context, Target);
         when Binary_Operator_Type =>
            This.Left.Translate (Context, Target);
            This.Right.Translate (Context, Target);
      end case;
   end Translate_Instance;

   -----------
   -- Unary --
   -----------

   function Unary
     (Context    : Macro11.Files.File_Context;
      Operator   : Unary_Operator_Type;
      Expression : not null access Parent'Class)
      return Reference
   is
   begin
      return This : constant Reference := new Instance'
        (Operator   => Operator,
         Context    => Context,
         Errors     => <>,
         Env        => <>,
         Properties => <>,
         Expression => Expressions.Reference (Expression))
      do
         null;
      end return;
   end Unary;

   -----------
   -- Value --
   -----------

   overriding function Value
     (This : Instance)
      return Macro11.Values.Reference
   is
   begin
      if Dispatch (This).Has_Word_Value then
         return Macro11.Values.Reference
           (Macro11.Values.Constants.Constant_Value
              (Pdp11.Word_32
                   (Dispatch (This).To_Word_Value)));
      else
         Dispatch (This).Children (1).Add_Error
           ("non-static expression");
         return Macro11.Values.Reference
           (Macro11.Values.Constants.Constant_Value (0));
      end if;
   end Value;

end Macro11.Syntax.Expressions.Operators;
