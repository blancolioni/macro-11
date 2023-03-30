with Macro11.Values.Constants;

package body Macro11.Syntax.Numbers is

   ------------
   -- Create --
   ------------

   function Create (Value : Integer) return Reference is
   begin
      return This : constant Reference := new Instance do
         This.Set_Value (Macro11.Values.Constants.Constant_Value (Value));
      end return;
   end Create;

end Macro11.Syntax.Numbers;
