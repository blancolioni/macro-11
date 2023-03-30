with Macro11.Syntax;

private package Macro11.Parser is

   function Has_Errors return Boolean;
   procedure Clear_Errors;

   function Load
     (Path : String)
      return Macro11.Syntax.Reference;

end Macro11.Parser;
