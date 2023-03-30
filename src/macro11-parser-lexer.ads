with Macro11.Files;

with Macro11.Parser.Tokens;            use Macro11.Parser.Tokens;

private package Macro11.Parser.Lexer is

   procedure Open (Path : String);
   procedure Close;

   procedure Scan;

   procedure Error (Message : String);
   function Has_Errors return Boolean;
   procedure Clear_Errors;

   function Tok return Token;
   function Next_Tok return Token;
   function Tok_Text return String;

   function Tok_Line return Positive;
   function Tok_Column return Positive;
   function Tok_File return Macro11.Files.Reference;

   function Tok_Context return Macro11.Files.File_Context;

end Macro11.Parser.Lexer;
