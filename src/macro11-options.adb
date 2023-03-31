with WL.Command_Line;

package body Macro11.Options is

   pragma Style_Checks (Off);

   function Verbose return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("verbose", ' ');
   end Verbose;

   function Output return String is
   begin
      return WL.Command_Line.Find_Option
               ("output", ' ');
   end Output;

   function Format return String is
   begin
      return WL.Command_Line.Find_Option
               ("format", ' ');
   end Format;

   function Warnings return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("warnings", ' ');
   end Warnings;

end Macro11.Options;
