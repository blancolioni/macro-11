with Ada.Text_IO;

package body Macro11.Logging is

   ---------
   -- Log --
   ---------

   procedure Log (Message : String) is
   begin
      Ada.Text_IO.Put_Line (Message);
   end Log;

end Macro11.Logging;
