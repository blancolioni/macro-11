with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with Macro11.Assemblies;

procedure Macro11.Driver is
   Asm : constant Macro11.Assemblies.Reference :=
           Macro11.Assemblies.Create;

begin

   if Ada.Command_Line.Argument_Count = 0 then
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         Ada.Command_Line.Command_Name
         & ": no input files");
      Ada.Command_Line.Set_Exit_Status (2);
      return;
   end if;

   Asm.Read ("e:/git/macro-11/share/macro11/standard.s");

   for I in 1 .. Ada.Command_Line.Argument_Count loop
      declare
         Source : constant String := Ada.Command_Line.Argument (I);
      begin
         if not Ada.Directories.Exists (Source) then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Source & ": not found");
            Ada.Command_Line.Set_Exit_Status (1);
            return;
         else
            Asm.Read (Source);
         end if;
      end;
   end loop;

   if Asm.Has_Errors then
      Asm.Report_Errors;
      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;

   Asm.Compile;

   if Asm.Has_Errors then
      Asm.Report_Errors;
      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;

   Asm.List;

   declare
      Format : constant Macro11.Assemblies.Output_Format :=
                 Assemblies.A_Out;
   begin
      Asm.Write ("a.out", Format);

   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "write failed: " & Ada.Exceptions.Exception_Message (E));
         Ada.Command_Line.Set_Exit_Status (4);
         return;
   end;

end Macro11.Driver;
