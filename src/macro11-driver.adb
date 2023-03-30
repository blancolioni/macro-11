with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with WL.Command_Line;

with Macro11.Assemblies;
with Macro11.Options;
with Macro11.Paths;

procedure Macro11.Driver is
   Asm : constant Macro11.Assemblies.Reference :=
           Macro11.Assemblies.Create;

begin

   if not Ada.Directories.Exists (".macro11-options") then
      Ada.Directories.Copy_File
        (Source_Name => Macro11.Paths.Config_File ("default-options.txt"),
         Target_Name => ".macro11-options");
   end if;

   WL.Command_Line.Load_Defaults (".macro11-options");

   if WL.Command_Line.Argument_Count = 0 then
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         Ada.Command_Line.Command_Name
         & ": no input files");
      Ada.Command_Line.Set_Exit_Status (2);
      return;
   end if;

   for I in 1 .. WL.Command_Line.Argument_Count loop
      declare
         Source : constant String := WL.Command_Line.Argument (I);
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
      use all type Macro11.Assemblies.Output_Format;
      Format_Name : constant String :=
                      Ada.Characters.Handling.To_Lower
                        (Macro11.Options.Format);
      Format      : Macro11.Assemblies.Output_Format;
   begin
      if Format_Name = "raw" then
         Format := Raw;
      elsif Format_Name = "a.out" or else Format_Name = "" then
         Format := A_Out;
      else
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "unknown format: " & Format_Name);
         Ada.Command_Line.Set_Exit_Status (3);
         return;
      end if;

      Asm.Write (Macro11.Options.Output, Format);

   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "write failed: " & Ada.Exceptions.Exception_Message (E));
         Ada.Command_Line.Set_Exit_Status (4);
         return;
   end;

end Macro11.Driver;
