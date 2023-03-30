with Ada.Directories;

package body Macro11.Files is

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name (This : Instance'Class) return String is
   begin
      return Ada.Directories.Base_Name
        (Macro11.Names."-" (This.Path));
   end Base_Name;

   ----------
   -- File --
   ----------

   function File (Path : String) return Reference is
   begin
      return new Instance'(Path => Macro11.Names."+" (Path));
   end File;

   ------------------
   -- Show_Context --
   ------------------

   function Show_Context
     (Context : File_Context)
      return String
   is
   begin
      if Context.File = null then
         return "unknown";
      else
         declare
            File_Name    : constant String := Context.File.Base_Name;
            Line_Image   : constant String := Context.Line'Image;
            Column_Image : constant String := Context.Column'Image;
         begin
            return File_Name & ":"
              & Line_Image (2 .. Line_Image'Last) & ":"
              & Column_Image (2 .. Column_Image'Last);
         end;
      end if;
   end Show_Context;

end Macro11.Files;
