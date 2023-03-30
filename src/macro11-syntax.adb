with Ada.Exceptions;
with Ada.Text_IO;

with Macro11.Logging;

package body Macro11.Syntax is

   ---------------
   -- Add_Error --
   ---------------

   procedure Add_Error
     (This    : in out Instance'Class;
      Message : String)
   is
   begin
      This.Errors.Append (Message);
   end Add_Error;

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (This   : in out Instance'Class;
      Offset : in out Pdp11.Address_Type)
   is
   begin
      This.Allocate_Instance (Offset);
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            This.Show_Context
            & ": " & This.To_String
            & ": allocate: internal error: "
            & Ada.Exceptions.Exception_Message (E));
         raise;
   end Allocate;

   -----------------------
   -- Allocate_Instance --
   -----------------------

   procedure Allocate_Instance
     (This   : in out Instance;
      Offset : in out Pdp11.Address_Type)
   is
   begin
      if not This.Has_Property (No_Allocation) then
         for Child of Dispatch (This).Children loop
            Child.Allocate (Offset);
         end loop;
      end if;
   end Allocate_Instance;

   -----------
   -- Check --
   -----------

   procedure Check
     (This : in out Instance'Class;
      Env  : not null access Macro11.Environments.Instance'Class)
   is
   begin
      This.Env := Env;
      This.Check_Instance;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            This.Show_Context
            & ": " & This.To_String
            & ": check: internal error: "
            & Ada.Exceptions.Exception_Message (E));
         raise;
   end Check;

   --------------------
   -- Clear_Property --
   --------------------

   procedure Clear_Property
     (This     : in out Instance'Class;
      Property : Property_Type)
   is
   begin
      This.Properties.Delete (String (Property));
   end Clear_Property;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry (This : Instance) return Macro11.Entries.Reference is
   begin
      return (raise Constraint_Error with
                "no entry: " & Dispatch (This).Show_Context
              & ": " & Dispatch (This).To_String);
   end Get_Entry;

   ---------------
   -- Has_Entry --
   ---------------

   function Has_Entry (This : Instance) return Boolean is
   begin
      return False;
   end Has_Entry;

   ----------------
   -- Has_Errors --
   ----------------

   function Has_Errors (This : Instance'Class) return Boolean is
   begin
      return not This.Errors.Is_Empty
        or else (for some Child of This.Children => Child.Has_Errors);
   end Has_Errors;

   ----------
   -- List --
   ----------

   procedure List
     (This   : Instance'Class;
      Target : Macro11.Bytecode.Instance'Class;
      Writer : in out Text_Writer'Class)
   is
   begin
      This.List_Instance (Target, Writer);
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            This.Show_Context
            & ": " & This.To_String
            & ": list: internal error: "
            & Ada.Exceptions.Exception_Message (E));
         raise;
   end List;

   -------------------
   -- List_Instance --
   -------------------

   procedure List_Instance
     (This   : Instance;
      Target : Macro11.Bytecode.Instance'Class;
      Writer : in out Text_Writer'Class)
   is
   begin
      for Child of Dispatch (This).Children loop
         Child.List (Target, Writer);
      end loop;
   end List_Instance;

   ---------
   -- Log --
   ---------

   procedure Log
     (This    : Instance'Class;
      Message : String)
   is
   begin
      Macro11.Logging.Log
        (This.Show_Context & ": " & Message);
   end Log;

   -----------------
   -- Scan_Errors --
   -----------------

   procedure Scan_Errors
     (This    : Instance'Class;
      Process : not null access procedure
        (File_Name : String; Line, Column : Positive; Message : String))
   is
      use type Macro11.Files.Reference;
   begin
      for Message of This.Errors loop
         Process (File_Name =>
                    (if This.Context.File = null
                     then "unknown"
                     else This.Context.File.Base_Name),
                  Line      => This.Context.Line,
                  Column    => This.Context.Column,
                  Message   => Message);
      end loop;
      for Child of This.Children loop
         Child.Scan_Errors (Process);
      end loop;
   end Scan_Errors;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context
     (File    : Macro11.Files.Reference;
      Line    : Positive;
      Column  : Positive)
   is
   begin
      Current_Context := (File, Line, Column);
   end Set_Context;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context
     (Context : Macro11.Files.File_Context)
   is
   begin
      Current_Context := Context;
   end Set_Context;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (This     : in out Instance'Class;
      Property : Property_Type)
   is
   begin
      This.Properties.Include (String (Property));
   end Set_Property;

   ------------------
   -- Show_Context --
   ------------------

   function Show_Context
     (This : Instance'Class)
      return String
   is
      use type Macro11.Files.Reference;
   begin
      if This.Context.File = null then
         return "unknown";
      else
         declare
            use Macro11.Files;
            Context : constant File_Context := This.Context;
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

   ---------------
   -- To_String --
   ---------------

   function To_String (This : Instance) return String is

      Refs : constant Reference_Array := Dispatch (This).Children;

      function Children_To_String
        (Start : Positive)
         return String;

      ------------------------
      -- Children_To_String --
      ------------------------

      function Children_To_String
        (Start : Positive)
         return String
      is
      begin
         return (if Start <= Refs'Last
                 then (if Start > Refs'First then "," else "")
                 & Refs (Start).To_String
                 & Children_To_String (Start + 1)
                 else "");
      end Children_To_String;

   begin
      return Dispatch (This).Class_Name
        & "<" & Children_To_String (Refs'First)
        & ">";
   end To_String;

   ---------------
   -- Translate --
   ---------------

   procedure Translate
     (This    : in out Instance'Class;
      Context : in out Translation_Context;
      Target  : in out Macro11.Bytecode.Instance'Class)
   is
   begin
      This.Translate_Instance (Context, Target);
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            This.Show_Context
            & ": " & This.To_String
            & ": translate: internal error: "
            & Ada.Exceptions.Exception_Message (E));
         raise;
   end Translate;

   ------------------------
   -- Translate_Instance --
   ------------------------

   procedure Translate_Instance
     (This    : in out Instance;
      Context : in out Translation_Context;
      Target  : in out Macro11.Bytecode.Instance'Class)
   is
   begin
      for Child of Dispatch (This).Children loop
         Child.Translate (Context, Target);
      end loop;
   end Translate_Instance;

end Macro11.Syntax;
