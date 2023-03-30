with Ada.Sequential_IO;

package body Macro11.Bytecode is

   function Get_Allocation
     (This : Instance'Class;
      Address : Pdp11.Address_Type)
      return Allocation_Lists.Cursor;

   ------------
   -- Append --
   ------------

   procedure Append (This  : in out Instance'Class;
                     Value : Pdp11.Word_8)
   is
   begin
      if not Allocation_Lists.Has_Element (This.Current_Allocation) then
         declare
            Alloc : constant Allocation :=
                      Allocation'(Base => This.Current_Address,
                                  Bound => This.Current_Address,
                                  Bytes => <>);
         begin
            This.Allocated.Append (Alloc);
            This.Current_Allocation := This.Allocated.Last;
         end;
      end if;

      declare
         use type Pdp11.Address_Type;
         Alloc : Allocation renames
                   This.Allocated (This.Current_Allocation);
      begin
         Alloc.Bytes.Append (Value);
         Alloc.Bound := Alloc.Bound + 1;
         This.Current_Address := This.Current_Address + 1;
      end;

   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (This  : in out Instance'Class;
      Value : Pdp11.Word_16)
   is
      use type Pdp11.Word_16;
   begin
      This.Append (Pdp11.Word_8 (Value mod 256));
      This.Append (Pdp11.Word_8 (Value / 256));
   end Append;

   --------------------
   -- Get_Allocation --
   --------------------

   function Get_Allocation
     (This    : Instance'Class;
      Address : Pdp11.Address_Type)
      return Allocation_Lists.Cursor
   is
   begin
      for Position in This.Allocated.Iterate loop
         declare
            use type Pdp11.Address_Type;
            Alloc : Allocation renames This.Allocated (Position);
         begin
            if Alloc.Base <= Address
              and then Alloc.Bound > Address
            then
               return Position;
            end if;
         end;
      end loop;
      return Allocation_Lists.No_Element;
   end Get_Allocation;

   ----------------
   -- Get_Word_8 --
   ----------------

   function Get_Word_8
     (This    : Instance'Class;
      Address : Pdp11.Address_Type)
      return Pdp11.Word_8
   is
      use Allocation_Lists;
      use type Pdp11.Address_Type;
      Position : constant Cursor := This.Get_Allocation (Address);
      pragma Assert (Has_Element (Position));
      Alloc : Allocation renames This.Allocated (Position);
   begin
      return Alloc.Bytes (Natural (Address - Alloc.Base));
   end Get_Word_8;

   -----------------
   -- Get_Word_16 --
   -----------------

   function Get_Word_16
     (This    : Instance'Class;
      Address : Pdp11.Address_Type)
      return Pdp11.Word_16
   is
      use type Pdp11.Address_Type;
      use type Pdp11.Word_16;
   begin
      return Pdp11.Word_16 (This.Get_Word_8 (Address))
        + Pdp11.Word_16 (This.Get_Word_8 (Address + 1)) * 256;
   end Get_Word_16;

   ------------------
   -- Is_Allocated --
   ------------------

   function Is_Allocated
     (This    : Instance'Class;
      Address : Pdp11.Address_Type)
      return Boolean
   is
   begin
      return Allocation_Lists.Has_Element (This.Get_Allocation (Address));
   end Is_Allocated;

   -------------------------
   -- Set_Current_Address --
   -------------------------

   procedure Set_Current_Address
     (This    : in out Instance'Class;
      Address : Pdp11.Address_Type)
   is
      use type Pdp11.Address_Type;
   begin
      This.Current_Address := Address;
      This.Current_Allocation := Allocation_Lists.No_Element;

      for Position in This.Allocated.Iterate loop
         declare
            Alloc : constant Allocation := Allocation_Lists.Element (Position);
         begin
            if Alloc.Bound = Address then
               This.Current_Allocation := Position;
               exit;
            end if;
         end;
      end loop;
   end Set_Current_Address;

   -----------
   -- Write --
   -----------

   procedure Write
     (This : Instance'Class;
      Path : String)
   is
      use Pdp11;
      package Word_8_IO is
        new Ada.Sequential_IO (Word_8);
      use Word_8_IO;
      File : File_Type;

      procedure Put (B : Word_8);
      procedure Put (A : Address_Type);

      ---------
      -- Put --
      ---------

      procedure Put (B : Word_8) is
      begin
         Write (File, B);
      end Put;

      ---------
      -- Put --
      ---------

      procedure Put (A : Address_Type)
      is
      begin
         Put (Word_8 (A mod 256));
         Put (Word_8 (A / 256));
      end Put;

   begin
      Create (File, Out_File, Path);
      for Alloc of This.Allocated loop
         if False then
            Put (Alloc.Base);
            Put (Alloc.Bound);
         end if;
         for B of Alloc.Bytes loop
            Put (B);
         end loop;
      end loop;
      Close (File);
   end Write;

end Macro11.Bytecode;
