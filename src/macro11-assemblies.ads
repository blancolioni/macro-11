private with Ada.Containers.Doubly_Linked_Lists;

private with Macro11.Environments;
private with Macro11.Syntax;
private with Macro11. Bytecode;

package Macro11.Assemblies is

   type Instance is tagged limited private;
   type Reference is access all Instance'Class;

   function Create return Reference;

   function Has_Errors (This : Instance'Class) return Boolean;

   procedure Scan_Errors
     (This    : Instance'Class;
      Process : not null access
        procedure (File_Name : String;
                   Line, Column : Positive;
                   Message : String));

   procedure Report_Errors
     (This    : Instance'Class);

   procedure Read (This : in out Instance'Class;
                   Path : String);

   procedure Compile (This : in out Instance'Class);

   procedure List (This : Instance'Class);

   type Output_Format is (Raw, A_Out);

   procedure Write (This   : Instance'Class;
                    Path   : String;
                    Format : Output_Format);

private

   package Syntax_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Macro11.Syntax.Reference, Macro11.Syntax."=");

   type Instance is tagged limited
      record
         Env    : Macro11.Environments.Reference;
         Files  : Syntax_Lists.List;
         Target : Macro11.Bytecode.Instance;
      end record;

end Macro11.Assemblies;
