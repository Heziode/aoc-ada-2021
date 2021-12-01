with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded;

package body Utils is

   ---------------
   -- Open_File --
   ---------------

   procedure Open_File
     (File      : in out File_Type; Mode : File_Mode; Path : String;
      File_Form : String := DEFAULT_FILE_FORM_VALUE; Auto : Boolean := False)
   is
      use Ada.Directories;
   begin
      if Exists (Path) then
         Open (File, Mode, Path, File_Form);
      else
         if Auto then
            Create (File, Mode, Path, File_Form);
         else
            raise Ada.Directories.Name_Error;
         end if;
      end if;
   end Open_File;

   --------------
   -- Get_File --
   --------------

   procedure Get_File (File : in out File_Type) is
      use Ada.Command_Line, Ada.Directories, Ada.Strings.Unbounded;

      File_Path : Unbounded_String := Null_Unbounded_String;
   begin
      if Exists (Compose (Current_Directory, DEFAULT_FILENAME)) then
         File_Path := To_Unbounded_String (Compose (Current_Directory, DEFAULT_FILENAME));
      elsif Exists (Compose (Containing_Directory (Command_Name), DEFAULT_FILENAME)) then
         File_Path := To_Unbounded_String (Compose (Containing_Directory (Command_Name), DEFAULT_FILENAME));
      else
         --  No file found
         raise Ada.Directories.Name_Error with "There is no file in '" &
           Current_Directory & "' nor in '" & Containing_Directory (Command_Name) & "'";
      end if;

      --  Load file and parse content
      Open_File (File      => File,
                 Mode      => In_File,
                 Path      => To_String (File_Path));
   end Get_File;

   -------------------
   -- Close_If_Open --
   -------------------

   procedure Close_If_Open (File : in out File_Type) is
   begin
      if Is_Open (File) then
         Close (File);
      end if;
   end Close_If_Open;

end Utils;
