with Ada.Text_IO;

use Ada.Text_IO;

package Utils is

   --  The parameter "wcem=8" indicates that we want to open a file in
   --  UTF-8 format (which can therefore contain accents)
   DEFAULT_FILE_FORM_VALUE : constant String  := "wcem=8";

   --  Default file name
   DEFAULT_FILENAME        : constant String  := "input.txt";

   --  Open a file. By default, if file does not exist, it create it.
   --  @param File The file container
   --  @param Mode Mode to open the file
   --  @param Path Location of the file
   --  @param File_Form "Form" to pass to the file
   --  @param Auto True if auto create the file if it does not exist, false otherwise
   procedure Open_File (File      : in out File_Type;
                        Mode      : File_Mode;
                        Path      : String;
                        File_Form : String  := DEFAULT_FILE_FORM_VALUE;
                        Auto      : Boolean := False);

   --  Open the file at the default location.
   --  Raise an error if the file cannot be found / open.
   --  @param File The openned file will be inside this param.
   procedure Get_File (File : in out File_Type);

   --  Check if the file is opened before attempt to close it (to prevent raise of error).
   --  @param File The file that shall be closed.
   procedure Close_If_Open (File : in out File_Type);

end Utils;
