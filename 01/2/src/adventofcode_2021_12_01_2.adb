with Ada.Containers.Vectors,
     Ada.Exceptions,
     Ada.Integer_Text_IO,
     Ada.Text_IO;

with Utils;

procedure Adventofcode_2021_12_01_2 is
   use Ada.Text_IO;
   use Utils;
   use type Ada.Containers.Count_Type;

   package Integer_Vectors is new Ada.Containers.Vectors (Natural, Integer);
   use Integer_Vectors;

   File   : File_Type;
   Values : Vector := Empty_Vector;
begin
   Get_File (File);

   --  Get all values
   while not End_Of_File (File) loop
      declare
         Value : Natural;
      begin
         Ada.Integer_Text_IO.Get (File, Value);
         Values.Append (Value);
      end;
   end loop;

   --  Exit the program if there is no values
   if Values.Length < 3 then
      Close_If_Open (File);
      Put_Line ("The input file is empty or too small (at least 3 values are required).");
      return;
   end if;

   --  Do the puzzle

   Solve_Puzzle : declare
      type Modular_Index_Of_3 is mod 3;
      Curs                     : Cursor                    := Values.First;
      Last_Value               : Natural                   := Natural'Last;
      Nb_Supp                  : Natural                   := Natural'First;
      Temp_Window              : array (0 .. 2) of Natural := (others => Natural'First);
      Mod_Index                : Modular_Index_Of_3        := Modular_Index_Of_3'First;
   begin
      Temp_Window (0) := Element (Curs);
      Next (Curs);

      Temp_Window (0) := Temp_Window (0) + Element (Curs);
      Temp_Window (1) := Element (Curs);
      Next (Curs);

      Temp_Window (0) := Temp_Window (0) + Element (Curs);
      Temp_Window (1) := Temp_Window (1) + Element (Curs);
      Temp_Window (2) := Element (Curs);
      Next (Curs);

      while Has_Element (Curs) loop
         if Temp_Window (Natural (Mod_Index)) > Last_Value then
            Nb_Supp := Nb_Supp + 1;
         end if;
         Last_Value := Temp_Window (Natural (Mod_Index));
         Temp_Window (Natural (Mod_Index)) := 0;

         Temp_Window (0) := Temp_Window (0) + Element (Curs);
         Temp_Window (1) := Temp_Window (1) + Element (Curs);
         Temp_Window (2) := Temp_Window (2) + Element (Curs);

         Next (Curs);
         Mod_Index :=  Mod_Index + 1;
      end loop;

      if Temp_Window (Natural (Mod_Index)) > Last_Value then
            Nb_Supp := Nb_Supp + 1;
      end if;
      Put ("Result: ");
      Ada.Integer_Text_IO.Put (Item  => Nb_Supp,
                               Width => 0);
      New_Line;
   end Solve_Puzzle;

   Close_If_Open (File);

exception
   when Occur : others =>
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (Occur));

      Close_If_Open (File);
end Adventofcode_2021_12_01_2;
