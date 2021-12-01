with Ada.Containers.Vectors,
     Ada.Exceptions,
     Ada.Integer_Text_IO,
     Ada.Text_IO;

with Utils;

procedure Adventofcode_2021_12_01_1 is
   use Ada.Text_IO;
   use Utils;

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
   if Values.Is_Empty then
      Close_If_Open (File);
      Put_Line ("The input file is empty.");
      return;
   end if;

   --  Do the puzzle

   Solve_Puzzle : declare
      Curs       : Cursor  := Values.First;
      Last_Value : Natural := Element (Curs);
      Nb_Supp    : Natural := 0;
   begin
      Next (Curs);

      while Has_Element (Curs) loop
         if Element (Curs) > Last_Value then
            Nb_Supp := Nb_Supp + 1;
         end if;

         Last_Value := Element (Curs);
         Next (Curs);
      end loop;

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
end Adventofcode_2021_12_01_1;
