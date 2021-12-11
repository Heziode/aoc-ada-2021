with Ada.Execution_Time,
     Ada.Integer_Text_IO,
     Ada.Real_Time,
     Ada.Text_IO;

with Utils;

procedure Main is
   use Ada.Execution_Time,
       Ada.Real_Time,
       Ada.Text_IO;
   use Utils;

   subtype Adjacent_Index is Integer range -1 .. 1;
   type Adjacent_Location is record
      Line   : Adjacent_Index;
      Column : Adjacent_Index;
   end record;

   type Adjacent is (Top, Top_Right, Right, Bottom_Right, Bottom, Bottom_Left, Left, Top_Left);
   type Adjacent_Array is array (Adjacent) of Adjacent_Location;

   type Octopuses_Energy_Level_Array is array (1 .. 10, 1 .. 10) of Integer;

   Adjacents : constant Adjacent_Array := (Top          => (-1, 0),
                                           Top_Right    => (-1, 1),
                                           Right        => (0, 1),
                                           Bottom_Right => (1, 1),
                                           Bottom       => (1, 0),
                                           Bottom_Left  => (1, -1),
                                           Left         => (0, -1),
                                           Top_Left     => (-1, -1));

   File                   : File_Type;
   Start_Time, End_Time   : CPU_Time;
   Execution_Duration     : Time_Span;
   Octopuses_Energy_Level : Octopuses_Energy_Level_Array;
   File_Is_Empty          : Boolean := True;
   Result                 : Natural := Natural'First;
begin
   Get_File (File);

   --  Get all values
   declare
      Current_Line,
      Current_Column : Positive := Positive'First;
   begin
      while not End_Of_File (File) loop
         declare
            Str  : constant String := Get_Line (File);
            Last : Positive;
         begin
            for Char of Str loop
               Ada.Integer_Text_IO.Get (Char & "", Octopuses_Energy_Level (Current_Line, Current_Column), Last);
               Current_Column := Current_Column + 1;
               File_Is_Empty := False;
            end loop;
         end;
         Current_Column := Positive'First;
         Current_Line   := Current_Line + 1;
      end loop;
   end;

   --  Exit the program if there is no values
   if File_Is_Empty then
      Close_If_Open (File);
      Put_Line ("The input file is empty.");
      return;
   end if;

   --  Do the puzzle
   Start_Time := Ada.Execution_Time.Clock;
   Solve_Puzzle : declare
      --  Do a flash for octopuses having a light level greater than 10
      procedure Do_Flash (Line, Column : Positive);

      --------------
      -- Do_Flash --
      --------------

      procedure Do_Flash (Line, Column : Positive) is
         Current_Line,
         Current_Column : Positive := Positive'First;
      begin
         Octopuses_Energy_Level (Line, Column) := -1;
         for Adjacent : Adjacent_Location of Adjacents loop
            if not (Line - Adjacent.Line in Octopuses_Energy_Level_Array'Range (1)) or
              not (Column - Adjacent.Column in Octopuses_Energy_Level_Array'Range (2))
            then
               goto Continue;
            end if;
            Current_Line   := Line   - Adjacent.Line;
            Current_Column := Column - Adjacent.Column;

            if Octopuses_Energy_Level (Current_Line, Current_Column) > -1 then
               Octopuses_Energy_Level (Current_Line, Current_Column) :=
                 Octopuses_Energy_Level (Current_Line, Current_Column) + 1;

               if Octopuses_Energy_Level (Current_Line, Current_Column) >= 10 then
                  Do_Flash (Current_Line, Current_Column);
               end if;
            end if;

            <<Continue>>
         end loop;
      end Do_Flash;

      All_Octopuses_Flash : Boolean := False;
   begin
      while not All_Octopuses_Flash loop
         Result              := Result + 1;
         All_Octopuses_Flash := True;

         --  First, increase light level of every octopuses by 1
         for Line in Octopuses_Energy_Level'Range (1) loop
            for Column in Octopuses_Energy_Level'Range (2) loop
               Octopuses_Energy_Level (Line, Column) := Octopuses_Energy_Level (Line, Column) + 1;
            end loop;
         end loop;

         --  Now, do a flash for every octopsuses that have a light level of 10
         for Line in Octopuses_Energy_Level'Range (1) loop
            for Column in Octopuses_Energy_Level'Range (2) loop
               if Octopuses_Energy_Level (Line, Column) = 10 then
                  Do_Flash (Line, Column);
               end if;
            end loop;
         end loop;

         --  Finally, reset to 0 every octopuses who flashed.
         --  The special value -1 is used to not process multiple flash of one octopus in a step
         for Line in Octopuses_Energy_Level'Range (1) loop
            for Column in Octopuses_Energy_Level'Range (2) loop
               if Octopuses_Energy_Level (Line, Column) = -1 then
                  Octopuses_Energy_Level (Line, Column) := 0;
               else
                  All_Octopuses_Flash := False;
               end if;
            end loop;
         end loop;
      end loop;
   end Solve_Puzzle;

   End_Time           := Ada.Execution_Time.Clock;
   Execution_Duration := End_Time - Start_Time;

   Put ("Result: ");
   Ada.Integer_Text_IO.Put (Item  => Result,
                            Width => 0);
   New_Line;

   Put_Line ("(Took " & Duration'Image (To_Duration (Execution_Duration) * 1_000_000) & "µs)");

exception
   when others =>
      Close_If_Open (File);
      raise;
end Main;
