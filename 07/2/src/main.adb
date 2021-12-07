with Ada.Containers.Vectors,
     Ada.Execution_Time,
     Ada.Exceptions,
     Ada.Integer_Text_IO,
     Ada.Real_Time,
     Ada.Text_IO;

with Utils;

procedure Main is
   use Ada.Execution_Time,
       Ada.Real_Time,
       Ada.Text_IO;
   use Utils;

   package Natural_Vectors is new Ada.Containers.Vectors (Natural, Natural);
   use Natural_Vectors;

   File                 : File_Type;
   Start_Time, End_Time : CPU_Time;
   Execution_Duration   : Time_Span;
   Crabs                : Vector;
   Min_Dist, Min        : Natural := Natural'Last;
   Max                  : Natural := Natural'First;
begin
   Get_File (File);

   if End_Of_File (File) then
      raise Program_Error with "Empty file";
   end if;

   --  Get all Lanternfishes
   while not End_Of_File (File) loop
      declare
         use Ada.Integer_Text_IO;
         Line                : constant String           := Get_Line (File);
         First               : Positive                  := Line'First;
         Last                : Positive                  := Line'First;
         Last_Index          : Positive                  := Line'First;
         Value               : Natural;
      begin
         while Last <= Line'Last loop
            if Line (Last) not in '0' .. '9' then
               if Line (First .. Last - 1) /= "" then
                  Get (Line (First .. Last - 1), Value, Last_Index);
                  Crabs.Append (Value);

                  if Value < Min then
                     Min := Value;
                  end if;

                  if Value > Max then
                     Max := Value;
                  end if;
               end if;
               First               := Last + 1;
               Last                := First;
            elsif Last = Line'Last then
               Get (Line (First .. Line'Last), Value, Last_Index);
               Crabs.Append (Value);

               if Value < Min then
                  Value := Min;
               end if;

               if Value > Max then
                  Value := Max;
               end if;

               Last := Last + 1;
            else
               Last := Last + 1;
            end if;
         end loop;
      end;
   end loop;

   Start_Time := Ada.Execution_Time.Clock;
   Solve_Puzzle : declare
      Sum : Natural := 0;
   begin
      for Index in Min .. Max + 1 loop
         for Crab of Crabs loop
            Sum := Sum + abs (Crab - Index) * ((abs (Crab - Index)) + 1) / 2;
         end loop;

         if Sum < Min_Dist then
            Min_Dist := Sum;
         end if;
         Sum := 0;
      end loop;
   end Solve_Puzzle;

   End_Time           := Ada.Execution_Time.Clock;
   Execution_Duration := End_Time - Start_Time;

   Put ("Result: ");
   Ada.Integer_Text_IO.Put (Min_Dist, Width => 0);
   New_Line;

   Put_Line ("(Took " & Duration'Image (To_Duration (Execution_Duration) * 1_000_000) & "µs)");

   Close_If_Open (File);

exception
   when Occur : others =>
      Close_If_Open (File);
      raise;
end Main;
