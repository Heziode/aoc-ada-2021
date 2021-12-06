with Ada.Execution_Time,
     Ada.Exceptions,
     Ada.Long_Long_Integer_Text_IO,
     Ada.Real_Time,
     Ada.Text_IO;

with Utils;

procedure Main is
   use Ada.Execution_Time,
       Ada.Real_Time,
       Ada.Text_IO;
   use Utils;

   type Lanternfish_Life_Span is range -1 .. 8;
   subtype Long_Long_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'Last;
   type Lanternfishes_School is array (Lanternfish_Life_Span) of Long_Long_Natural;

   package Lanternfish_Life_Span_IO is new Ada.Text_IO.Integer_IO (Lanternfish_Life_Span);
   use Lanternfish_Life_Span_IO;

   File                 : File_Type;
   Start_Time, End_Time : CPU_Time;
   Execution_Duration   : Time_Span;
   Lanternfishes        : Lanternfishes_School := (others => Long_Long_Natural'First);
   Nb_Lanternfishes     : Long_Long_Natural    := Long_Long_Natural'First;
begin
   Get_File (File);

   if End_Of_File (File) then
      raise Program_Error with "Empty file";
   end if;

   --  Get all Lanternfishes
   while not End_Of_File (File) loop
      declare
         Line                : constant String           := Get_Line (File);
         First               : Positive                  := Line'First;
         Last                : Positive                  := Line'First;
         Last_Index          : Positive                  := Line'First;
         Value               : Lanternfish_Life_Span;
      begin
         while Last <= Line'Last loop
            if Line (Last) not in '0' .. '9' then
               if Line (First .. Last - 1) /= "" then
                  Get (Line (First .. Last - 1), Value, Last_Index);
                  Lanternfishes (Value) := Lanternfishes (Value) + 1;
               end if;
               First               := Last + 1;
               Last                := First;
            elsif Last = Line'Last then
               Get (Line (First .. Line'Last), Value, Last_Index);
               Lanternfishes (Value) := Lanternfishes (Value) + 1;
               Last := Last + 1;
            else
               Last := Last + 1;
            end if;
         end loop;
      end;
   end loop;

   Start_Time := Ada.Execution_Time.Clock;
   Solve_Puzzle : declare
   begin
      for Day in 1 .. 80 loop
         for Index in 0 .. Lanternfishes_School'Last loop
            Lanternfishes (Index - 1) := Lanternfishes (Index);
         end loop;

         Lanternfishes (Lanternfishes_School'Last)  := Lanternfishes (Lanternfishes_School'First);
         Lanternfishes (6)                          := Lanternfishes (Lanternfishes_School'First) + Lanternfishes (6);
         Lanternfishes (Lanternfishes_School'First) := Long_Long_Natural'First;
      end loop;

      for Nb of Lanternfishes loop
         Nb_Lanternfishes := Nb_Lanternfishes + Nb;
      end loop;
   end Solve_Puzzle;

   End_Time           := Ada.Execution_Time.Clock;
   Execution_Duration := End_Time - Start_Time;

   Put ("Result: ");
   Ada.Long_Long_Integer_Text_IO.Put (Nb_Lanternfishes, Width => 0);
   New_Line;

   Put_Line ("(Took " & Duration'Image (To_Duration (Execution_Duration) * 1_000_000) & "µs)");

   Close_If_Open (File);

exception
   when Occur : others =>
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (Occur));

      Close_If_Open (File);
end Main;
