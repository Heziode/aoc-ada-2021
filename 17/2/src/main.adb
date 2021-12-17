with Ada.Execution_Time,
     Ada.Integer_Text_IO,
     Ada.Real_Time,
     Ada.Strings.Fixed,
     Ada.Text_IO;

with Utils,
     Coordinates_2D;

procedure Main is
   use Ada.Execution_Time,
       Ada.Real_Time,
       Ada.Text_IO;
   use Utils;

   package My_Coordinates is new Coordinates_2D (Integer);
   use My_Coordinates;

   File                 : File_Type;
   Start_Time, End_Time : CPU_Time;
   Execution_Duration   : Time_Span;
   Result               : Natural := Natural'First;

   X_Min, X_Max,
   Y_Min, Y_Max         : Integer;
begin
   Get_File (File);

   --  Get all values
   declare
      procedure Split_Range (Str : String; Lower, Upper : out Integer);

      procedure Split_Range (Str : String; Lower, Upper : out Integer) is
         use Ada.Integer_Text_IO;

         Pattern         : constant String  := "..";
         Separator_Index : constant Natural :=
           Ada.Strings.Fixed.Index (Source => Str, Pattern => Pattern);
         Left_Str        : constant String  := Str (Str'First .. Separator_Index - 1);
         Right_Str       : constant String  := Str (Separator_Index + Pattern'Length .. Str'Last);

         Last : Positive;
      begin
         Get (Left_Str, Lower, Last);
         Get (Right_Str, Upper, Last);
      end Split_Range;

      Str   : constant String := Get_Line (File);
      Pattern         : constant String  := ",";
      Separator_Index : constant Natural :=
        Ada.Strings.Fixed.Index (Source => Str (1 .. Str'Last), Pattern => Pattern);
      Left_Str        : constant String  := Str (16 .. Separator_Index - 1);
      Right_Str       : constant String  := Str (Separator_Index + Pattern'Length + 3 .. Str'Last);

   begin
      Split_Range (Left_Str, X_Min, X_Max);
      Split_Range (Right_Str, Y_Min, Y_Max);
   end;

   --  Do the puzzle
   Start_Time := Ada.Execution_Time.Clock;
   Solve_Puzzle : declare
      subtype X_Velocity_Range is Integer range X_Min .. X_Max;
      subtype Y_Velocity_Range is Integer range Y_Min .. Y_Max;

      subtype Sign_Values is Integer range -1 .. 1;

      --  Returns either a positive or negative +/- 1, indicating the sign of a number passed in argument.
      --  If the number passed is 0, it will return 0.
      function Sign (Value : Integer) return Sign_Values;

      function Sign (Value : Integer) return Sign_Values is

      begin
         if Value < 0 then
            return -1;
         elsif Value > 0 then
            return 1;
         end if;
         return 0;
      end Sign;

   begin
      for Y_Velocity in Y_Min .. Integer'Max (0, Integer'Max (abs Y_Min, abs Y_Max)) loop
         for X_Velocity in Integer'Min (0, X_Min) .. Integer'Max (0, X_Max) loop
            Simulate_Probe_Launch : declare
               Highest_Y     : Integer       := Integer'First;
               Current_Point : Coordinate_2D := (Line => 0,          Column => 0);
               Velocity      : Coordinate_2D := (Line => Y_Velocity, Column => X_Velocity);
            begin
               Simulation : loop
                  Current_Point := Current_Point + Velocity;
                  Velocity      := Velocity - (Line => 1, Column => Sign (Velocity.Column));
                  Highest_Y     := Integer'Max (Highest_Y, Current_Point.Line);

                  if Current_Point.Line in Y_Velocity_Range and Current_Point.Column in X_Velocity_Range then
                     Result := Result + 1;
                     exit Simulation;
                  elsif Current_Point.Line < Y_Velocity_Range'First
                    or Current_Point.Column > X_Velocity_Range'Last
                  then
                     exit Simulation;
                  end if;
               end loop Simulation;
            end Simulate_Probe_Launch;
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
