with Ada.Containers.Vectors,
     Ada.Execution_Time,
     Ada.Exceptions,
     Ada.Integer_Text_IO,
     Ada.Real_Time,
     Ada.Text_IO;

with Utils;

procedure Main is
   use Ada.Containers,
       Ada.Execution_Time,
       Ada.Real_Time,
       Ada.Text_IO;
   use Utils;

   type Coordinate_2D is record
      Line   : Natural;
      Column : Natural;
   end record;

   type Segment is record
      Start_Line : Coordinate_2D;
      End_Line   : Coordinate_2D;
   end record;

   package Segment_Vectors is new Ada.Containers.Vectors (Natural, Segment);
   use Segment_Vectors;

   File                   : File_Type;
   Start_Time, End_Time   : CPU_Time;
   Execution_Duration     : Time_Span;
   Segments               : Vector   := Empty_Vector;
   Max_Lines, Max_Columns : Positive := Positive'First;
   Nb_Dangerous_Cells     : Natural  := Natural'First;
begin
   Get_File (File);

   if End_Of_File (File) then
      raise Program_Error with "Empty file";
   end if;

   --  Get all segments
   while not End_Of_File (File) loop
      declare
         use Ada.Integer_Text_IO;

         Line                : constant String           := Get_Line (File);
         First               : Positive                  := Line'First;
         Last                : Positive                  := Line'First;
         Last_Index          : Positive                  := Line'First;
         Values              : array (1 .. 4) of Natural := (others => 0);
         Current_Value_Index : Positive                  := Positive'First;
      begin
         while Last <= Line'Last loop
            if Line (Last) not in '0' .. '9' then
               if Line (First .. Last - 1) /= "" then
                  Get (Line (First .. Last - 1), Values (Current_Value_Index), Last_Index);
                  Current_Value_Index := Current_Value_Index + 1;
               end if;
               First := Last + 1;
               Last  := First;
            elsif Last = Line'Last then
               Get (Line (First .. Line'Last), Values (Current_Value_Index), Last_Index);
               Last := Last + 1;
            else
               Last := Last + 1;
            end if;
         end loop;

         if Values (1) > Max_Lines then
            Max_Lines := Values (1);
         elsif Values (3) > Max_Lines then
            Max_Lines := Values (3);
         end if;

         if Values (2) > Max_Columns then
            Max_Columns := Values (2);
         elsif Values (4) > Max_Columns then
            Max_Columns := Values (4);
         end if;

         Segments.Append ((
                          Start_Line => (Line => Values (1), Column => Values (2)),
                          End_Line   => (Line => Values (3), Column => Values (4))
                         ));
      end;
   end loop;

   Start_Time := Ada.Execution_Time.Clock;
   Solve_Puzzle : declare
      Grid     : array (0 .. Max_Lines, 0 .. Max_Columns) of Natural := (others => (others => Natural'First));
      Min, Max : Natural;
   begin
      for Seg : Segment of Segments loop
         if Seg.Start_Line.Line = Seg.End_Line.Line then
            --  Same line
            if Seg.Start_Line.Column > Seg.End_Line.Column then
               Min := Seg.End_Line.Column;
               Max := Seg.Start_Line.Column;
            else
               Min := Seg.Start_Line.Column;
               Max := Seg.End_Line.Column;
            end if;

            for Index in Min .. Max loop
               Grid (Seg.Start_Line.Line, Index) := Grid (Seg.Start_Line.Line, Index) + 1;
               if Grid (Seg.Start_Line.Line, Index) = 2 then
                  Nb_Dangerous_Cells := Nb_Dangerous_Cells + 1;
               end if;
            end loop;
         elsif Seg.Start_Line.Column = Seg.End_Line.Column then
            --  Same Column
            if Seg.Start_Line.Line > Seg.End_Line.Line then
               Min := Seg.End_Line.Line;
               Max := Seg.Start_Line.Line;
            else
               Min := Seg.Start_Line.Line;
               Max := Seg.End_Line.Line;
            end if;

            for Index in Min .. Max loop
               Grid (Index, Seg.Start_Line.Column) := Grid (Index, Seg.Start_Line.Column) + 1;
               if Grid (Index, Seg.Start_Line.Column) = 2 then
                  Nb_Dangerous_Cells := Nb_Dangerous_Cells + 1;
               end if;
            end loop;
         else
            --  Diagonal
            declare
               Slope                : constant Integer :=
                 (Seg.Start_Line.Line - Seg.End_Line.Line) / (Seg.Start_Line.Column - Seg.End_Line.Column);
               Coordinate_Increment : Integer          := Natural'First;
               Min_Line, Max_Line   : Natural          := Natural'First;
            begin
               --  Only take in account 45/-45 degree segments
               if abs Slope = 1 then
                  if Seg.Start_Line.Line < Seg.End_Line.Line then
                     Coordinate_Increment := Seg.Start_Line.Column;
                     Min_Line             := Seg.Start_Line.Line;
                     Max_Line             := Seg.End_Line.Line;
                  else
                     Coordinate_Increment := Seg.End_Line.Column;
                     Min_Line             := Seg.End_Line.Line;
                     Max_Line             := Seg.Start_Line.Line;
                  end if;

                  for Index in Min_Line .. Max_Line loop
                     Grid (Index, Coordinate_Increment) := Grid (Index, Coordinate_Increment) + 1;
                     if Grid (Index, Coordinate_Increment) = 2 then
                        Nb_Dangerous_Cells := Nb_Dangerous_Cells + 1;
                     end if;
                     Coordinate_Increment := Coordinate_Increment + Slope;
                  end loop;
               end if;
            end;
         end if;
      end loop;
   end Solve_Puzzle;

   End_Time           := Ada.Execution_Time.Clock;
   Execution_Duration := End_Time - Start_Time;

   Put ("Result: ");
   Ada.Integer_Text_IO.Put (Nb_Dangerous_Cells, Width => 0);
   New_Line;

   Put_Line ("(Took " & Duration'Image (To_Duration (Execution_Duration) * 1_000_000) & "µs)");

   Close_If_Open (File);

exception
   when Occur : others =>
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (Occur));

      Close_If_Open (File);
end Main;
