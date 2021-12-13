with Ada.Containers.Hashed_Maps,
     Ada.Containers.Vectors,
     Ada.Execution_Time,
     Ada.Integer_Text_IO,
     Ada.Real_Time,
     Ada.Strings.Fixed,
     Ada.Text_IO;

with Utils;

procedure Main is
   use Ada.Execution_Time,
       Ada.Real_Time,
       Ada.Text_IO;
   use Utils;

   type Coordinate_2D is record
      Line   : Natural;
      Column : Natural;
   end record;

   type Folding_Kind is (K_Line, K_Column);

   type Folding_T is record
      Kind  : Folding_Kind;
      Value : Natural;
   end record;

   package Folding_Vectors is new Ada.Containers.Vectors (Natural, Folding_T);

   --  This Hash function transform a 2 dimensional location to an unique ID using Cantor pairing enumeration.
   --  @param Coord A 2 dimensional location
   --  @returns Return the corresponding hash
   --  @link https://en.wikipedia.org/wiki/Pairing_function#Cantor_pairing_function
   function Hash (Coord : Coordinate_2D) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type (((Coord.Line + Coord.Column) * (Coord.Line + Coord.Column + 1) / 2) + Coord.Column));

   function Equivalent_Keys (Left, Right : Coordinate_2D) return Boolean is
     (Left.Line = Right.Line and Left.Column = Right.Column);

   package Dots_Maps is new Ada.Containers.Hashed_Maps (Key_Type        => Coordinate_2D,
                                                        Element_Type    => Boolean,
                                                        Hash            => Hash,
                                                        Equivalent_Keys => Equivalent_Keys,
                                                        "="             => "=");

   File                   : File_Type;
   Start_Time, End_Time   : CPU_Time;
   Execution_Duration     : Time_Span;
   File_Is_Empty          : Boolean  := True;
   Max_Lines, Max_Columns : Positive := Positive'First;
   Folding_Instructions   : Folding_Vectors.Vector := Folding_Vectors.Empty_Vector;
   Dots_Map               : Dots_Maps.Map := Dots_Maps.Empty_Map;
begin
   Get_File (File);

   --  Get all values
   begin
      while not End_Of_File (File) loop
         declare
            Str : constant String := Get_Line (File);
         begin
            if Str'Length = 0 then
               goto Continue_Next_Line;
            end if;

            if Str (Str'First) = 'f' then
               --  "fold along" instruction
               declare
                  use Ada.Integer_Text_IO;

                  Separator_Index : constant Integer :=
                    Ada.Strings.Fixed.Index (Source => Str (1 .. Str'Last), Pattern => "=");
                  Kind            : constant String  := Str (Separator_Index - 1 .. Separator_Index - 1);
                  Value_Str       : constant String  := Str (Separator_Index + 1 .. Str'Last);
                  Last_Index      : Positive := Positive'First;
                  Value           : Natural;
               begin
                  Get (Value_Str, Value, Last_Index);

                  Folding_Instructions.Append (((if Kind = "x" then K_Column else K_Line), Value));
               end;
            else
               --  Point coordinate
               declare
                  use Ada.Integer_Text_IO;

                  Separator_Index : constant Integer :=
                    Ada.Strings.Fixed.Index (Source => Str (1 .. Str'Last), Pattern => ",");
                  Column_Str      : constant String  := Str (1 .. Separator_Index - 1);
                  Line_Str        : constant String  := Str (Separator_Index + 1 .. Str'Last);
                  Column, Line    : Natural;
                  Last_Index      : Positive := Positive'First;
               begin
                  Get (Column_Str, Column, Last_Index);
                  Get (Line_Str, Line, Last_Index);

                  Dots_Map.Include ((Column => Column, Line => Line), True);

                  if Max_Lines < Line then
                     Max_Lines := Line;
                  end if;

                  if Max_Columns < Column then
                     Max_Columns := Column;
                  end if;
               end;
            end if;
         end;
         File_Is_Empty  := False;

         <<Continue_Next_Line>>
      end loop;

      --  Exit the program if there is no values
      if File_Is_Empty then
         Close_If_Open (File);
         Put_Line ("The input file is empty.");
         return;
      end if;
   end;

   --  Do the puzzle
   Start_Time := Ada.Execution_Time.Clock;
   Solve_Puzzle : declare
      use Dots_Maps;
      Coord : Coordinate_2D;
      Dots  : Map := Empty_Map;
   begin
      for Fold_Along : Folding_T of Folding_Instructions loop
         case Fold_Along.Kind is
            when K_Column =>
               Max_Columns := Fold_Along.Value - 1;
               for Curs in Dots_Map.Iterate loop
                  Coord := Key  (Curs);
                  if Coord.Column < Fold_Along.Value then
                     Dots.Include (Coord, True);
                  else
                     if Fold_Along.Value - (Coord.Column - Fold_Along.Value) >= 0 then
                        Dots.Include ((Column => Fold_Along.Value - (Coord.Column - Fold_Along.Value),
                                       Line   => Coord.Line
                                      ), True);
                     end if;
                  end if;
               end loop;
            when K_Line =>
               Max_Lines := Fold_Along.Value - 1;
               for Curs in Dots_Map.Iterate loop
                  Coord := Key  (Curs);
                  if Coord.Line < Fold_Along.Value then
                     Dots.Include (Coord, True);
                  else
                     if Fold_Along.Value - (Coord.Line - Fold_Along.Value) >= 0 then
                        Dots.Include ((Column => Coord.Column,
                                       Line   => Fold_Along.Value - (Coord.Line - Fold_Along.Value)
                                      ), True);
                     end if;
                  end if;
               end loop;
         end case;
         Dots_Map := Dots;

      end loop;
   end Solve_Puzzle;

   End_Time           := Ada.Execution_Time.Clock;
   Execution_Duration := End_Time - Start_Time;

   Put_Line ("Result: ");
   for Line in 0 .. Max_Lines loop
      for Column in 0 .. Max_Columns loop
         if Dots_Map.Contains ((Column => Column, Line => Line)) then
            Put ("#");
         else
            Put (" ");
         end if;
      end loop;
      New_Line;
   end loop;
   New_Line;

   Put_Line ("(Took " & Duration'Image (To_Duration (Execution_Duration) * 1_000_000) & "µs)");

exception
   when others =>
      Close_If_Open (File);
      raise;
end Main;
