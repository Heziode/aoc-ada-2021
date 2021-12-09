with Ada.Containers.Vectors,
     Ada.Containers.Bounded_Hashed_Maps,
     Ada.Exceptions,
     Ada.Integer_Text_IO,
     Ada.Text_IO;

with Utils;

procedure Main is
   use Ada.Text_IO;
   use Utils;

   package Integer_Vectors is new Ada.Containers.Vectors (Natural, Integer);
   use Integer_Vectors;

   subtype Hegihtmap_Values is Natural range 0 .. 10;
   subtype Hegiht is Hegihtmap_Values range 0 .. 9;

   type Cave_Heightmap_Array is array (Natural range <>, Natural range <>) of Hegihtmap_Values;

   File         : File_Type;
   Values       : Vector  := Empty_Vector;
   Array_Width,
   Array_Height : Natural := Natural'First;
begin
   Get_File (File);

   --  Get all values
   while not End_Of_File (File) loop
      Array_Height := Array_Height + 1;
      declare
         Str   : constant String := Get_Line (File);
         Value : Hegiht;
         Last  : Positive;
      begin
         if Array_Width = Natural'First then
            Array_Width := Str'Length;
         end if;
         for Char of Str loop
            Ada.Integer_Text_IO.Get (Char & "", Value, Last);
            Values.Append (Value);
         end loop;
      end;
   end loop;

   --  Exit the program if there is no values
   if Values.Is_Empty then
      Close_If_Open (File);
      Put_Line ("The input file is empty.");
      return;
   end if;

   declare
      Cave_Heightmap : Cave_Heightmap_Array (Natural'First .. Array_Height + 1, Natural'First .. Array_Width + 1) :=
        (others => (others => Hegihtmap_Values'Last));
      Curs           : Cursor  := Values.First;
      Result         : Natural := Natural'First;
   begin
      --  Initialize Array
      for Line in 1 .. Array_Height loop
         for Column in 1 .. Array_Width loop
            Cave_Heightmap (Line, Column) := Element (Curs);
            Curs := Next (Curs);
         end loop;
      end loop;

      --  Do the puzzle
      Solve_Puzzle : declare
         Current : Hegihtmap_Values;
      begin
         for Line in 1 .. Array_Height loop
            for Column in 1 .. Array_Width loop
               Current := Cave_Heightmap (Line, Column);

               if Current < Cave_Heightmap (Line - 1, Column)
                 and then Current < Cave_Heightmap (Line + 1, Column)
                 and then Current < Cave_Heightmap (Line, Column - 1)
                 and then Current < Cave_Heightmap (Line, Column + 1)
               then
                  Result := Result + Current + 1;
               end if;
            end loop;
         end loop;
      end Solve_Puzzle;

      Put ("Result: ");
      Ada.Integer_Text_IO.Put (Item  => Result,
                               Width => 0);
      New_Line;
   end;

   Close_If_Open (File);

exception
   when Occur : others =>
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (Occur));

      Close_If_Open (File);
end Main;
