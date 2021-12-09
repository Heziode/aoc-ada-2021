with Ada.Containers.Bounded_Hashed_Maps,
     Ada.Containers.Hashed_Sets,
     Ada.Containers.Synchronized_Queue_Interfaces,
     Ada.Containers.Unbounded_Priority_Queues,
     Ada.Containers.Unbounded_Synchronized_Queues,
     Ada.Containers.Vectors,
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

   package Integer_Vectors is new Ada.Containers.Vectors (Natural, Natural);
   use Integer_Vectors;

   subtype Hegihtmap_Values is Natural range 0 .. 10;
   subtype Hegiht is Hegihtmap_Values range 0 .. 9;

   type Cave_Heightmap_Array is array (Natural range <>, Natural range <>) of Hegihtmap_Values;

   type Location is record
      Line   : Natural;
      Column : Natural;
   end record;

   subtype Adjacent_Index is Integer range -1 .. 1;
   type Adjacent_Location is record
      Line   : Adjacent_Index;
      Column : Adjacent_Index;
   end record;

   type Adjacent is (Top, Right, Bottom, Left);
   type Adjacent_Array is array (Adjacent) of Adjacent_Location;

   --  This Hash function transform a 2 dimensional location to an unique ID using Cantor pairing enumeration.
   --  @param Loc A 2 dimensional location
   --  @returns Return the corresponding hash
   --  @link https://en.wikipedia.org/wiki/Pairing_function#Cantor_pairing_function
   function Hash (Loc : Location) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type (((Loc.Line + Loc.Column) * (Loc.Line + Loc.Column + 1) / 2) + Loc.Column));

   function Is_Equal (Loc_1, Loc_2 : Location) return Boolean is
     (Loc_1.Line = Loc_2.Line and Loc_1.Column = Loc_2.Column);

   package Location_Sets is new Ada.Containers.Hashed_Sets (Element_Type        => Location,
                                                            Hash                => Hash,
                                                            Equivalent_Elements => Is_Equal,
                                                            "="                 => Is_Equal);

   Adjacents : constant Adjacent_Array := (Top    => (-1, 0),
                                           Right  => (0, 1),
                                           Bottom => (1, 0),
                                           Left   => (0, -1));

   File                 : File_Type;
   Start_Time, End_Time : CPU_Time;
   Execution_Duration   : Time_Span;
   Values               : Vector  := Empty_Vector;
   Array_Width,
   Array_Height         : Natural := Natural'First;
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
      Curs           : Cursor            := Values.First;
      Result         : Natural           := Natural'First;
      Used_Locations : Location_Sets.Set := Location_Sets.Empty_Set;
   begin
      --  Initialize Array
      for Line in 1 .. Array_Height loop
         for Column in 1 .. Array_Width loop
            Cave_Heightmap (Line, Column) := Element (Curs);
            Curs := Next (Curs);
         end loop;
      end loop;

      Values := Empty_Vector;

      --  Do the puzzle
      Start_Time := Ada.Execution_Time.Clock;
      Solve_Puzzle : declare
         package Location_Queue_Interfaces is
           new Ada.Containers.Synchronized_Queue_Interfaces
             (Element_Type => Location);

         package Location_Queue is new Ada.Containers.Unbounded_Synchronized_Queues
           (Queue_Interfaces => Location_Queue_Interfaces);

         package Integer_Vectors_Sorting is new Integer_Vectors.Generic_Sorting;

         Current : Hegihtmap_Values;
         Loc     : Location;
      begin
         for Line in 1 .. Array_Height loop
            for Column in 1 .. Array_Width loop
               Loc     := (Line, Column);
               Current := Cave_Heightmap (Loc.Line, Loc.Column);
               if Used_Locations.Contains (Loc) or else Current >= 9 then
                  goto Continue;
               end if;

               Compute_Basin_Size : declare
                  use Location_Queue;

                  Basin_Queue : Queue;
                  Size        : Natural := Natural'First;
                  Current_Loc : Location;
               begin
                  Basin_Queue.Enqueue ((Line, Column));
                  while Natural (Basin_Queue.Current_Use) > 0 loop
                     Basin_Queue.Dequeue (Loc);

                     if Used_Locations.Contains (Loc) then
                        goto Continue_Exploring_Basin;
                     end if;

                     Size := Size + 1;
                     Used_Locations.Include (Loc);
                     for Adjacent : Adjacent_Location of Adjacents loop
                        Current_Loc := (Loc.Line + Adjacent.Line, Loc.Column + Adjacent.Column);
                        Current     := Cave_Heightmap (Current_Loc.Line, Current_Loc.Column);

                        if Current_Loc.Line in 1 .. Array_Height
                          and then Current_Loc.Column in 1 .. Array_Width
                          and then Current < 9
                        then
                           Basin_Queue.Enqueue (Current_Loc);
                        end if;
                     end loop;

                     <<Continue_Exploring_Basin>>
                  end loop;

                  Values.Append (Size);
               end Compute_Basin_Size;

               <<Continue>>
            end loop;
         end loop;

         Integer_Vectors_Sorting.Sort (Values);

         Result :=
           Values.Element (Values.Last_Index)
           * Values.Element (Values.Last_Index - 1)
           * Values.Element (Values.Last_Index - 2);
      end Solve_Puzzle;

      End_Time           := Ada.Execution_Time.Clock;
      Execution_Duration := End_Time - Start_Time;

      Put ("Result: ");
      Ada.Integer_Text_IO.Put (Item  => Result,
                               Width => 0);
      New_Line;

      Put_Line ("(Took " & Duration'Image (To_Duration (Execution_Duration) * 1_000_000) & "µs)");
   end;

   Close_If_Open (File);

exception
   when Occur : others =>
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (Occur));

      Close_If_Open (File);
end Main;
