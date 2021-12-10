with Ada.Characters,
     Ada.Containers.Hashed_Maps,
     Ada.Containers.Synchronized_Queue_Interfaces,
     Ada.Containers.Unbounded_Synchronized_Queues,
     Ada.Containers.Vectors,
     Ada.Execution_Time,
     Ada.Long_Long_Integer_Text_IO,
     Ada.Real_Time,
     Ada.Text_IO;

with Utils;

procedure Main is
   use Ada.Execution_Time,
       Ada.Real_Time,
       Ada.Text_IO;
   use Utils;

   subtype Chunks is Character
     with Static_Predicate => Chunks in
       '('..'('
         | '{'..'{'
           | '['..'['
             | '<'..'<'
               | ')'..')'
                 | '}'..'}'
                   | ']'..']'
                     | '>'..'>';
   subtype Open_Chunks is Chunks
     with Static_Predicate => Open_Chunks in
       '('..'('
         | '{'..'{'
           | '['..'['
             | '<'..'<';
   subtype Close_Chunks is Chunks
     with Static_Predicate => Close_Chunks in
       ')'..')'
         | '}'..'}'
           | ']'..']'
             | '>'..'>';

   function ID_Hashed (Id : Open_Chunks) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type (Open_Chunks'Pos (Id)));

   package Corresponding_Chunk_Maps is new Ada.Containers.Hashed_Maps (Key_Type        => Open_Chunks,
                                                                       Element_Type    => Close_Chunks,
                                                                       Hash            => ID_Hashed,
                                                                       Equivalent_Keys => "=");

   package Chunks_Vectors is new Ada.Containers.Vectors (Natural, Chunks);
   subtype Long_Long_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'Last;
   package Natural_Vectors is new Ada.Containers.Vectors (Natural, Long_Long_Natural);
   package Natural_Vectors_Sorting is new Natural_Vectors.Generic_Sorting;

   package Chunks_Vectors_Vectors is new Ada.Containers.Vectors (Natural, Chunks_Vectors.Vector, Chunks_Vectors."=");

   package Chunks_Queue_Interfaces is
     new Ada.Containers.Synchronized_Queue_Interfaces
       (Element_Type => Open_Chunks);

   package Chunks_Queue is new Ada.Containers.Unbounded_Synchronized_Queues
     (Queue_Interfaces => Chunks_Queue_Interfaces);

   Corresponding_Chunk  : Corresponding_Chunk_Maps.Map;
   File                 : File_Type;
   Start_Time, End_Time : CPU_Time;
   Execution_Duration   : Time_Span;
   Values               : Chunks_Vectors_Vectors.Vector := Chunks_Vectors_Vectors.Empty_Vector;
   Result               : Long_Long_Natural := 0;
begin
   Get_File (File);

   --  Get all values
   while not End_Of_File (File) loop
      declare
         use Chunks_Vectors;

         Str   : constant String := Get_Line (File);
         Row   : Vector := Empty_Vector;
      begin
         for Char of Str loop
            Row.Append (Char);
         end loop;
         Values.Append (Row);
      end;
   end loop;

   --  Exit the program if there is no values
   if Values.Is_Empty then
      Close_If_Open (File);
      Put_Line ("The input file is empty.");
      return;
   end if;

   Corresponding_Chunk_Maps.Insert (Corresponding_Chunk, '(', ')');
   Corresponding_Chunk_Maps.Insert (Corresponding_Chunk, '{', '}');
   Corresponding_Chunk_Maps.Insert (Corresponding_Chunk, '[', ']');
   Corresponding_Chunk_Maps.Insert (Corresponding_Chunk, '<', '>');

   --  Do the puzzle
   Start_Time := Ada.Execution_Time.Clock;
   Solve_Puzzle : declare
      Solutions : Natural_Vectors.Vector;
   begin
      for Row of Values loop
         declare
            use Chunks_Vectors;

            Open_Chunk           : Vector;
            Current_Open_Chunk   : Open_Chunks;
            Current_Close_Chunck : Close_Chunks;
            Is_Valid             : Boolean := True;
         begin
            Check_Line : for Char of Row loop
               if Char in Open_Chunks then
                  Open_Chunk.Append (Char);
               else
                  Current_Open_Chunk := Open_Chunk.Last_Element;
                  Open_Chunk.Delete_Last;
                  Current_Close_Chunck := Char;
                  if Corresponding_Chunk.Element (Current_Open_Chunk) /= Current_Close_Chunck then
                     Is_Valid := False;
                     exit Check_Line;
                  end if;
               end if;
            end loop Check_Line;

            if Is_Valid then
               while not Open_Chunk.Is_Empty loop
                  Current_Open_Chunk := Open_Chunk.Last_Element;
                  Open_Chunk.Delete_Last;
                  Current_Close_Chunck := Corresponding_Chunk.Element (Current_Open_Chunk);
                  case Current_Close_Chunck is
                  when ')' =>
                     Result := Result * 5 + 1;
                  when ']' =>
                     Result := Result * 5 + 2;
                  when '}' =>
                     Result := Result * 5 + 3;
                  when '>' =>
                     Result := Result * 5 + 4;
                  end case;
               end loop;
               Natural_Vectors.Append (Solutions, Result);
               Result := Long_Long_Natural'First;
            end if;
         end;
      end loop;

      Natural_Vectors_Sorting.Sort (Solutions);

      declare
         use Natural_Vectors, Ada.Containers;
         Index : constant Count_Type := Count_Type (Solutions.Length) / Count_Type (2);
      begin
         Result := Solutions.Element (Integer (Index));
      end;
   end Solve_Puzzle;

   End_Time           := Ada.Execution_Time.Clock;
   Execution_Duration := End_Time - Start_Time;

   Put ("Result: ");
   Ada.Long_Long_Integer_Text_IO.Put (Item  => Result,
                                      Width => 0);
   New_Line;

   Put_Line ("(Took " & Duration'Image (To_Duration (Execution_Duration) * 1_000_000) & "µs)");


   Close_If_Open (File);
exception
   when Occur : others =>
      Close_If_Open (File);
      raise;
end Main;
