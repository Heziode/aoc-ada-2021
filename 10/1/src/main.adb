with Ada.Containers.Hashed_Maps,
     Ada.Containers.Vectors,
     Ada.Execution_Time,
     Ada.Integer_Text_IO,
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

   package Chunks_Vectors_Vectors is new Ada.Containers.Vectors (Natural, Chunks_Vectors.Vector, Chunks_Vectors."=");

   Corresponding_Chunk  : Corresponding_Chunk_Maps.Map;
   File                 : File_Type;
   Start_Time, End_Time : CPU_Time;
   Execution_Duration   : Time_Span;
   Values               : Chunks_Vectors_Vectors.Vector := Chunks_Vectors_Vectors.Empty_Vector;
   Result               : Natural := Natural'First;
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
   begin
      for Row of Values loop
         declare
            use Chunks_Vectors;

            Open_Chunk           : Vector;
            Current_Open_Chunk   : Open_Chunks;
            Current_Close_Chunck : Close_Chunks;
         begin
            Check_Line : for Char of Row loop
               if Char in Open_Chunks then
                  Open_Chunk.Append (Char);
               else
                  Current_Open_Chunk := Open_Chunk.Last_Element;
                  Open_Chunk.Delete_Last;
                  Current_Close_Chunck := Char;
                  if Corresponding_Chunk.Element (Current_Open_Chunk) /= Current_Close_Chunck then
                     case Current_Close_Chunck is
                     when ')' =>
                        Result := Result + 3;
                     when ']' =>
                        Result := Result + 57;
                     when '}' =>
                        Result := Result + 1197;
                     when '>' =>
                        Result := Result + 25137;
                     end case;
                     exit Check_Line;
                  end if;
               end if;
            end loop Check_Line;
         end;
      end loop;
   end Solve_Puzzle;

   End_Time           := Ada.Execution_Time.Clock;
   Execution_Duration := End_Time - Start_Time;

   Put ("Result: ");
   Ada.Integer_Text_IO.Put (Item  => Result,
                            Width => 0);
   New_Line;

   Put_Line ("(Took " & Duration'Image (To_Duration (Execution_Duration) * 1_000_000) & "µs)");


   Close_If_Open (File);
exception
   when Occur : others =>
      Close_If_Open (File);
      raise;
end Main;
