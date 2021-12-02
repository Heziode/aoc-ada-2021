with Ada.Characters.Latin_9,
     Ada.Exceptions,
     Ada.Integer_Text_IO,
     Ada.Text_IO;

with Utils;

procedure Main is
   use Ada.Text_IO;
   use Utils;

   package L9 renames Ada.Characters.Latin_9;

   type Movements is (Forward, Down, Up);

   File                : File_Type;
   Horizontal_Position : Natural := 0;
   Depth               : Natural := 0;
   Aim                 : Natural := 0;
begin
   Get_File (File);

   --  Get all values
   while not End_Of_File (File) loop
      declare
         Line : constant String := Get_Line (File);
      begin
         Split_Value : for Index in Line'Range loop
            if Line (Index) = L9.Space then
               Solve_Puzzle : declare
                  Movement : constant Movements := Movements'Value (Line (Line'First .. Index - 1));
                  Value : constant Natural := Natural'Value (Line (Index + 1 .. Line'Last));
               begin
                  case Movement is
                  when Forward =>
                     Horizontal_Position := Horizontal_Position + Value;
                     Depth               := Depth + Aim * Value;
                  when Down =>
                     Aim := Aim + Value;
                  when Up =>
                     Aim := Aim - Value;
                  end case;
               end Solve_Puzzle;
               exit Split_Value;
            end if;

         end loop Split_Value;
      end;
   end loop;

   --  Print the result
   Put ("Result: ");
   Ada.Integer_Text_IO.Put (Item  => Horizontal_Position * Depth,
                            Width => 0);
   New_Line;

   Close_If_Open (File);

exception
   when Occur : others =>
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (Occur));

      Close_If_Open (File);
end Main;
