with Ada.Exceptions,
     Ada.Integer_Text_IO,
     Ada.Text_IO;

with Utils;

procedure Main is
   use Ada.Text_IO;
   use Utils;

   File                 : File_Type;
   Result               : Natural := Natural'First;
begin
   Get_File (File);

   if End_Of_File (File) then
      raise Program_Error with "Empty file";
   end if;

   --  Resolve puzzle while exploring file
   while not End_Of_File (File) loop
      declare
         use Ada.Integer_Text_IO;
         Line         : constant String := Get_Line (File);
         First        : Positive        := Line'First;
         Last         : Positive        := Line'First;
         Last_Index   : Positive        := Line'First;
         After_Pipe   : Boolean         := False;
         Current_Size : Natural         := Natural'First;
      begin
         while Last <= Line'Last loop
            if After_Pipe then
               -- Process data
               if Line (Last) = ' ' then
                  Current_Size := Last - First;
                  First := Last + 1;

                  if Current_Size in 2 .. 4 or Current_Size = 7 then
                     Result := Result + 1;
                  end if;
               elsif Last = Line'Last then
                  Current_Size := Last - First + 1;
                  First := Last + 1;

                  if Current_Size in 2 .. 4 or Current_Size = 7 then
                     Result := Result + 1;
                  end if;
               end if;

               Last := Last + 1;
            elsif Line (Last) = '|' then
               After_Pipe := True;

               Last  := Last + 2;
               First := Last;
            else
               Last := Last + 1;
            end if;
         end loop;
      end;
   end loop;

   Put ("Result: ");
   Ada.Integer_Text_IO.Put (Result, Width => 0);
   New_Line;

   Close_If_Open (File);

exception
   when Occur : others =>
      Close_If_Open (File);
      raise;
end Main;
