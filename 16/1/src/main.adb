with Ada.Containers.Synchronized_Queue_Interfaces,
     Ada.Containers.Unbounded_Synchronized_Queues,
     Ada.Execution_Time,
     Ada.Integer_Text_IO,
     Ada.Long_Long_Integer_Text_IO,
     Ada.Real_Time,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded,
     Ada.Text_IO;

with Utils;

procedure Main is
   use Ada.Execution_Time,
       Ada.Real_Time,
       Ada.Strings.Fixed,
       Ada.Strings.Unbounded,
       Ada.Text_IO;
   use Utils;

   package Character_Queue_Interfaces is
     new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => Character);

   package Character_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
     (Queue_Interfaces => Character_Queue_Interfaces);
   use Character_Queues;

   function Pop (Q : in out Queue; N : Positive; Nb_Delete : in out Natural) return String;

   function Pop (Q : in out Queue; N : Positive; Nb_Delete : in out Natural) return String is
      Result : String (1 .. N);
   begin
      for Index in 1 .. N loop
         Q.Dequeue (Result (Index));
      end loop;
      Nb_Delete := Nb_Delete + N;
      return Result;
   end Pop;

   File                   : File_Type;
   Start_Time, End_Time   : CPU_Time;
   Execution_Duration     : Time_Span;
   File_Is_Empty          : Boolean          := True;
   Result                 : Natural          := Natural'First;
   Packet                 : Queue;
begin
   Get_File (File);

   --  Get all values
   declare
   begin
      while not End_Of_File (File) loop
         declare
            Str   : constant String := Get_Line (File);
            Value : Natural;
            Temp  : String (1 .. 8);
         begin
            for Char of Str loop
               Value := Natural'Value ("16#" & Char & "#");
               Ada.Integer_Text_IO.Put (Temp,
                                        Value,
                                        Base  => 2);
               File_Is_Empty := False;

               declare
                  Trimmed           : constant String := Trim (Temp, Ada.Strings.Both);
                  Formatted_Bin_Str : constant String :=
                    Tail (Trimmed (Trimmed'First + 2  .. Trimmed'Last - 1), 4, '0');
               begin
                  for Elt of Formatted_Bin_Str loop
                     Packet.Enqueue (Elt);
                  end loop;
               end;
            end loop;
         end;
      end loop;
   end;

   --  Exit the program if there is no values
   if File_Is_Empty then
      Close_If_Open (File);
      Put_Line ("The input file is empty.");
      return;
   end if;

   --  Do the puzzle
   Start_Time := Ada.Execution_Time.Clock;
   Solve_Puzzle : declare
      procedure Parse_Packet (Packet : in out Queue);

      procedure Parse_Packet (Packet : in out Queue) is
         procedure Inner_Parse (Packet : in out Queue; Last_Index : in out Natural);

         procedure Inner_Parse (Packet : in out Queue; Last_Index : in out Natural)
         is
            use Ada.Integer_Text_IO, Ada.Long_Long_Integer_Text_IO;

            Version : Natural;
            Type_Id : Natural;
            Length_Id : Natural;
            Sub_Packet_Length : Natural;
            Number_Of_Sub_Packet : Natural;
            Value : Long_Long_Natural;
            Last : Positive;
         begin
            Get ("2#" & Pop (Packet, 3, Last_Index) & "#", Version, Last);
            Get ("2#" & Pop (Packet, 3, Last_Index) & "#", Type_Id, Last);
            Result := Result + Version;

            if Type_Id = 4 then
               declare
                  Accumulator : Unbounded_String := Null_Unbounded_String;
                  Current     : String (1 .. 1);
               begin
                  loop
                     Current := Pop (Packet, 1, Last_Index);
                     Accumulator := Accumulator & Pop (Packet, 4, Last_Index);
                     exit when Current = "0";
                  end loop;

                  Get ("2#" & To_String (Accumulator) & "#", Value, Last);
               end;
            else
               Get ("2#" & Pop (Packet, 1, Last_Index) & "#", Length_Id, Last);

               if Length_Id = 0 then
                  Get ("2#" & Pop (Packet, 15, Last_Index) & "#", Sub_Packet_Length, Last);

                  declare
                     End_Pop : constant Natural := Last_Index + Sub_Packet_Length;
                  begin
                     while Last_Index < End_Pop loop
                        Inner_Parse (Packet, Last_Index);
                     end loop;
                  end;
               else
                  Get ("2#" & Pop (Packet, 11, Last_Index) & "#", Number_Of_Sub_Packet, Last);

                  for Sub_Packet_Index in 1 .. Number_Of_Sub_Packet loop
                     Inner_Parse (Packet, Last_Index);
                  end loop;
               end if;
            end if;
         end Inner_Parse;
         Last_Index : Natural := Natural'First;
      begin
         Inner_Parse (Packet, Last_Index);
      end Parse_Packet;

   begin
      Parse_Packet (Packet);
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
