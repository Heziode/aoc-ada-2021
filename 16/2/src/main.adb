with Ada.Containers.Synchronized_Queue_Interfaces,
     Ada.Containers.Unbounded_Synchronized_Queues,
     Ada.Containers.Vectors,
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

   function Pop (Q : in out Queue; N : Positive; Nb_Delete : in out Long_Long_Natural) return String;

   function Pop (Q : in out Queue; N : Positive; Nb_Delete : in out Long_Long_Natural) return String is
      Result : String (1 .. N);
   begin
      for Index in 1 .. N loop
         Q.Dequeue (Result (Index));
      end loop;
      Nb_Delete := Nb_Delete + Long_Long_Natural (N);
      return Result;
   end Pop;

   File                   : File_Type;
   Start_Time, End_Time   : CPU_Time;
   Execution_Duration     : Time_Span;
   File_Is_Empty          : Boolean           := True;
   Result                 : Long_Long_Natural := Long_Long_Natural'First;
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
      function Parse_Packet (Packet : in out Queue) return Long_Long_Natural;

      function Parse_Packet (Packet : in out Queue) return Long_Long_Natural is
         package Long_Long_Natural_Vectrors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                           Element_Type => Long_Long_Natural,
                                                                           "="          => "=");

         function Inner_Parse (Packet : in out Queue; Last_Index : in out Long_Long_Natural) return Long_Long_Natural;

         function Inner_Parse (Packet : in out Queue; Last_Index : in out Long_Long_Natural) return Long_Long_Natural
         is
            use Ada.Long_Long_Integer_Text_IO;
            use Long_Long_Natural_Vectrors;

            procedure Get (Source : String; Result : out Long_Long_Natural);

            procedure Get (Source : String; Result : out Long_Long_Natural) is
               Last : Positive;
            begin
               Get ("2#" & Source & "#", Result, Last);
            end Get;

            subtype Type_Id_Value is Long_Long_Natural range 0 .. 7;

            Version : Long_Long_Natural;
            Type_Id : Type_Id_Value;
         begin
            Get (Pop (Packet, 3, Last_Index), Version);
            Get (Pop (Packet, 3, Last_Index), Type_Id);

            if Type_Id = 4 then
               declare
                  Accumulator : Unbounded_String := Null_Unbounded_String;
                  Current     : String (1 .. 1);
                  Value       : Long_Long_Natural;
               begin
                  loop
                     Current := Pop (Packet, 1, Last_Index);
                     Accumulator := Accumulator & Pop (Packet, 4, Last_Index);
                     exit when Current = "0";
                  end loop;

                  Get (To_String (Accumulator), Value);
                  return Value;
               end;
            else
               declare
                  Values               : Vector;
                  Length_Id            : Long_Long_Natural;
                  Sub_Packet_Length    : Long_Long_Natural;
                  Number_Of_Sub_Packet : Long_Long_Natural;
               begin
                  Get (Pop (Packet, 1, Last_Index), Length_Id);

                  if Length_Id = 0 then
                     Get (Pop (Packet, 15, Last_Index), Sub_Packet_Length);

                     declare
                        End_Pop : constant Long_Long_Natural := Last_Index + Sub_Packet_Length;
                     begin
                        while Last_Index < End_Pop loop
                           Values.Append (Inner_Parse (Packet, Last_Index));
                        end loop;
                     end;
                  else
                     Get (Pop (Packet, 11, Last_Index), Number_Of_Sub_Packet);

                     for Sub_Packet_Index in 1 .. Number_Of_Sub_Packet loop
                        Values.Append (Inner_Parse (Packet, Last_Index));
                     end loop;
                  end if;

                  return Value : Long_Long_Natural do
                     case Type_Id is
                        when 0 =>
                           --  Sum
                           for Elt of Values loop
                              Value := Value + Elt;
                           end loop;
                        when 1 =>
                           --  Product
                           Value := 1;
                           for Elt of Values loop
                              Value := Value * Elt;
                           end loop;
                        when 2 =>
                           --  Minimum
                           Value := Long_Long_Natural'Last;
                           for Elt of Values loop
                              if Elt < Value then
                                 Value := Elt;
                              end if;
                           end loop;
                        when 3 =>
                           --  Maximum
                           Value := Long_Long_Natural'First;
                           for Elt of Values loop
                              if Elt > Value then
                                 Value := Elt;
                              end if;
                           end loop;
                        when 4 =>
                           raise Constraint_Error with "Unexpected Type ID 4";
                        when 5 =>
                           --  Greater than
                           if Values.Element (1) > Values.Element (2) then
                              Value := 1;
                           else
                              Value := 0;
                           end if;
                        when 6 =>
                           --  Less than
                           if Values.Element (1) < Values.Element (2) then
                              Value := 1;
                           else
                              Value := 0;
                           end if;
                        when 7 =>
                           --  Equal to
                           if Values.Element (1) = Values.Element (2) then
                              Value := 1;
                           else
                              Value := 0;
                           end if;
                     end case;
                  end return;
               end;
            end if;
         end Inner_Parse;
         Last_Index : Long_Long_Natural := Long_Long_Natural'First;
      begin
         return Inner_Parse (Packet, Last_Index);
      end Parse_Packet;

   begin
      Result := Parse_Packet (Packet);
   end Solve_Puzzle;

   End_Time           := Ada.Execution_Time.Clock;
   Execution_Duration := End_Time - Start_Time;

   Put ("Result: ");
   Ada.Long_Long_Integer_Text_IO.Put (Item  => Result,
                                      Width => 0);
   New_Line;

   Put_Line ("(Took " & Duration'Image (To_Duration (Execution_Duration) * 1_000_000) & "µs)");

exception
   when others =>
      Close_If_Open (File);
      raise;
end Main;
