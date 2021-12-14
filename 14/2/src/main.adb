with Ada.Containers.Hashed_Maps,
     Ada.Execution_Time,
     Ada.Long_Long_Integer_Text_IO,
     Ada.Real_Time,
     Ada.Strings.Fixed,
     Ada.Strings.Hash,
     Ada.Strings.Unbounded,
     Ada.Text_IO;

with Utils;

procedure Main is
   use Ada.Execution_Time,
       Ada.Real_Time,
       Ada.Strings.Unbounded,
       Ada.Text_IO;
   use Utils;

   Max_Steps : constant := 40;

   subtype Rule_Str  is String (1 .. 2);
   subtype Long_Long_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   package Rule_Maps is new Ada.Containers.Hashed_Maps (Key_Type        => Rule_Str,
                                                        Element_Type    => Character,
                                                        Hash            => Ada.Strings.Hash,
                                                        Equivalent_Keys => "=",
                                                        "="             => "=");

   package Rule_Counter_Maps is new Ada.Containers.Hashed_Maps (Key_Type        => Rule_Str,
                                                                Element_Type    => Long_Long_Natural,
                                                                Hash            => Ada.Strings.Hash,
                                                                Equivalent_Keys => "=",
                                                                "="             => "=");
   use Rule_Maps;
   use Rule_Counter_Maps;

   File                 : File_Type;
   Start_Time, End_Time : CPU_Time;
   Execution_Duration   : Time_Span;
   File_Is_Empty        : Boolean               := True;
   Result               : Long_Long_Natural     := Long_Long_Natural'First;
   Rules                : Rule_Maps.Map         := Rule_Maps.Empty_Map;
   Counter              : Rule_Counter_Maps.Map := Rule_Counter_Maps.Empty_Map;
   Polymer_Template     : Unbounded_String;
begin
   Get_File (File);

   Polymer_Template := To_Unbounded_String (Get_Line (File));

   --  Get all rules
   begin
      while not End_Of_File (File) loop
         declare
            Str : constant String := Get_Line (File);
         begin
            if Str'Length = 0 then
               goto Continue_Next_Line;
            end if;

            declare
               Pattern         : constant String  := " -> ";
               Separator_Index : constant Integer :=
                 Ada.Strings.Fixed.Index (Source => Str (1 .. Str'Last), Pattern => Pattern);
               Left_Str        : constant String  := Str (1 .. Separator_Index - 1);
               Right_Str       : constant String  := Str (Separator_Index + Pattern'Length .. Str'Last);
            begin
               Rules.Include (Rule_Str (Left_Str), Right_Str (Right_Str'First));
            end;
         end;
         File_Is_Empty  := False;

         <<Continue_Next_Line>>
      end loop;
   end;

   --  Exit the program if there is no values
   if File_Is_Empty then
      Close_If_Open (File);
      Put_Line ("The input file is empty.");
      return;
   end if;

   --  Initialize Counter
   for Index in 1 .. Length (Polymer_Template) - 1 loop
      if not Counter.Contains (Element (Polymer_Template, Index) & Element (Polymer_Template, Index + 1)) then

         Counter.Include (Element (Polymer_Template, Index) & Element (Polymer_Template, Index + 1), 0);
      end if;
      Counter.Include (Element (Polymer_Template, Index) & Element (Polymer_Template, Index + 1),
                       Counter.Element (Element (Polymer_Template, Index) & Element (Polymer_Template, Index + 1)) + 1
                      );
   end loop;

   --  Do the puzzle
   Start_Time := Ada.Execution_Time.Clock;
   Solve_Puzzle : declare
   begin
      for Step in 1 .. Max_Steps loop
         declare
            Temp_Counter : Rule_Counter_Maps.Map := Rule_Counter_Maps.Empty_Map;
            Current_Key : Rule_Str;
            Current_Rule_Key : Rule_Str;
         begin
            for Curs in Counter.Iterate loop
               Current_Key := Key (Curs);
               Current_Rule_Key := Current_Key (Rule_Str'First) & Rules (Current_Key);

               if not Temp_Counter.Contains (Current_Rule_Key) then
                  Temp_Counter.Insert (Current_Rule_Key, 0);
               end if;
               Temp_Counter.Include (Current_Rule_Key,
                                     Temp_Counter.Element (Current_Rule_Key) + Counter.Element (Current_Key));

               Current_Rule_Key := Rules (Current_Key) & Current_Key (Rule_Str'Last);
               if not Temp_Counter.Contains (Current_Rule_Key) then
                  Temp_Counter.Insert (Current_Rule_Key, 0);
               end if;
               Temp_Counter.Include (Current_Rule_Key,
                                     Temp_Counter.Element (Current_Rule_Key) + Counter.Element (Current_Key));
            end loop;
            Counter := Temp_Counter;
         end;
      end loop;

      Get_Result : declare
         function Character_Hash (Elt : Character) return Ada.Containers.Hash_Type is
           (Ada.Containers.Hash_Type (Character'Pos (Elt)));

         package Character_Counter_Maps is new Ada.Containers.Hashed_Maps (Key_Type        => Character,
                                                                           Element_Type    => Long_Long_Natural,
                                                                           Hash            => Character_Hash,
                                                                           Equivalent_Keys => "=",
                                                                           "="             => "=");
         use Character_Counter_Maps;

         Final_Counter : Character_Counter_Maps.Map := Character_Counter_Maps.Empty_Map;
         Min           : Long_Long_Natural          := Long_Long_Natural'Last;
         Max           : Long_Long_Natural          := Long_Long_Natural'First;
         Current_Key   : Character;
         Current_Value : Long_Long_Natural;
      begin
         for Curs in Counter.Iterate loop
            Current_Key := Key (Curs) (Rule_Str'First);
            if not Final_Counter.Contains (Current_Key) then
               Final_Counter.Insert (Current_Key, 0);
            end if;
            Final_Counter.Include (Current_Key, Final_Counter.Element (Current_Key) + Counter.Element (Key (Curs)));
         end loop;

         Current_Key := Element (Polymer_Template, Length (Polymer_Template));
         Final_Counter.Include (Current_Key, Final_Counter.Element (Current_Key) + 1);

         for Curs in Final_Counter.Iterate loop
            Current_Value := Element (Curs);
            if Current_Value < Min then
               Min := Current_Value;
            end if;
            if Current_Value > Max then
               Max := Current_Value;
            end if;
         end loop;

         Result := Max - Min;
      end Get_Result;
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
