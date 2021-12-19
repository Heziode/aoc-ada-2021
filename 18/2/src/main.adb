with Ada.Containers.Vectors,
     Ada.Containers.Doubly_Linked_Lists,
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

   Opening_Bracket : constant := -2;
   Closing_Bracket : constant := -1;
   subtype Snailfish_Values is Integer range Opening_Bracket .. Integer'Last;

   package Snailfish_Math_List is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Snailfish_Values,
                                                                          "="          => "=");
   use Snailfish_Math_List;

   function "+" (Left, Right : List) return List;
   function "+" (Left, Right : List) return List is
      Result : List := Copy (Left);
   begin
      Result.Prepend (New_Item => Opening_Bracket,
                      Count    => 1);

      for Elt of Right loop
         Result.Append (Elt);
      end loop;

      Result.Append (Closing_Bracket);

      return Result;
   end "+";

   function "+" (Left : Snailfish_Math_List.Cursor; Right : Positive) return Snailfish_Math_List.Cursor;

   function "+" (Left : Snailfish_Math_List.Cursor; Right : Positive) return Snailfish_Math_List.Cursor is
      Result : Snailfish_Math_List.Cursor := Left;
   begin
      for Index in 1 .. Right loop
         Result := Next (Result);
      end loop;
      return Result;
   end "+";

   function "-" (Left : Snailfish_Math_List.Cursor; Right : Positive) return Snailfish_Math_List.Cursor;

   function "-" (Left : Snailfish_Math_List.Cursor; Right : Positive) return Snailfish_Math_List.Cursor is
      Result : Snailfish_Math_List.Cursor := Left;
   begin
      for Index in 1 .. Right loop
         Result := Previous (Result);
      end loop;
      return Result;
   end "-";

   procedure Put (Snailfish_Number : List);

   procedure Put (Snailfish_Number : List) is
      Curs : Snailfish_Math_List.Cursor := Snailfish_Number.First;
   begin
      while Has_Element (Curs) loop
         case Element (Curs) is
            when Opening_Bracket =>
               Put ("[");
            when Closing_Bracket =>
               Put ("]");
               if Has_Element (Curs + 1) and then Element (Curs + 1) = Opening_Bracket then
                  Put (", ");
               end if;
            when others =>
               if Element (Curs - 1) > Closing_Bracket then
                  Put (", ");
               end if;
               Ada.Integer_Text_IO.Put (Item  => Element (Curs), Width => 0);
         end case;
         Curs := Curs + 1;
      end loop;
   end Put;

   package Snailfish_Math_Vectors is new Ada.Containers.Vectors (Index_Type   => Natural,
                                                                 Element_Type => List,
                                                                 "="          => "=");
   use Snailfish_Math_Vectors;

   function Pop (Vec : in out Vector) return List;

   function Pop (Vec : in out Vector) return List is
      Result : List;
   begin
      if Vec.Is_Empty then
         raise Constraint_Error with "Container is empty";
      end if;
      Result := Vec.First_Element;
      Vec.Delete_First;
      return Result;
   end Pop;

   File                 : File_Type;
   Start_Time, End_Time : CPU_Time;
   Execution_Duration   : Time_Span;
   File_Is_Empty        : Boolean := True;
   Result               : Natural := Natural'First;
   Result_List          : List;
   Input                : Vector;
   Output               : Vector;
begin
   Get_File (File);

   --  Get all values
   declare
   begin
      while not End_Of_File (File) loop
         declare
            Str              : constant String := Get_Line (File);
            Last             : Positive;
            Value            : Natural;
            Snailfish_Number : List;
         begin
            for Char of Str loop
               case Char is
                  when '[' =>
                     Snailfish_Number.Append (Opening_Bracket);
                  when ']' =>
                     Snailfish_Number.Append (Closing_Bracket);
                  when '0' .. '9' =>
                     Ada.Integer_Text_IO.Get (Char & "", Value, Last);
                     Snailfish_Number.Append (Value);
                  when others =>
                     null;
               end case;
               File_Is_Empty := False;
            end loop;
            Input.Append (Snailfish_Number);
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
      use Ada.Containers;
      procedure Reduce (Snailfish_Number : in out List);

      procedure Reduce (Snailfish_Number : in out List) is
         function Find_Rightmost (From : Snailfish_Math_List.Cursor) return Snailfish_Math_List.Cursor;

         function Find_Rightmost (From : Snailfish_Math_List.Cursor) return Snailfish_Math_List.Cursor is
            Result : Snailfish_Math_List.Cursor := Next (From);
         begin
            while Has_Element (Result) loop
               if Element (Result) > Closing_Bracket then
                  return Result;
               end if;
               Result := Next (Result);
            end loop;
            return Result;
         end Find_Rightmost;

         Is_Reduced : Boolean := True;
         Depth      : Natural := Natural'First;
         Elt        : Snailfish_Values;
         Curs,
         Leftmost,
         Rightmost,
         Temp_Curs  : Snailfish_Math_List.Cursor;
      begin --  Reduce
         loop
            Is_Reduced := True;

            --  Explode
            Curs     := Snailfish_Number.First;
            Leftmost := Snailfish_Number.First;
            Depth    := Natural'First;
            loop
               Elt := Element (Curs);
               case Elt is
                  when Opening_Bracket =>
                     Depth := Depth + 1;
                  when Closing_Bracket =>
                     Depth := Depth - 1;
                  when others =>
                     if Depth > 4 and Elt > Closing_Bracket and Element (Curs + 1) > Closing_Bracket then
                        --  Left Part
                        if Has_Element (Leftmost) and then Element (Leftmost) > Closing_Bracket then
                           Snailfish_Number.Replace_Element (Leftmost, Element (Leftmost) + Elt);
                           --  Remove '['
                           Temp_Curs := Curs - 1;
                           Delete (Snailfish_Number, Temp_Curs);

                           --  Replace current value by 0
                           Snailfish_Number.Replace_Element (Curs, 0);
                           Leftmost  := Curs;
                           Curs      := Curs + 1;
                        else
                           --  Replace previous '[' by 0
                           Snailfish_Number.Replace_Element (Curs - 1, 0);
                           --  Delete current value
                           Temp_Curs := Curs;
                           Curs      := Curs + 1;
                           Delete (Snailfish_Number, Temp_Curs);
                           Leftmost  := Curs;
                        end if;

                        --  Right Part
                        Elt := Element (Curs);
                        Rightmost := Find_Rightmost (Curs);
                        if Has_Element (Rightmost) then
                           Snailfish_Number.Replace_Element (Rightmost, Element (Rightmost) + Elt);
                           --  Delete current value
                           Temp_Curs := Curs;
                           Curs      := Curs + 1;
                           Delete (Snailfish_Number, Temp_Curs);
                           --  Delete next ']'
                           Temp_Curs := Curs;
                           Curs      := Curs + 1;
                           Delete (Snailfish_Number, Temp_Curs);
                           Leftmost  := Curs;
                        else
                           --  Replace previous value by 0
                           Snailfish_Number.Replace_Element (Curs - 1, 0);
                           --  Delete current value
                           Temp_Curs := Curs;
                           Curs      := Curs + 1;
                           Delete (Snailfish_Number, Temp_Curs);
                           Temp_Curs := Curs;
                           Curs      := Curs + 1;
                           Delete (Snailfish_Number, Temp_Curs);
                           Leftmost  := Curs;
                        end if;

                        Is_Reduced := False;
                     else
                        Leftmost := Curs;
                     end if;
               end case;
               Curs := Curs + 1;
               exit when not Has_Element (Curs) or not Is_Reduced;
            end loop;

            --  Split
            if Is_Reduced then
               Curs := Snailfish_Number.First;
               loop
                  Elt := Element (Curs);
                  case Elt is
                  when Opening_Bracket .. Closing_Bracket =>
                     --  Nothing to do here
                     null;
                  when others =>
                     if Elt > 9 then
                        Snailfish_Number.Insert (Before   => Curs,
                                                 New_Item => Opening_Bracket);
                        Snailfish_Number.Insert (Before   => Curs,
                                                 New_Item => Elt / 2);
                        Snailfish_Number.Insert (Before   => Curs,
                                                 New_Item => (Elt + 1) / 2);
                        Snailfish_Number.Replace_Element (Curs, Closing_Bracket);

                        Is_Reduced := False;
                     else
                        Leftmost := Curs;
                     end if;
                  end case;
                  Curs := Curs + 1;
                  exit when not Has_Element (Curs) or not Is_Reduced;
               end loop;
            end if;
            exit when Is_Reduced;
         end loop;
      end Reduce;

      Pair             : Vector;
      Best_Pair        : Vector;
      Best_Pair_Result : List;
      Best_Magnitude   : Natural := Natural'First;
   begin
      for Left of Input loop
         for Right of Input loop
            if Left = Right then
               goto Continue;
            end if;
            for Switch in 1 .. 2 loop
               Pair := Empty_Vector;
               if Switch = 1 then
                  Pair.Append (Left);
                  Pair.Append (Right);
               else
                  Pair.Append (Right);
                  Pair.Append (Left);
               end if;
               Output := Pair;

               Reducer : while Length (Pair) > 1 loop
                  declare
                     Left   : constant List := Pop (Pair);
                     Right  : constant List := Pop (Pair);
                     Result : List          := Left + Right;
                  begin
                     Reduce (Result);
                     Pair.Prepend (Result, 1);
                  end;
               end loop Reducer;

               Result_List := Pair.First_Element;

               Magnitude : declare
                  Snailfish_Number : List := Pair.First_Element;
                  Elt              : Integer;
                  Curs,
                  Temp_Curs        : Snailfish_Math_List.Cursor;
               begin
                  while Snailfish_Number.Length > 1 loop
                     if not Has_Element (Curs) then
                        Curs := Snailfish_Number.First;
                     end if;
                     Elt := Element (Curs);
                     if Elt not in Opening_Bracket | Closing_Bracket
                       and then Has_Element (Curs + 1)
                       and then Element (Curs + 1) not in Opening_Bracket | Closing_Bracket

                     then
                        Snailfish_Number.Replace_Element (Curs, Elt * 3 + Element (Curs + 1) * 2);

                        --  Remove '['
                        Temp_Curs := Curs - 1;
                        Delete (Snailfish_Number, Temp_Curs);

                        --  Remove next number
                        Curs      := Curs + 2;
                        Temp_Curs := Curs - 1;
                        Delete (Snailfish_Number, Temp_Curs);

                        --  Remove ']'
                        if Has_Element (Curs + 1) then
                           Curs := Curs + 1;
                           Temp_Curs := Curs - 1;
                           Delete (Snailfish_Number, Temp_Curs);
                        else
                           Delete (Snailfish_Number, Curs);
                        end if;
                     else
                        Curs := Curs + 1;
                     end if;
                  end loop;
                  Result := Snailfish_Number.First_Element;
               end Magnitude;

               if Result > Best_Magnitude then
                  Best_Magnitude := Result;
                  Best_Pair_Result := Result_List;
                  Best_Pair := Output;
               end if;
            end loop;
            <<Continue>>
         end loop;
      end loop;
      Result      := Best_Magnitude;
      Result_List := Best_Pair_Result;
      Output      := Best_Pair;
   end Solve_Puzzle;

   End_Time           := Ada.Execution_Time.Clock;
   Execution_Duration := End_Time - Start_Time;

   Put_Line ("Highest magnitude obtained with the pair:");
   Put (Output.First_Element);
   Put (" + ");
   Put (Element (Next (Output.First)));
   New_Line;

   Put_Line ("Which reduces to:");
   Put (Result_List);
   New_Line;
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
