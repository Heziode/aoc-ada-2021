with Ada.Containers.Vectors,
     Ada.Exceptions,
     Ada.Integer_Text_IO,
     Ada.Strings.Unbounded,
     Ada.Text_IO;

with Utils;

procedure Main is
   use Ada.Containers, Ada.Strings.Unbounded, Ada.Text_IO;
   use Utils;

   subtype Binary_Values is Natural range 0 .. 1;

   package Unbounded_String_Vectors is new Ada.Containers.Vectors (Natural, Unbounded_String);
   use Unbounded_String_Vectors;

   package Unbounded_String_Cursor_Vectors is new Ada.Containers.Vectors (Natural, Cursor);

   type Rate_Arrays is array (Binary_Values'Range) of Natural;
   type Binary_Maps is array (Binary_Values'Range) of Unbounded_String_Cursor_Vectors.Vector;
   type Strategies is (Most_Common, Least_Common);

   --  Given a String which represents a Natural in binary format, it returns the corresponding Natural.
   --  @param Str A String which represents a Natural in binary format
   --  @retuns Returns the corresponding Natural
   function Binary_String_To_Number (Str : String) return Natural;

   function Binary_String_To_Number (Str : String) return Natural
   is
      Exponent : Natural := Natural'First;
      Result   : Natural := Natural'First;
   begin
      for Elt of reverse Str loop
         if Elt = '1' then
            Result := Result + 2 ** Exponent;
         end if;
         Exponent := Exponent + 1;
      end loop;

      return Result;
   end Binary_String_To_Number;

   --  Find the result in a set of String which represents a Natural in binary format according to criteria.
   --  @param Values Contains all values (String which represents a Natural in binary format) extracted from a file.
   --  @param Rate Indicate how many 0s and 1s there are at index "Index".
   --  @param Map An array (Called Map because each index corresponds to a filter) that contains for each index (0 and
   --    1) each values of Values that contain a 0 (for index 0 of Map) or a 1 (for index 1 of Map) at index "Index" in
   --    the the value in Values.
   --  @param Index This index denote the position in the binary string representation of a Natural.
   --  @param Strategy The strategy to apply (Most Common, Least Common).
   --  @retuns Return the value that match the criteria (Strategy)
   function Find_Value_According_To_Criteria (Values   : Vector;
                                              Rate     : Rate_Arrays;
                                              Map      : Binary_Maps;
                                              Index    : in out Positive;
                                              Strategy : Strategies)
                                              return Natural;

   function Find_Value_According_To_Criteria (Values   : Vector;
                                              Rate     : Rate_Arrays;
                                              Map      : Binary_Maps;
                                              Index    : in out Positive;
                                              Strategy : Strategies)
                                              return Natural
   is
      --  Filter the result for the next recursive call of Find_Value_According_To_Criteria.
      --  @param Previous_Filtered_Values List of Cursor (pointing to value in Values) that match the previous
      --    criteria.
      --  @param Rate Indicate how many 0s and 1s there are at index "Index".
      --  @param Map An array (Called Map because each index corresponds to a filter) that contains for each index (0
      --    and 1) each values of Values that contain a 0 (for index 0 of Map) or a 1 (for index 1 of Map) at index
      --    "Index" in the the value in Values.
      procedure Filter (Previous_Filtered_Values : Unbounded_String_Cursor_Vectors.Vector;
                        Rate                     : in out Rate_Arrays;
                        Map                      : in out Binary_Maps);

      procedure Filter (Previous_Filtered_Values : Unbounded_String_Cursor_Vectors.Vector;
                        Rate                     : in out Rate_Arrays;
                        Map                      : in out Binary_Maps)
      is
         Current_Value : Unbounded_String;
         Binary_Value  : Binary_Values;

      begin
         for Curs : Cursor of Previous_Filtered_Values loop
            Current_Value := Element (Curs);

            declare
               Str : constant String := To_String (Current_Value);
            begin
               Binary_Value := Binary_Values'Value (Str (Index .. Index));

               Map (Binary_Value).Append (Curs);
               Rate (Binary_Value) := Rate (Binary_Value) + 1;

            end;
         end loop;
      end Filter;

      New_Rate           : Rate_Arrays;
      New_Map            : Binary_Maps;
      Filtered_Map_Index : Natural;
   begin
      --  Short-circuit in the case where before having gone through the whole bit chain, there is only one solution.
      if Values.Length = 1 then
         return Binary_String_To_Number (To_String (Values.First_Element));
      end if;

      --  Apply the bit criteria strategy
      case Strategy is
      when Most_Common =>
         if Rate (1) > Rate (0) or else Rate (0) = Rate (1) then
            --  Take in account values which contains a 1 at index "Index"
            Filtered_Map_Index := 1;
         else
            --  Take in account values which contains a 0 at index "Index"
            Filtered_Map_Index := 0;

         end if;
      when Least_Common =>
         if Rate (1) > Rate (0) or else Rate (0) = Rate (1) then
            --  Take in account values which contains a 0 at index "Index"
            Filtered_Map_Index := 0;
         else
            --  Take in account values which contains a 0 at index "Index"
            Filtered_Map_Index := 1;
         end if;
      end case;

      Index := Index + 1;

      --  If there is only one remaining value, is that it is the desired solution
      if Map (Filtered_Map_Index).Length = 1 then
         return Binary_String_To_Number (To_String (Element (Map (Filtered_Map_Index).First_Element)));
      end if;

      --  Apply the filter to the next Index
      Filter (Previous_Filtered_Values => Map (Filtered_Map_Index),
              Rate                     => New_Rate,
              Map                      => New_Map);

      return Find_Value_According_To_Criteria (Values   => Values,
                                               Rate     => New_Rate,
                                               Map      => New_Map,
                                               Index    => Index,
                                               Strategy => Strategy);
   end Find_Value_According_To_Criteria;

   File          : File_Type;
   Values        : Vector;
   Rate          : Rate_Arrays := (others => Natural'First);
   Bin_Map_Sort  : Binary_Maps := (others => Unbounded_String_Cursor_Vectors.Empty_Vector);
   Current_Index : Positive    := Positive'First;
begin
   Get_File (File);

   --  Get all values
   while not End_Of_File (File) loop
      declare
         Line        : constant String        := Get_Line (File);
         First_Value : constant Binary_Values := Binary_Values'Value (Line (Line'First .. Line'First));
      begin
         Values.Append (To_Unbounded_String (Line));

         Bin_Map_Sort (First_Value).Append (Values.Last);
         Rate (First_Value) := Rate (First_Value) + 1;
      end;
   end loop;

   Put_Result : declare
      Oxygen_Generator_Rating : constant Natural := Find_Value_According_To_Criteria (Values   => Values,
                                                                                      Rate     => Rate,
                                                                                      Map      => Bin_Map_Sort,
                                                                                      Index    => Current_Index,
                                                                                      Strategy => Most_Common);
      CO2_Scrubber_Rating     : Natural;
   begin
      Current_Index := Positive'First;
      CO2_Scrubber_Rating := Find_Value_According_To_Criteria (Values   => Values,
                                                               Rate     => Rate,
                                                               Map      => Bin_Map_Sort,
                                                               Index    => Current_Index,
                                                               Strategy => Least_Common);

      Put ("Result: ");
      Ada.Integer_Text_IO.Put (Item  => Oxygen_Generator_Rating * CO2_Scrubber_Rating,
                               Width => 0);
      New_Line;
   end Put_Result;

   Close_If_Open (File);

exception
   when Occur : others =>
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (Occur));

      Close_If_Open (File);
end Main;
