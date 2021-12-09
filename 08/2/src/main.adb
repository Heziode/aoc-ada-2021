with Ada.Containers.Generic_Array_Sort,
     Ada.Containers.Generic_Constrained_Array_Sort,
     Ada.Containers.Indefinite_Hashed_Maps,
     Ada.Integer_Text_IO,
     Ada.Strings.Bounded,
     Ada.Strings.Bounded.Hash,
     Ada.Text_IO;

with Utils;

procedure Main is
   use Ada.Text_IO;
   use Utils;

   package Digit_Str is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 7);
   use Digit_Str;

   subtype Segment is Character range 'a' .. 'g';
   subtype Digit is Bounded_String;
   subtype Seven_Segments_Digits is Natural range 0 .. 9;

   --  Sort the characters in a String
   procedure String_Sort is new Ada.Containers.Generic_Array_Sort (Positive, Character, String);

   function Digit_Hash is new Ada.Strings.Bounded.Hash (Digit_Str);

   package Segments_To_Digit_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Digit,
      Element_Type    => Seven_Segments_Digits,
      Hash            => Digit_Hash,
      Equivalent_Keys => "=");

   function Segment_Hash (Elt : Segment) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type (Segment'Pos (Elt)));

   package Mapping_Table_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Segment,
      Element_Type    => Segment,
      Hash            => Segment_Hash,
      Equivalent_Keys => "=");

   subtype Digits_Array_Index is Positive range 1 .. 10;
   type Digits_Array is array (Digits_Array_Index) of Digit;

   function Is_Lower_Than (Left, Right : Digit) return Boolean is (Length (Left) < Length (Right));

   procedure Digits_Sort is new Ada.Containers.Generic_Constrained_Array_Sort (Index_Type   => Digits_Array_Index,
                                                                               Element_Type => Digit,
                                                                               Array_Type   => Digits_Array,
                                                                               "<"          => Is_Lower_Than);

   --  Given a String that represent a segment (like "be", or "fgaecd", etc.) it retrieve the corresponding segment.
   --  Note: when the word "associated" is used, it means that we cannot know exactly which segment corresponds
   --  to which other segment.
   --  When the word "corresponding" is used, it means that we know exactly which segment corresponds to which
   --  other segment.
   --
   --  @param Seg a String that represent a digit
   --  @returns Retruns the corresponding mapping table between segments of digits
   function Get_Corresponding_Segments (Digit_Array : Digits_Array) return Mapping_Table_Maps.Map;

   --------------------------------
   -- Get_Corresponding_Segments --
   --------------------------------

   function Get_Corresponding_Segments (Digit_Array : Digits_Array) return Mapping_Table_Maps.Map is
      use Mapping_Table_Maps;

      Mapping_Table : Mapping_Table_Maps.Map;
      CF_Seg,
      BD_Seg,
      EG_Seg        : Digit   := Null_Bounded_String;
      Can_Break_CF,
      Can_Break_BD  : Boolean := False;
   begin
      --  Get the number 1 to get associated segments "c" and "f"
      CF_Seg := Digit_Array (1);

      --  Get the number 7 to find the corresponding segment of "a"
      for Char of To_String (Digit_Array (2)) loop
         if Index (CF_Seg, Char & "", 1) = 0 then
            Mapping_Table.Include (Char, 'a');
            exit;
         end if;
      end loop;

      --  Get the number 4 to find associated to "b" and "d"
      for Char of To_String (Digit_Array (3)) loop
         if Index (CF_Seg, Char & "", 1) = 0 then
            BD_Seg := BD_Seg & Char;
         end if;
      end loop;

      for Idx in 7 .. 9 loop
         --  Find the number 6 to find corresponding segment of "f" and "c"
         if (Index (Digit_Array (Idx), Element (CF_Seg, 1) & "") > 0) /=
           (Index (Digit_Array (Idx), Element (CF_Seg, 2) & "") > 0)
         then
            if Index (Digit_Array (Idx), Element (CF_Seg, 1) & "") > 0 then
               Mapping_Table.Include (Element (CF_Seg, 1), 'f');
               Mapping_Table.Include (Element (CF_Seg, 2), 'c');
            else
               Mapping_Table.Include (Element (CF_Seg, 2), 'f');
               Mapping_Table.Include (Element (CF_Seg, 1), 'c');
            end if;
            Can_Break_CF := True;
         end if;

         --  Find the number 0 to find corresponding segment of "b" and "d"
         if (Index (Digit_Array (Idx), Element (BD_Seg, 1) & "") > 0) /=
           (Index (Digit_Array (Idx), Element (BD_Seg, 2) & "") > 0)
         then
            if Index (Digit_Array (Idx), Element (BD_Seg, 1) & "") > 0 then
               Mapping_Table.Include (Element (BD_Seg, 1), 'b');
               Mapping_Table.Include (Element (BD_Seg, 2), 'd');
            else
               Mapping_Table.Include (Element (BD_Seg, 2), 'b');
               Mapping_Table.Include (Element (BD_Seg, 1), 'd');
            end if;
            Can_Break_BD := True;
         end if;

         if Can_Break_CF and Can_Break_BD then
            exit;
         end if;
      end loop;

      for Char in Character range 'a' .. 'g' loop
         if not Mapping_Table.Contains (Char) then
            EG_Seg := EG_Seg & Char;
         end if;
      end loop;

      --  Find the number 9 to find corresponding segment of "e" and "g"
      for Idx in 7 .. 10 loop
         if (Index (Digit_Array (Idx), Element (EG_Seg, 1) & "") > 0) /=
           (Index (Digit_Array (Idx), Element (EG_Seg, 2) & "") > 0)
         then
            if Index (Digit_Array (Idx), Element (EG_Seg, 1) & "") > 0 then
               Mapping_Table.Include (Element (EG_Seg, 1), 'g');
               Mapping_Table.Include (Element (EG_Seg, 2), 'e');
            else
               Mapping_Table.Include (Element (EG_Seg, 2), 'g');
               Mapping_Table.Include (Element (EG_Seg, 1), 'e');
            end if;
            exit;
         end if;
      end loop;

      return Mapping_Table;
   end Get_Corresponding_Segments;
   use Segments_To_Digit_Maps;

   --  Given a Value, it retrieve the original value according to Mapping_Table.
   --  @param Mapping_Table The mapping table that correspond mixed segment signal with the good segment signal
   --  @param Value The digit te retrieve
   --  @returns Return the resolved digit
   function Digit_Reconstruction (Mapping_Table : Mapping_Table_Maps.Map; Value : Digit) return Digit;

   --------------------------
   -- Digit_Reconstruction --
   --------------------------

   function Digit_Reconstruction (Mapping_Table : Mapping_Table_Maps.Map; Value : Digit) return Digit is
      Result : Digit := Null_Bounded_String;
   begin
      for Char of To_String (Value) loop
         Result := Result & Mapping_Table.Element (Char);
      end loop;

      declare
         Str : String := To_String (Result);
      begin
         String_Sort (Str);
         Result := To_Bounded_String (Str);
      end;

      return Result;
   end Digit_Reconstruction;

   File                 : File_Type;
   Result               : Natural := Natural'First;
   Seven_Segment_Digit  : Map     := Empty_Map;
begin
   Get_File (File);

   if End_Of_File (File) then
      raise Program_Error with "Empty file";
   end if;

   --  Initialilze "Seven_Segment_Digit" map

   --  Length: 6
   Seven_Segment_Digit.Include (To_Bounded_String ("abcefg"), 0);

   --  Length: 2 (unique length)
   Seven_Segment_Digit.Include (To_Bounded_String ("cf"), 1);

   --  Length: 5
   Seven_Segment_Digit.Include (To_Bounded_String ("acdeg"), 2);

   --  Length: 5
   Seven_Segment_Digit.Include (To_Bounded_String ("acdfg"), 3);

   --  Length: 4 (unique length)
   Seven_Segment_Digit.Include (To_Bounded_String ("bcdf"), 4);

   --  Length: 5
   Seven_Segment_Digit.Include (To_Bounded_String ("abdfg"), 5);

   --  Length: 6
   Seven_Segment_Digit.Include (To_Bounded_String ("abdefg"), 6);

   --  Length: 3 (unique length)
   Seven_Segment_Digit.Include (To_Bounded_String ("acf"), 7);

   --  Length: 7 (unique length)
   Seven_Segment_Digit.Include (To_Bounded_String ("abcdefg"), 8);

   --  Length: 6
   Seven_Segment_Digit.Include (To_Bounded_String ("abcdfg"), 9);

   --  Get digit list
   while not End_Of_File (File) loop
      declare
         Line                       : constant String        := Get_Line (File);
         First                      : Positive               := Line'First;
         Last                       : Positive               := Line'First;
         After_Pipe                 : Boolean                := False;
         Current_Input_Digits_Index : Digits_Array_Index     := Digits_Array_Index'First;
         Current_Digit_Value        : Natural                := Natural'First;
         Current_Exponent           : Natural                := 3;
         Mapping_Table              : Mapping_Table_Maps.Map := Mapping_Table_Maps.Empty_Map;
         Current_Digit              : Digit;
         Input_Digits               : Digits_Array;
      begin
         while Last <= Line'Last loop
            if After_Pipe then
               --  Right data
               if Line (Last) = ' ' then
                  Current_Digit := Digit_Reconstruction (Mapping_Table, To_Bounded_String (Line (First .. Last - 1)));
                  Current_Digit_Value :=
                    Current_Digit_Value + Seven_Segment_Digit.Element (Current_Digit) * 10 ** Current_Exponent;
                  Current_Exponent := Current_Exponent - 1;
                  First := Last + 1;
               elsif Last = Line'Last then
                  Current_Digit := Digit_Reconstruction (Mapping_Table, To_Bounded_String (Line (First .. Last)));
                  Current_Digit_Value :=
                    Current_Digit_Value + Seven_Segment_Digit.Element (Current_Digit);
                  Result := Result + Current_Digit_Value;
               end if;

               Last := Last + 1;
            elsif Line (Last) = '|' then
               After_Pipe := True;

               Last  := Last + 2;
               First := Last;

               Digits_Sort (Input_Digits);
               Mapping_Table := Get_Corresponding_Segments (Input_Digits);
            else
               --  Left part
               if Line (Last) = ' ' then
                  declare
                     Str : String := Line (First .. Last - 1);
                  begin
                     String_Sort (Str);
                     Input_Digits (Current_Input_Digits_Index) := To_Bounded_String (Str);
                  end;
                  Current_Input_Digits_Index :=
                    (if Current_Input_Digits_Index < 10 then Current_Input_Digits_Index + 1
                     else Digits_Array_Index'First);
                  First := Last + 1;
               elsif Last = Line'Last then
                  declare
                     Str : String := Line (First .. Last);
                  begin
                     String_Sort (Str);
                     Input_Digits (Current_Input_Digits_Index) := To_Bounded_String (Str);
                  end;
                  Current_Input_Digits_Index := Current_Input_Digits_Index + 1;
                  First := Last + 1;
               end if;

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
   when others =>
      Close_If_Open (File);
      raise;
end Main;
