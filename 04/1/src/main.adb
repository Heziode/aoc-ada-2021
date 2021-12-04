with Ada.Containers.Hashed_Maps,
     Ada.Containers.Unbounded_Synchronized_Queues,
     Ada.Containers.Synchronized_Queue_Interfaces,
     Ada.Containers.Vectors,
     Ada.Exceptions,
     Ada.Integer_Text_IO,
     Ada.Text_IO;

with Utils;

procedure Main is
   use Ada.Containers, Ada.Text_IO;
   use Utils;

   type Bingo_Range is range 1 .. 5;
   subtype Bingo_Values is Natural range 0 .. 99;
   package Bingo_Values_IO is new Ada.Text_IO.Integer_IO (Bingo_Values);
   use Bingo_Values_IO;

   type Grid_Element is record
      Value  : Natural;
      Marked : Boolean;
   end record;

   type Lookup_Item is record
      Board_Id : Natural;
      Line     : Bingo_Range;
      Column   : Bingo_Range;
   end record;

   package Lookup_Item_Vectors is new Ada.Containers.Vectors (Natural, Lookup_Item);

   function Bingo_Range_Hash (Elt : Bingo_Values) return Ada.Containers.Hash_Type is (Ada.Containers.Hash_Type (Elt));

   package Lookup_Map is new Ada.Containers.Hashed_Maps (Key_Type        => Bingo_Values,
                                                         Element_Type    => Lookup_Item_Vectors.Vector,
                                                         Hash            => Bingo_Range_Hash,
                                                         Equivalent_Keys => "=",
                                                         "="             => Lookup_Item_Vectors."=");
   use Lookup_Map;

   --  Given a Lookup map, it add an element to the specified value.
   --  @param Lookup Lookup map to update
   --  @param Value Id in the map "Lookup"
   --  @param Board_Id Property of the Lookup_Item that will be added to the vector at id "Value"
   --  @param Line Property of the Lookup_Item that will be added to the vector at id "Value"
   --  @param Column Property of the Lookup_Item that will be added to the vector at id "Value"
   procedure Update_Lookup (Lookup : in out Map; Value : Bingo_Values; Board_Id : Natural; Line, Column : Bingo_Range);

   -------------------
   -- Update_Lookup --
   -------------------

   procedure Update_Lookup (Lookup : in out Map; Value : Bingo_Values; Board_Id : Natural; Line, Column : Bingo_Range)
   is
      use Lookup_Item_Vectors;

      Item : constant Lookup_Item       := (Board_Id, Line, Column);
      Vec  : Lookup_Item_Vectors.Vector := Lookup.Element (Value);
   begin
      Vec.Append (Item);
      Lookup.Replace_Element (Lookup.Find (Value), Vec);
   end Update_Lookup;

   type Grid is array (Bingo_Range, Bingo_Range) of Grid_Element;

   package Grid_Vectors is new Ada.Containers.Vectors (Natural, Grid);
   use Grid_Vectors;

   package Bingo_Values_Queue_Interfaces is
     new Ada.Containers.Synchronized_Queue_Interfaces
       (Element_Type => Bingo_Values);

   package Bingo_Balls is new Ada.Containers.Unbounded_Synchronized_Queues
     (Queue_Interfaces => Bingo_Values_Queue_Interfaces);
   use Bingo_Balls;

   --  Tell if there is a bingo.
   --  @param Board The board to check
   --  @param Line The marked line
   --  @param Column The marked column
   --  @returns Return True if there is a bingo, False otherwise
   function Is_Bingo (Board : Grid; Line, Column : Bingo_Range) return Boolean;

   --------------
   -- Is_Bingo --
   --------------

   function Is_Bingo (Board : Grid; Line, Column : Bingo_Range) return Boolean is
      Line_Bingo, Column_Bingo : Boolean := True;
   begin
      for Index in Bingo_Range loop
         Line_Bingo   := Line_Bingo and Board (Index, Column).Marked;
         Column_Bingo := Column_Bingo and Board (Line, Index).Marked;

         if not Line_Bingo and not Column_Bingo then
            return False;
         end if;
      end loop;

      return True;
   end Is_Bingo;

   --  Compute the value of a board according to the exercice.
   --  Formula: "sum of all unmarked numbers" x Current_Value
   --  @param Board the winning board
   --  @param Current_Value The last number called
   --  @returns Returns the result of the calculation
   function Compute_Board_Value (Board : Grid; Current_Value : Bingo_Values) return Natural;

   -------------------------
   -- Compute_Board_Value --
   -------------------------

   function Compute_Board_Value (Board : Grid; Current_Value : Bingo_Values) return Natural is
      Sum : Natural := Natural'First;
   begin
      for Line in Board'Range (1) loop
         for Column in Board'Range (2) loop
            if not Board (Line, Column).Marked then
               Sum := Sum + Board (Line, Column).Value;
            end if;
         end loop;
      end loop;

      return Sum * Current_Value;
   end Compute_Board_Value;

   File     : File_Type;
   Ball_Box : Queue;
   Boards   : Vector := Empty_Vector;
   Lookup   : Map    := Empty_Map;
begin
   Get_File (File);

   if End_Of_File (File) then
      raise Program_Error with "Empty file";
   end if;

   Fill_Ball_Box : declare
      Line       : constant String := Get_Line (File);
      First      : Positive        := Line'First;
      Last       : Positive        := Line'First;
      Last_Index : Positive        := Line'First;
      Value      : Bingo_Values;
   begin
      while Last <= Line'Last loop
         if Line (Last .. Last) = "," then
            Get (Line (First .. Last - 1), Value, Last_Index);

            Ball_Box.Enqueue (Value);
            Lookup.Insert (Value, Lookup_Item_Vectors.Empty_Vector);

            First := Last + 1;
         elsif Last = Line'Last then
            Get (Line (First .. Line'Last), Value, Last_Index);

            Ball_Box.Enqueue (Value);
            Lookup.Insert (Value, Lookup_Item_Vectors.Empty_Vector);

            First := Last + 1;
         end if;
         Last := Last + 1;
      end loop;
   end Fill_Ball_Box;

   --  Get all boards
   Load_Boards : declare
      Line_Index   : Bingo_Range := Bingo_Range'First;
      Column_Index : Bingo_Range := Bingo_Range'First;
      Value        : Bingo_Values;
      Board        : Grid;
   begin
      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
            Last : Positive        := Line'First;
         begin
            while Last < Line'Last loop
               Get (Line (Last .. Line'Last), Value, Last);

               Board (Line_Index, Column_Index) := (Value, False);
               Update_Lookup (Lookup   => Lookup,
                              Value    => Value,
                              Board_Id => Natural (Boards.Length),
                              Line     => Line_Index,
                              Column   => Column_Index);

               if Column_Index = Bingo_Range'Last then
                  Column_Index := Bingo_Range'First;

                  if Line_Index = Bingo_Range'Last then
                     Line_Index := Bingo_Range'First;

                     Boards.Append (Board);
                  else
                     Line_Index := Line_Index + 1;
                  end if;
               else
                  Column_Index := Column_Index + 1;
               end if;

               Last := Last + 1;
            end loop;
         end;
      end loop;
   end Load_Boards;

   Solve_Puzzle : declare
      Current_Value : Bingo_Values;
      Current_Found : Lookup_Item_Vectors.Vector;
   begin
      Solve : loop
         Ball_Box.Dequeue (Current_Value);
         Current_Found := Lookup.Element (Current_Value);

         --  The case where a number is not found in any boards
         if Current_Found.Is_Empty then
            goto Continue;
         end if;

         --  For each times the current value is on a board
         for Current_Lookup of Current_Found loop
            declare
               Current_Board : Grid := Boards.Element (Current_Lookup.Board_Id);
            begin
               Current_Board (Current_Lookup.Line, Current_Lookup.Column).Marked := True;
               Boards.Replace_Element (Current_Lookup.Board_Id, Current_Board);

               if Is_Bingo (Current_Board, Current_Lookup.Line, Current_Lookup.Column) then
                  Put ("Result: ");
                  Ada.Integer_Text_IO.Put (Compute_Board_Value (Current_Board, Current_Value), Width => 0);
                  New_Line;
                  exit Solve;
               end if;

            end;
         end loop;

         <<Continue>>
         exit Solve when Ball_Box.Current_Use = 0;
      end loop Solve;
   end Solve_Puzzle;

   Close_If_Open (File);

exception
   when Occur : others =>
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (Occur));

      Close_If_Open (File);
end Main;
