with Ada.Containers.Hashed_Maps,
     Ada.Execution_Time,
     Ada.Integer_Text_IO,
     Ada.Long_Long_Integer_Text_IO,
     Ada.Real_Time,
     Ada.Text_IO;

with Utils,
     Dijkstra;

procedure Main is
   use Ada.Execution_Time,
       Ada.Real_Time,
       Ada.Text_IO;
   use Utils;

   Nb_Tiles : constant := 5;

   subtype Risk_Level is Natural range 1 .. 9;

   type Location is record
      Line   : Natural;
      Column : Natural;
   end record;

   subtype Adjacent_Index is Integer range -1 .. 1;
   type Adjacent_Location is record
      Line   : Adjacent_Index;
      Column : Adjacent_Index;
   end record;

   type Adjacent is (Top, Right, Bottom, Left);
   type Adjacent_Array is array (Adjacent) of Adjacent_Location;

   --  This Hash function transform a 2 dimensional location to an unique ID using Cantor pairing enumeration.
   --  @param Loc A 2 dimensional location
   --  @returns Return the corresponding hash
   --  @link https://en.wikipedia.org/wiki/Pairing_function#Cantor_pairing_function
   function Hash (Loc : Location) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type (((Loc.Line + Loc.Column) * (Loc.Line + Loc.Column + 1) / 2) + Loc.Column));

   package Location_Maps is new Ada.Containers.Hashed_Maps (Key_Type        => Location,
                                                            Element_Type    => Risk_Level,
                                                            Hash            => Hash,
                                                            Equivalent_Keys => "=",
                                                            "="             => "=");
   use Location_Maps;

   Adjacents : constant Adjacent_Array := (Top    => (-1, 0),
                                           Right  => (0, 1),
                                           Bottom => (1, 0),
                                           Left   => (0, -1));

   function Equivalent_Keys (Left, Right : Location) return Boolean is
     (Left.Line = Right.Line and Left.Column = Right.Column);

   --  Display the location
   procedure Put (Loc : Location);

   ---------
   -- Put --
   ---------

   procedure Put (Loc : Location) is
   begin
      Put ("(");
      Ada.Integer_Text_IO.Put (Item  => Loc.Line,
                               Width => 0);
      Put (",");
      Ada.Integer_Text_IO.Put (Item  => Loc.Column,
                               Width => 0);
      Put (")");
   end Put;

   package Chiton_Dijkstra is new Dijkstra (Node            => Location,
                                            Hash            => Hash,
                                            Equivalent_Keys => Equivalent_Keys,
                                            Put             => Put,
                                            Is_Directed     => True);

   --  This heuristic compute the Manhattan distance from Current to To
   function Heuristic (Current, To : Location) return Long_Long_Natural;

   function Heuristic (Current, To : Location) return Long_Long_Natural is
   begin
      return Long_Long_Natural (abs (To.Line - Current.Line) + abs (To.Column - Current.Column));
   end Heuristic;

   use Chiton_Dijkstra;

   File                 : File_Type;
   Start_Time, End_Time : CPU_Time;
   Execution_Duration   : Time_Span;
   Array_Width,
   Array_Height         : Natural            := Natural'First;
   Result               : Long_Long_Natural  := Long_Long_Natural'First;
   Nodes                : Map;
   Edges                : Edges_Lists.Vector := Edges_Lists.Empty_Vector;
   Graph                : Graph_Access;
begin
   Get_File (File);

   --  Get all values
   while not End_Of_File (File) loop
      declare
         Str   : constant String := Get_Line (File);
         Value : Risk_Level;
         Last  : Positive;
         Current_Colum : Natural := Natural'First;
      begin
         if Array_Width = Natural'First then
            Array_Width := Str'Length;
         end if;
         for Char of Str loop
            Ada.Integer_Text_IO.Get (Char & "", Value, Last);
            Nodes.Insert ((Array_Height, Current_Colum), Value);
            Current_Colum := Current_Colum + 1;
         end loop;

         Create_Horizontal_Tiles : for Column in Array_Width .. Array_Width * Nb_Tiles - 1 loop
            Last := Nodes.Element ((Array_Height, Column - Array_Width)) + 1;
            if Last > 9 then
               Last := Last - 9;
            end if;
            Nodes.Insert ((Array_Height, Column), Last);
         end loop Create_Horizontal_Tiles;
      end;
      Array_Height := Array_Height + 1;
   end loop;

   --  Exit the program if there is no values
   if Nodes.Is_Empty then
      Close_If_Open (File);
      Put_Line ("The input file is empty.");
      return;
   end if;

   Create_Vertical_Tiles : declare
      Value : Natural;
   begin
      for Line in Array_Height .. Array_Height * Nb_Tiles - 1 loop
         for Column in 0 .. Array_Width * Nb_Tiles - 1 loop
            Value := Nodes.Element ((Line - Array_Height, Column)) + 1;
            if Value > 9 then
               Value := Value - 9;
            end if;
            Nodes.Insert ((Line, Column), Risk_Level (Value));
         end loop;
      end loop;
   end Create_Vertical_Tiles;

   Create_Graph : declare
      Current_Loc : Location;
   begin
      for Line in 0 .. Array_Height * Nb_Tiles - 1 loop
         for Column in 0 .. Array_Width * Nb_Tiles - 1 loop
            for Adjacent : Adjacent_Location of Adjacents loop
               if Line + Adjacent.Line in 0 .. Array_Height * Nb_Tiles - 1
                 and Column + Adjacent.Column in 0 .. Array_Width * Nb_Tiles - 1
               then
                  Current_Loc := (Line + Adjacent.Line, Column + Adjacent.Column);

                  Edges.Append ((
                                Start_Node => (Line, Column),
                                End_Node   => (Current_Loc.Line, Current_Loc.Column),
                                Cost       =>
                                  Long_Long_Natural (Nodes.Element ((Current_Loc.Line, Current_Loc.Column)))
                               ));
               end if;
            end loop;
         end loop;
      end loop;

      Graph := new Graph_T '(Initialize (Edges));
   end Create_Graph;

   --  Do the puzzle
   Start_Time := Ada.Execution_Time.Clock;
   Solve_Puzzle : declare
   begin
      Result := Graph.Shortest_Cost (From      => (0, 0),
                                     To        => (Array_Height * Nb_Tiles - 1, Array_Width * Nb_Tiles - 1),
                                     Heuristic => Heuristic'Access);
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
