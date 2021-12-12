with Ada.Containers.Indefinite_Hashed_Maps,
     Ada.Containers.Indefinite_Hashed_Sets,
     Ada.Containers.Indefinite_Vectors,
     Ada.Containers.Synchronized_Queue_Interfaces,
     Ada.Containers.Unbounded_Synchronized_Queues,
     Ada.Execution_Time,
     Ada.Integer_Text_IO,
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

   subtype Big_Cave_Character is Character range 'A' .. 'Z';
   subtype Small_Cave_Character is Character range 'a' .. 'z';

   package Cave_Vectors is new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                                                  Element_Type => String,
                                                                  "="          => "=");
   use Cave_Vectors;

   type Cave_Kind is (K_Start, K_Big, K_Small, K_End);
   Start_Name : constant String := "start";
   End_Name   : constant String := "end";

   --  Given the name of a node, it retreive the corresponding kind
   function Get_Kind (Name : String) return Cave_Kind;

   --------------
   -- Get_Kind --
   --------------

   function Get_Kind (Name : String) return Cave_Kind is
   begin
      if Name = Start_Name then
         return K_Start;
      elsif Name = End_Name then
         return K_End;
      elsif Name (Name'First) in Big_Cave_Character then
         return K_Big;
      elsif Name (Name'First) in Small_Cave_Character then
         return K_Small;
      end if;
      raise Constraint_Error with "Unexpected cave name: " & Name;
   end Get_Kind;

   type Cave_Info (Length : Positive) is record
      Name : String (1 .. Length);
      Kind : Cave_Kind;
      Adjacent : Vector;
   end record;

   package Cave_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Cave_Info,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   package String_Sets is new Ada.Containers.Indefinite_Hashed_Sets (Element_Type        => String,
                                                                     Hash                => Ada.Strings.Hash,
                                                                     Equivalent_Elements => "=",
                                                                     "="                 => "=");

   type Cave_Explorer is record
      Name                            : Unbounded_String;
      Already_Visited_Small_Cave_Name : Unbounded_String;
      Already_Visited_Small_Cave      : String_Sets.Set;
   end record;

   package Explored_Cave_Interfaces is
     new Ada.Containers.Synchronized_Queue_Interfaces
       (Element_Type => Cave_Explorer);

   package Explored_Cave_Queue is new Ada.Containers.Unbounded_Synchronized_Queues
     (Queue_Interfaces => Explored_Cave_Interfaces);

   File                   : File_Type;
   Start_Time, End_Time   : CPU_Time;
   Execution_Duration     : Time_Span;
   File_Is_Empty          : Boolean       := True;
   Result                 : Natural       := Natural'First;
   Nodes                  : Cave_Maps.Map := Cave_Maps.Empty_Map;
begin
   Get_File (File);

   --  Get all values
   begin
      while not End_Of_File (File) loop
         declare
            Str             : constant String  := Get_Line (File);
            Separator_Index : constant Integer :=
              Ada.Strings.Fixed.Index (Source => Str (1 .. Str'Last), Pattern => "-");
            Node_1          : constant String  := Str (1 .. Separator_Index - 1);
            Node_2          : constant String  := Str (Separator_Index + 1 .. Str'Last);
         begin
            declare
               Cave_Node_1 : Cave_Info := (Length => Node_1'Length,
                                           Name => Node_1,
                                           Kind => Get_Kind (Node_1),
                                           Adjacent => Empty_Vector);

               Cave_Node_2 : Cave_Info := (Length => Node_2'Length,
                                           Name => Node_2,
                                           Kind => Get_Kind (Node_2),
                                           Adjacent => Empty_Vector);
            begin
               if Nodes.Contains (Node_1) then
                  Cave_Node_1 := Nodes.Element (Node_1);
               end if;
               if Nodes.Contains (Node_2) then
                  Cave_Node_2 := Nodes.Element (Node_2);
               end if;

               Cave_Node_1.Adjacent.Append (Node_2);
               Cave_Node_2.Adjacent.Append (Node_1);

               Nodes.Include (Node_1, Cave_Node_1);
               Nodes.Include (Node_2, Cave_Node_2);
            end;
            File_Is_Empty := False;
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
      use Ada.Containers, Explored_Cave_Queue;
      Current_Cave        : Cave_Explorer;
      Cave_Explorer_Queue : Explored_Cave_Queue.Queue;
   begin
      Current_Cave.Name := To_Unbounded_String (Start_Name);
      Current_Cave.Already_Visited_Small_Cave_Name := Null_Unbounded_String;
      Current_Cave.Already_Visited_Small_Cave.Include (Start_Name);

      Cave_Explorer_Queue.Enqueue (Current_Cave);

      while Cave_Explorer_Queue.Current_Use > 0 loop
         Cave_Explorer_Queue.Dequeue (Current_Cave);

         if Nodes.Element (To_String (Current_Cave.Name)).Kind = K_End then
            Result := Result + 1;
            goto Continue;
         end if;

         --  Get all adjacent nodes (cave) to explore them
         for Node of Nodes.Element (To_String (Current_Cave.Name)).Adjacent loop

            if not Current_Cave.Already_Visited_Small_Cave.Contains (Node) then
               declare
                  Next_Already_Visited_Small_Cave : String_Sets.Set :=
                    String_Sets.Copy (Current_Cave.Already_Visited_Small_Cave);
               begin
                  if Get_Kind (Node) = K_Small then
                     Next_Already_Visited_Small_Cave.Include (Node);
                  end if;
                  Cave_Explorer_Queue.Enqueue
                    ((Name                            => To_Unbounded_String (Node),
                      Already_Visited_Small_Cave_Name => Current_Cave.Already_Visited_Small_Cave_Name,
                      Already_Visited_Small_Cave      => Next_Already_Visited_Small_Cave));
               end;
            end if;
         end loop;

         <<Continue>>
      end loop;
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
