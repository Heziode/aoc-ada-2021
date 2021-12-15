with Ada.Containers.Hashed_Sets,
     Ada.Containers.Synchronized_Queue_Interfaces,
     Ada.Containers.Unbounded_Priority_Queues,
     Ada.Text_IO;

package body Dijkstra is
   use Ada.Containers;

   Nil_Node : Node;
   pragma Unmodified (Nil_Node);

   type Node_Element_Info is record
      This      : Node;
      Parent    : Node := Nil_Node;
      Cost      : Long_Long_Natural;
      Heuristic : Long_Long_Natural;
   end record;

   function Hash (Elt : Node_Element_Info) return Hash_Type is (Hash (Elt.This));

   function Equivalent_Keys (Left, Right : Node_Element_Info) return Boolean is
     (Equivalent_Keys (Left.This, Right.This));

   package Node_Sets is new Ada.Containers.Hashed_Sets (Element_Type        => Node_Element_Info,
                                                        Hash                => Hash,
                                                        Equivalent_Elements => Equivalent_Keys,
                                                        "="                 => Equivalent_Keys);

   package Node_Element_Info_Interface is new Ada.Containers.Synchronized_Queue_Interfaces (Node_Element_Info);

   function Get_Priority (Elt : Node_Element_Info) return Node_Element_Info is (Elt);

   --  Used to get the priority between Left and Right into the priority queue.
   function "<" (Left, Right : Node_Element_Info) return Boolean;

   function "<" (Left, Right : Node_Element_Info) return Boolean is
   begin
      if Left.Cost < Right.Cost then
         return True;
      elsif Left.Cost > Right.Cost then
         return False;
      end if;

      return Left.Heuristic < Right.Heuristic;
   end "<";

   package Node_Priority_Queues is
     new Ada.Containers.Unbounded_Priority_Queues (Node_Element_Info_Interface,
                                                   Queue_Priority => Node_Element_Info,
                                                   Get_Priority => Get_Priority,
                                                   Before => "<");

   --  Dijkstra's algorithm optimized using priority queue.
   --  @see https://en.wikipedia.org/wiki/Dijkstra's_algorithm#Practical_optimizations_and_infinite_graphs
   procedure Uniform_Cost_Search (Graph     : Graph_T;
                                  From, To  : Node;
                                  Heuristic : Heuristic_Access;
                                  Explored  : in out Node_Sets.Set;
                                  Cost      : out Long_Long_Natural);

   -------------------------
   -- Uniform_Cost_Search --
   -------------------------

   procedure Uniform_Cost_Search (Graph     : Graph_T;
                                  From, To  : Node;
                                  Heuristic : Heuristic_Access;
                                  Explored  : in out Node_Sets.Set;
                                  Cost      : out Long_Long_Natural)
   is
      use Node_Sets;
      use Node_Priority_Queues;

      Current_Node_Element : Node_Element_Info := (From, Nil_Node, 0, Heuristic (From, To));
      Current_Node         : Node;
      Neighbors            : Vertices_Cost_Maps.Map;
      Frontier             : Queue;
   begin
      Frontier.Enqueue (Current_Node_Element);

      loop
         if Frontier.Current_Use = 0 then
            raise Not_Path;
         end if;

         Frontier.Dequeue (Current_Node_Element);

         if Equivalent_Keys (Current_Node_Element.This, To) then
            Explored.Include (Current_Node_Element);
            Cost := Current_Node_Element.Cost;
            return;
         end if;

         if Explored.Contains (Current_Node_Element) then
            goto Continue_Next_Vertex;
         end if;

         Explored.Include (Current_Node_Element);

         Neighbors := Graph.Vertices.Element (Current_Node_Element.This).Neighbors;
         for Curs in Neighbors.Iterate loop
            Current_Node := Vertices_Cost_Maps.Key (Curs);

            if not Explored.Contains ((Current_Node, Nil_Node, 0, 0)) then
               Frontier.Enqueue ((Current_Node,
                                 Current_Node_Element.This,
                                 Current_Node_Element.Cost + Vertices_Cost_Maps.Element (Curs),
                                 Heuristic (Current_Node, To)));
            end if;
         end loop;

         <<Continue_Next_Vertex>>
      end loop;
   end Uniform_Cost_Search;

   ----------------
   -- Initialize --
   ----------------

   function Initialize (Edges : Edges_Lists.Vector) return Graph_T is
      use Vertices_Maps;

      Result : Map;
      Default_Vertex_Info : Vertex_Info;
      Current_Vertex_Info : Vertex_Info;
   begin
      for Elt : Edge of Edges loop
         if not Result.Contains (Elt.Start_Node) then
            Result.Insert (Key      => Elt.Start_Node,
                           New_Item => Default_Vertex_Info);
         end if;
         Current_Vertex_Info := Result.Element (Elt.Start_Node);
         Current_Vertex_Info.Neighbors.Insert (Elt.End_Node, Elt.Cost);
         Result.Include (Elt.Start_Node, Current_Vertex_Info);

         if not Is_Directed then
            if not Result.Contains (Elt.End_Node) then
               Result.Insert (Key      => Elt.End_Node,
                              New_Item => Default_Vertex_Info);
            end if;
            Current_Vertex_Info := Result.Element (Elt.End_Node);
            Current_Vertex_Info.Neighbors.Insert (Elt.Start_Node, Elt.Cost);
            Result.Include (Elt.End_Node, Current_Vertex_Info);
         end if;
      end loop;
      return (Vertices => Result);
   end Initialize;

   -------------------
   -- Shortest_Path --
   -------------------

   function Shortest_Path
     (Graph : Graph_T; From, To : Node; Heuristic : Heuristic_Access := Default_Heuristic'Access) return Path_List.List
   is
      use Path_List;
      use Node_Sets;

      Result   : Path_List.List;
      Explored : Set  := Empty_Set;
      Cost     : Long_Long_Natural;
   begin
      Uniform_Cost_Search (Graph     => Graph,
                           From      => From,
                           To        => To,
                           Heuristic => Heuristic,
                           Explored  => Explored,
                           Cost      => Cost);

      declare
         Curs : Node_Sets.Cursor := Explored.Find ((To, Nil_Node, 0, 0));
         Info : Node_Element_Info;
      begin
         while Has_Element (Curs) loop
            Info := Element (Curs);
            Result.Prepend (Info.This);
            exit when Info.Parent = Nil_Node;
            Curs := Explored.Find ((Info.Parent, Nil_Node, 0, 0));
         end loop;
      end;

      return Result;
   end Shortest_Path;

   -------------------
   -- Shortest_Cost --
   -------------------

   function Shortest_Cost (Graph : Graph_T; From, To : Node; Heuristic : Heuristic_Access := Default_Heuristic'Access)
                           return Long_Long_Natural
   is
      use Node_Sets;
      Explored : Set := Empty_Set;
      Cost     : Long_Long_Natural;
   begin
      Uniform_Cost_Search (Graph     => Graph,
                           From      => From,
                           To        => To,
                           Heuristic => Heuristic,
                           Explored  => Explored,
                           Cost      => Cost);
      return Cost;
   end Shortest_Cost;

   ---------
   -- Put --
   ---------

   procedure Put (Graph : Graph_T) is
      use Ada.Text_IO;
      use Vertices_Maps;
      Current_Node : Node;
      Neighbors    : Vertices_Cost_Maps.Map;
   begin
      for Curs in Graph.Vertices.Iterate loop
         Current_Node := Key (Curs);
         Put (Current_Node);
         Put (":");
         New_Line;

         Neighbors := Element (Curs).Neighbors;
         for Neighbor_Curs in Neighbors.Iterate loop
            Put ("   - ");
            Put (Vertices_Cost_Maps.Key (Neighbor_Curs));
            Put (Long_Long_Natural'Image (Vertices_Cost_Maps.Element (Neighbor_Curs)));
            New_Line;
         end loop;
      end loop;
   end Put;

end Dijkstra;
