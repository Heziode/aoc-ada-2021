with Ada.Containers.Doubly_Linked_Lists,
     Ada.Containers.Hashed_Maps,
     Ada.Containers.Vectors;

with Utils; --  For Long_Long_Natural type

generic
   type Node is private;

   with function Hash (Coord : Node) return Ada.Containers.Hash_Type is <>;

   with function Equivalent_Keys (Left, Right : Node) return Boolean is <>;

   with procedure Put (Elt : Node);

   Is_Directed : Boolean := True;
package Dijkstra is
   use Utils;

   type Graph_T is tagged limited private;
   type Graph_Access is access all Graph_T;

   type Edge is record
      Start_Node : Node;
      End_Node   : Node;
      Cost       : Long_Long_Natural;
   end record;

   --  This exception is raised when there is no path between two nodes
   Not_Path : exception;

--     overriding function "=" (Left, Right : Edge) return Boolean is
--       (Equivalent_Keys (Left.Start_Node, Right.Start_Node)
--        and Equivalent_Keys (Left.End_Node, Right.End_Node)
--        and Left.Cost = Right.Cost);

   package Edges_Lists is new Ada.Containers.Vectors (Index_Type   => Long_Long_Natural,
                                                      Element_Type => Edge,
                                                      "="          => "=");

--     package Node_Lists is new Ada.Containers.Vectors (Index_Type   => Long_Long_Natural,
--                                                       Element_Type => Node,
--                                                       "="          => Equivalent_Keys);

   package Path_List is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Node,
                                                                "="          => Equivalent_Keys);

   --  Heuristict to be used with Dijkstra (so, transform them more like A*) to imporve the graph exploration.
   --  @param Current Current vertex where the exploration is
   --  @param To the vertex to reach
   --  @return Return the value of the estimation. It is used in the priority queue, so lower value is better.
   type Heuristic_Access is access function (Current, To : Node) return Long_Long_Natural;

   --  This heuristic is means that the algorithm do not make an a priori estimation. So this is a pure Dijkstra's.
   --
   --  @note
   --    The use of Current and To in the expression is to prevent "Unreferenced" warning.
   --    Indeed, before GNAT 202X, we cannot use "pragma Unreferenced" (it is logic because outside of declaration) nor
   --    the aspect form. The support of aspect form was introduce in GNAT 202X and needs "-gnat2022" switch.
   --  @see AdaCore GNAT Reference Manual > 3. Implementation Defined Aspects > Aspect Unreferenced
   function Default_Heuristic (Current, To : Node) return Long_Long_Natural is
     (if Current = Current and To = To then 0 else 0);

   --  Constructor of Graph_T
   function Initialize (Edges : Edges_Lists.Vector) return Graph_T;

   --  Compute the shortest path between From and To using Dijkstra algorithm.
   --  It returns a Doubly_Linked_Lists of Node where the begining of this list is the node From and the end is To.
   function Shortest_Path (Graph : Graph_T; From, To : Node; Heuristic : Heuristic_Access := Default_Heuristic'Access)
                           return Path_List.List;

   --  Compute the shortest path between From and To using Dijkstra algorithm.
   --  It only returns the shortest cost.
   function Shortest_Cost (Graph : Graph_T; From, To : Node; Heuristic : Heuristic_Access := Default_Heuristic'Access)
                           return Long_Long_Natural;

   --  To display the graph like:
   --  Node_A:
   --     - Node_x (from Node_A -> Node_x)
   --     - Node_y (from Node_A -> Node_y)
   --  Node_B:
   --     - Node_x (from Node_B -> Node_x)
   procedure Put (Graph : Graph_T);

private
   package Vertices_Cost_Maps is new Ada.Containers.Hashed_Maps (Key_Type        => Node,
                                                                 Element_Type    => Long_Long_Natural,
                                                                 Hash            => Hash,
                                                                 Equivalent_Keys => Equivalent_Keys,
                                                                 "="             => "=");

   type Vertex_Info is record
      Cumulated_Cost : Long_Long_Natural      := Long_Long_Natural'Last;
      Visited        : Boolean                := False;
      Neighbors      : Vertices_Cost_Maps.Map := Vertices_Cost_Maps.Empty_Map;
   end record;

   package Vertices_Maps is new Ada.Containers.Hashed_Maps (Key_Type        => Node,
                                                            Element_Type    => Vertex_Info,
                                                            Hash            => Hash,
                                                            Equivalent_Keys => Equivalent_Keys,
                                                            "="             => "=");

   type Graph_T is tagged limited record
      Vertices : Vertices_Maps.Map;
   end record;
end Dijkstra;
