generic
   type Element is (<>);
   with function "+" (Left, Right : Element) return Element is <>;
   with function "-" (Left, Right : Element) return Element is <>;
package Coordinates_2D is
   type Coordinate_2D is record
      Line, Column : Element;
   end record;

   function "+" (Left, Right : Coordinate_2D) return Coordinate_2D is
     ((Line   => Left.Line + Right.Line,
       Column => Left.Column + Right.Column));

   function "-" (Left, Right : Coordinate_2D) return Coordinate_2D is
     ((Line   => Left.Line - Right.Line,
       Column => Left.Column - Right.Column));
end Coordinates_2D;
