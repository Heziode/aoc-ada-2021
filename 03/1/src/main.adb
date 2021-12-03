with Ada.Containers.Vectors,
     Ada.Exceptions,
     Ada.Integer_Text_IO,
     Ada.Text_IO;

with Utils;

procedure Main is
   use Ada.Containers, Ada.Text_IO;
   use Utils;

   type Binary_Rate is record
      Zero     : Natural;
      One      : Natural;
   end record;

   type Binary is mod 2;

   package Binary_Rate_Vectors is new Ada.Containers.Vectors (Natural, Binary_Rate);
   use Binary_Rate_Vectors;

   File    : File_Type;
   Gamma   : Vector := Empty_Vector;
   Epsilon : Vector := Empty_Vector;
begin
   Get_File (File);

   --  Get all values
   while not End_Of_File (File) loop
      declare
         procedure Process (Vec : out Vector; Index : Natural; Bit : Binary);

         procedure Process (Vec : out Vector; Index : Natural; Bit : Binary) is
         begin
            if Count_Type (Index) > Vec.Length then
               Vec.Append (New_Item => (Zero => (if Bit = 0 then 1 else 0), One => Natural (Bit)));
            else
               declare
                  Temp_Rate : Binary_Rate := Vec.Element (Index - 1);
               begin
                  if Bit = 0 then
                     Temp_Rate.Zero := Temp_Rate.Zero + 1;
                  else
                     Temp_Rate.One := Temp_Rate.One + 1;
                  end if;

                  Vec.Replace_Element (Index - 1, Temp_Rate);
               end;
            end if;
         end Process;

         Line : constant String := Get_Line (File);
      begin
         Split_Value : for Index in Line'Range loop
            Solve_Puzzle : declare
               Bit : constant Binary := Binary'Value (Line (Index) & "");
            begin
               Process (Gamma, Index, Bit);
               Process (Epsilon, Index, Bit + 1);
            end Solve_Puzzle;
         end loop Split_Value;
      end;
   end loop;

   declare
      function Compute_Vector_Value (Vec : Vector) return Natural;

      function Compute_Vector_Value (Vec : Vector) return Natural is
         Exp    : Natural := 0;
         Result : Natural := 0;

      begin
         for Elt : Binary_Rate of reverse Vec loop
            if Elt.One > Elt.Zero then
               Result := Result + 2**Exp;
            end if;
            Exp := Exp + 1;
         end loop;

         return Result;
      end Compute_Vector_Value;

      Gamma_Result   : constant Natural := Compute_Vector_Value (Gamma);
      Epsilon_Result : constant Natural := Compute_Vector_Value (Epsilon);

   begin
      --  Print the result
      Put ("Result: ");
      Ada.Integer_Text_IO.Put (Item  => Gamma_Result * Epsilon_Result,
                               Width => 0);
      New_Line;
   end;

   Close_If_Open (File);

exception
   when Occur : others =>
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (Occur));

      Close_If_Open (File);
end Main;
