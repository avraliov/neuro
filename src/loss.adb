pragma Ada_2022;
pragma Extensions_Allowed (On);

package body Loss is

   function AE (Target, Predict : Matrix_Pack.Real_Vector) return Matrix_Pack.Real_Vector is
      use type Matrix_Pack.Real_Vector;
   begin
      return Result : Matrix_Pack.Real_Vector (Target'Range) do
         Result := Predict - Target;
--         Result := Target - Predict;
         Result := [for I of @ => abs(I)];
      end return;
   end AE;

   function SE (Target, Predict : Matrix_Pack.Real_Vector) return Matrix_Pack.Real_Vector is
      use type Matrix_Pack.Real_Vector;
   begin
      return Result : Matrix_Pack.Real_Vector (Target'Range) do
         Result := Predict - Target;
--         Result := Target - Predict;
         Result := [for I of @ => I ** 2];
      end return;
   end SE;

end Loss;
