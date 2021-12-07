pragma Ada_2022;

with Ada.Numerics.Generic_Real_Arrays;

generic
   type Value_Type is digits <>;
   with package Matrix_Pack is new Ada.Numerics.Generic_Real_Arrays (Real => Value_Type);
package Loss is

   function AE (Target, Predict : Matrix_Pack.Real_Vector) return Matrix_Pack.Real_Vector with
     Pre => Target'Length = Predict'Length,
     Post => AE'Result'Length = Target'Length;
   
   function SE (Target, Predict : Matrix_Pack.Real_Vector) return Matrix_Pack.Real_Vector with
     Pre => Target'Length = Predict'Length,
     Post => SE'Result'Length = Target'Length;
   
end Loss;
