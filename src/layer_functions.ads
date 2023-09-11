pragma Ada_2012;

with Ada.Numerics.Generic_Real_Arrays;

generic
   type Value_Type is digits <>;
   with package Matrix_Pack is new Ada.Numerics.Generic_Real_Arrays (Real => Value_Type);
package Layer_Functions is

   type Pre_Post_ProcType is access procedure (Values_Arr : in out Matrix_Pack.Real_Vector);
   
   generic
      Rate : Value_Type := 0.5;
   procedure Dropout (Values_Arr : in out Matrix_Pack.Real_Vector);

end Layer_Functions;
