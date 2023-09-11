pragma Ada_2022;

generic
package Net.Utils is
   use Layer_Package.Matrix_Pack;

   procedure Save_Waights (This : Net; Name : String; Layer_Num : Positive);
   -- Saves waights of layer(Layer_Num) to file (Name)

   function Get_Weights (This : in Net; Layer_Num : Positive) return Real_Matrix with
     Post => Get_Weights'Result'Length (1) /= 0 and then
     Get_Weights'Result'Length (2) /= 0;
   -- Get waights of layer(Layer_Num)

end Net.Utils;
