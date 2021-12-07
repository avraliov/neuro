pragma Ada_2022;

generic
package Net.Utils is

   procedure Save_Waights (This : Net; Name : String; Layer_Num : Positive);
   -- Saves waights of layer(Layer_Num) to file (Name)

end Net.Utils;
