pragma Ada_2012;

with Ada.Numerics.Float_Random;

package body Abstract_Layer.Dropout is

   procedure Reset_Mask ( This : in out Layer_Dropout;  Rate : Value_Type) is 
      package Random renames Ada.Numerics.Float_Random;
      G : Random.Generator;
      Tmp : Mask_Rnd := Mask_Rnd'First;
   begin
      Random.Reset (G);
      for I in This.Layer_Mask'Range loop
         Tmp := Value_Type (Random.Random (Gen => G));
         This.Layer_Mask(I) := (if Tmp <= Rate then 0.0 else 1.0);
      end loop;
   end Reset_Mask;
   
end Abstract_Layer.Dropout;
