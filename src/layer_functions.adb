pragma Ada_2012;

with Ada.Numerics.Float_Random;

package body Layer_Functions is

   procedure Dropout (Values_Arr : in out Matrix_Pack.Real_Vector; Rate : Value_Type) is
      subtype Mask_Rnd is Value_Type range 0.0 .. 1.0;
      package Random renames Ada.Numerics.Float_Random;
      G : Random.Generator;
      Mask : array (Values_Arr'Range) of Mask_Rnd := (others => Mask_Rnd'First);
      Tmp : Mask_Rnd := Mask_Rnd'First;
  begin
      Random.Reset (G);
      for I of Mask loop
         Tmp := Value_Type(Random.Random (Gen => G));
         I := (if Tmp <= Rate then 0.0 else 1.0);
      end loop;
      for Idx in Values_Arr'Range loop
         Values_Arr (Idx) := Values_Arr (Idx) * (1.0 / (1.0 - Rate) * Mask(Idx));
      end loop;
--         Result := Value_Type (1.0 / (1.0 -Rate) * Mask);
   end Dropout;
     

end Layer_Functions;
