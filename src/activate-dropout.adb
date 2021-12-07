pragma Ada_2022;

with Ada.Numerics.Discrete_Random;

package body Activate.Dropout is

   ----------
   -- Func --
   ----------

   function Func (Item : in Matrix_Pack.Real_Vector; Idx : Positive) return Value_Type is
      subtype Mask_Rnd is Natural range 0 .. 1;
      package Random is new Ada.Numerics.Discrete_Random (Result_Subtype => Mask_Rnd);
      G : Random.Generator;
      Mask : Mask_Rnd;
   begin
      return Result : Value_Type do
         Random.Reset (G);
         Mask := Random.Random (Gen => G);
         Result := Value_Type (1.0 / (1.0 - Dropout_Rate) * Value_Type (Mask) * Item (Idx));
      end return;
   end Func;

   --------------------
   -- DerivativeFunc --
   --------------------

   function DerivativeFunc (Item : in Value_Type) return Value_Type is
   begin
      return Result : Value_Type := 1.0 do
         null;
      end return;
   end DerivativeFunc;


end Activate.Dropout;
