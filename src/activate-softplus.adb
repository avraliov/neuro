pragma Ada_2022;

with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body Activate.Softplus is

   ----------
   -- Func --
   ----------

   function Func (Item : in Matrix_Pack.Real_Vector; Idx : Positive) return Value_Type is
   begin
      return Result : Value_Type do
         Result := Value_Type (Ada.Numerics.Elementary_Functions.Log (X => (1.0 + Ada.Numerics.Elementary_Functions.Exp (Float(Item (Idx))))));
      end return;
   end Func;

   --------------------
   -- DerivativeFunc --
   --------------------

   function DerivativeFunc (Item : in Value_Type) return Value_Type is
   begin
      return Result : Value_Type := 0.0 do
         Result := Value_Type ((Ada.Numerics.Elementary_Functions."**" (Ada.Numerics.E, Float (Item))) /
                               (1.0 + (Ada.Numerics.Elementary_Functions.Exp (Float (Item)))));
      end return;
   end DerivativeFunc;


end Activate.Softplus;
