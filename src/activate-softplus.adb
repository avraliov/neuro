pragma Ada_2022;

with Ada.Numerics.Generic_Elementary_Functions;

package body Activate.Softplus is
   package Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions (Float_Type => Value_Type);
   ----------
   -- Func --
   ----------

   function Func (Item : in Matrix_Pack.Real_Vector; Idx : Positive) return Value_Type is
   begin
      return Result : Value_Type do
         Result := Value_Type (Elementary_Functions.Log (X => (1.0 + Elementary_Functions.Exp (Item (Idx)))));
      end return;
   end Func;

   --------------------
   -- DerivativeFunc --
   --------------------

   function DerivativeFunc (Item : in Value_Type) return Value_Type is
   begin
      return Result : Value_Type do
         Result := Value_Type(1.0 / (1.0 + Elementary_Functions.Exp (X => -Item)));
      end return;
   end DerivativeFunc;


end Activate.Softplus;
