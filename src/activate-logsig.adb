pragma Ada_2012;

with Ada.Numerics.Generic_Elementary_Functions;

package body Activate.Logsig is

   ----------
   -- Func --
   ----------

   function Func (Item : in Matrix_Pack.Real_Vector; Idx : Positive) return Value_Type is
      package My_Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions (Float_Type => Value_Type);
      use My_Elementary_Functions;
   begin
      return Result : Value_Type do
         Result := Value_Type(1.0 / (1.0 + Exp (X => -Item(Idx))));
      end return;
   end Func;

   --------------------
   -- DerivativeFunc --
   --------------------

   function DerivativeFunc (Item : in Value_Type) return Value_Type is
   begin
      return Result : Value_Type do
         Result := Value_Type(Item * (1.0 - Item));
      end return;
   end DerivativeFunc;

end Activate.Logsig;
