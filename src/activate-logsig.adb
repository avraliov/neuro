pragma Ada_2012;

with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body Activate.Logsig is

   ----------
   -- Func --
   ----------

   function Func (Item : in Matrix_Pack.Real_Vector; Idx : Positive) return Value_Type is
   begin
      return Result : Value_Type do
         Result := Value_Type(1.0 / (1.0 + Exp (X => Float(-Item(Idx)))));
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
