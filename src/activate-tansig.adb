pragma Ada_2012;

with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body Activate.Tansig is

   ----------
   -- Func --
   ----------

   function Func (Item : in Matrix_Pack.Real_Vector; Idx : Positive) return Value_Type is
   begin
      return Result : Value_Type do
         Result := Value_Type(Tanh(X => Float(Item(Idx))));
      end return;
   end Func;

   --------------------
   -- DerivativeFunc --
   --------------------

   function DerivativeFunc (Item : in Value_Type) return Value_Type is
   begin
      return Result : Value_Type do
         Result := Value_Type(1.0 - Tanh(X => Float(Item))**2);
      end return;
   end DerivativeFunc;

end Activate.Tansig;
