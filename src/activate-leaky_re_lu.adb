pragma Ada_2012;

package body Activate.Leaky_Re_LU is

   ----------
   -- Func --
   ----------

   function Func (Item : in Matrix_Pack.Real_Vector; Idx : Positive) return Value_Type is
   begin
      return Result : Value_Type do
         Result := (if Value_Type(Item(Idx)) <= 0.0 then 0.01 * Value_Type(Item(Idx)) else Value_Type(Item(Idx)));
      end return;
   end Func;

   --------------------
   -- DerivativeFunc --
   --------------------

   function DerivativeFunc (Item : in Value_Type) return Value_Type is
   begin
      return Result : Value_Type := (if Item <= 0.0 then 0.01 else 1.0) do
         null;
      end return;
   end DerivativeFunc;


end Activate.Leaky_Re_LU;
