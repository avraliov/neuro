pragma Ada_2012;

package body Activate.Linear is

   ----------
   -- Func --
   ----------

   function Func (Item : in Matrix_Pack.Real_Vector; Idx : Positive) return Value_Type is
   begin
      return Result : Value_Type do
         Result := Value_Type(Item(Idx));
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
   

end Activate.Linear;
