pragma Ada_2012;

with Ada.Numerics.Generic_Elementary_Functions;

package body Activate.Softmax is

   ----------
   -- Func --
   ----------

   function Func
     (Item : in Matrix_Pack.Real_Vector;
      Idx : Positive)
      return Value_Type
   is
      package My_Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions (Float_Type => Value_Type);
      Sum : Value_Type := 0.0;
      Ex : Value_Type := 0.0;
   begin
      return Res : Value_Type := 0.0 do
         Ex := My_Elementary_Functions.Exp (X => Item (Idx));
         for I of Item loop
            Sum := Sum + My_Elementary_Functions.Exp (I);
            Res := Ex / Sum;
         end loop;
      end return;
   end Func;

   --------------------
   -- DerivativeFunc --
   --------------------

   function DerivativeFunc (Item : in Value_Type) return Value_Type is
   begin
      return Res : Value_Type := 0.0 do
         Res := Item * (1.0 - Item);
      end return;
   end DerivativeFunc;

end Activate.Softmax;
