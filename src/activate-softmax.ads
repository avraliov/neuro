pragma Ada_2012;

generic
package Activate.Softmax is
   
   type Softmax_Routine is tagged private;
   
   function Get_Routine return Routine_Acc;
   function Get_Activate (R : in Softmax_Routine) return Activate_Type;
   function Get_Activate return Activate_Type;
   function Get_Derivative (R : in Softmax_Routine) return Derivative_Type;
   function Get_Activate_Enum (R : in Softmax_Routine) return Function_Enum;

private
   function Func (Item : in Matrix_Pack.Real_Vector; Idx : Positive) return Value_Type with
     Post => Func'Result /= 0.0;
   function DerivativeFunc (Item : in Value_Type) return Value_Type;

   type Softmax_Routine is new Routine with null record;

   R : aliased Softmax_Routine := Softmax_Routine'(Activate   => Func'Access,
                                   Derivative => DerivativeFunc'Access,
                                   Func_Enum  => Soft);

   function Get_Routine return Routine_Acc is
     (R'Access);
   function Get_Activate (R : in Softmax_Routine) return Activate_Type is
      (R.Activate);
   function Get_Activate return Activate_Type is
     (Func'Access);
   function Get_Derivative (R : in Softmax_Routine) return Derivative_Type is
     (R.Derivative);
   function Get_Activate_Enum (R : in Softmax_Routine) return Function_Enum is
     (R.Func_Enum);
   
end Activate.Softmax;
