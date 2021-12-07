pragma Ada_2012;

generic
package Activate.Linear is

   type Linear_Routine is tagged private;

   function Get_Routine return Routine_Acc;
   function Get_Activate (R : in Linear_Routine) return Activate_Type;
   function Get_Activate return Activate_Type;
   function Get_Derivative (R : in Linear_Routine) return Derivative_Type;
   function Get_Activate_Enum (R : in Linear_Routine) return Function_Enum;

private
   function Func (Item : in Matrix_Pack.Real_Vector; Idx : Positive) return Value_Type;
   function DerivativeFunc (Item : in Value_Type) return Value_Type with
     Post => DerivativeFunc'Result = 1.0;

   type Linear_Routine is new Routine with null record;
   R : aliased Linear_Routine := Linear_Routine'(Activate   => Func'Access,
                                   Derivative => DerivativeFunc'Access,
                                   Func_Enum  => Lin);

   function Get_Routine return Routine_Acc is
     (R'Access);
   function Get_Activate (R : in Linear_Routine) return Activate_Type is
      (R.Activate);
   function Get_Activate return Activate_Type is
     (Func'Access);
   function Get_Derivative (R : in Linear_Routine) return Derivative_Type is
     (R.Derivative);
   function Get_Activate_Enum (R : in Linear_Routine) return Function_Enum is
     (R.Func_Enum);

end Activate.Linear;
