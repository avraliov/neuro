pragma Ada_2012;

generic
package Activate.Tansig is

   type Tansig_Routine is tagged private;

   function Get_Routine return Routine_Acc;
   function Get_Activate (R : in Tansig_Routine) return Activate_Type;
   function Get_Activate return Activate_Type;
   function Get_Derivative (R : in Tansig_Routine) return Derivative_Type;
   function Get_Activate_Enum (R : in Tansig_Routine) return Function_Enum;

private
   function Func (Item : in Matrix_Pack.Real_Vector; Idx : Positive) return Value_Type;
   function DerivativeFunc (Item : in Value_Type) return Value_Type;

   type Tansig_Routine is new Routine with null record;
   R : aliased Tansig_Routine := Tansig_Routine'(Activate   => Func'Access,
                                                 Derivative => DerivativeFunc'Access,
                                                 Func_Enum  => Tan);

   function Get_Routine return Routine_Acc is
     (R'Access);
   function Get_Activate (R : in Tansig_Routine) return Activate_Type is
      (R.Activate);
   function Get_Activate return Activate_Type is
     (Func'Access);
   function Get_Derivative (R : in Tansig_Routine) return Derivative_Type is
     (R.Derivative);
   function Get_Activate_Enum (R : in Tansig_Routine) return Function_Enum is
     (R.Func_Enum);

end Activate.Tansig;
