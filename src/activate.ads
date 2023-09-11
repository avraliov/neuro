pragma Ada_2012;

with Ada.Numerics.Generic_Real_Arrays;

generic
   type Value_Type is digits <>;
   with package Matrix_Pack is new Ada.Numerics.Generic_Real_Arrays (Real => Value_Type);
package Activate is
   type Function_Enum is (Drop, None, Log, Tan, Lin, Soft_Max, ReLU, Leaky_ReLU, Soft_Plus);
   type Routine is abstract tagged private;
   type Routine_Acc is access all Routine'Class;

   function Func (Item : in Matrix_Pack.Real_Vector; Idx : Positive) return Value_Type is abstract;
   function DerivativeFunc (Item : in Value_Type) return Value_Type is abstract;

   type Activate_Type is access function (Item : in Matrix_Pack.Real_Vector; Idx : Positive) return Value_Type;
   type Derivative_Type is access function (Item : in Value_Type) return Value_Type;

   function Get_Routine return Routine_Acc is abstract;
   function Get_Activate (R : in Routine) return Activate_Type is abstract;
   function Get_Derivative (R : in Routine) return Derivative_Type is abstract;
   function Get_Activate_Enum (R : in Routine) return Function_Enum is abstract;
private

   type Routine is abstract tagged record
      Activate   : Activate_Type;
      Derivative : Derivative_Type;
      Func_Enum  : Function_Enum;
   end record;

end Activate;
