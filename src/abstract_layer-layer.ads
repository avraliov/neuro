pragma Ada_2012;

with Ada.Numerics.Generic_Real_Arrays;
with Activate;
with Layer_Functions;

generic
package Abstract_Layer.Layer is
   package Matrix_Pack is new Ada.Numerics.Generic_Real_Arrays (Value_Type);
   package Activate_Pack is new Activate (Value_Type  => Value_Type,
                                          Matrix_Pack => Matrix_Pack);
   package Layer_Func_Pack is new Layer_Functions (Value_Type  => Value_Type,
                                                   Matrix_Pack => Matrix_Pack);

   type Layer (Num : Natural := 0; R : Activate_Pack.Routine_Acc := null; Post_P : Layer_Func_Pack.Pre_Post_ProcType := null) is limited private;
   function Func (This : in Layer) return Activate_Pack.Activate_Type;
   function Deriv (This : in Layer) return Activate_Pack.Derivative_Type;
   function Func_Enum (This : in Layer) return Activate_Pack.Function_Enum;
   function Post_Proc (This : in Layer) return Layer_Func_Pack.Pre_Post_ProcType;

   procedure Make (This : out Layer; Num : in Natural; F : in Activate_Pack.Activate_Type := null);
   function Is_Transitory (This : in Layer) return Boolean
     with Post => Is_Transitory'Result = False;

private
   type Layer (Num : Natural := 0; R : Activate_Pack.Routine_Acc := null; Post_P : Layer_Func_Pack.Pre_Post_ProcType := null) is limited new Abstract_Layer (Num => Num)
     with record
      Func      : Activate_Pack.Activate_Type := R.Get_Activate;
      Deriv     : Activate_Pack.Derivative_Type := R.Get_Derivative;
      Func_Enum : Activate_Pack.Function_Enum := R.Get_Activate_Enum;
      Post_Proc : Layer_Func_Pack.Pre_Post_ProcType := Post_P;
   end record;

   function Func (This : in Layer) return Activate_Pack.Activate_Type is
     (This.Func);
   function Deriv (This : in Layer) return Activate_Pack.Derivative_Type is
      (This.Deriv);
   function Func_Enum (This : in Layer) return Activate_Pack.Function_Enum is
     (This.Func_Enum);
   function Post_Proc (This : in Layer) return Layer_Func_Pack.Pre_Post_ProcType is
      (This.Post_Proc);

   function Is_Transitory (This : in Layer) return Boolean is
      (False);

end Abstract_Layer.Layer;
