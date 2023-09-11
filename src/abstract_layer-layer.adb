pragma Ada_2012;
package body Abstract_Layer.Layer is

   ----------
   -- Make --
   ----------
--   procedure Make (This : out Layer; Num : in Natural; F : in Activate_Pack.Activate_Type := null) is
--   begin
      --      This := Layer'(Num => Num, F => F, Length => Num, Func => F, FuncDeriv => <>);
--      null;
--   end Make;

   function Make (Num : in Natural; R : in Activate_Pack.Routine_Acc := null;
                  P   : in Layer_Func_Pack.Pre_Post_ProcType := null) return Layer is
   begin
      return Result : Layer (Num => Num, R => R, Post_P => P) do
         null;
      end return;
   end Make;
end Abstract_Layer.Layer;
