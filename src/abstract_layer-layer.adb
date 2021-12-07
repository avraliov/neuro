pragma Ada_2012;
package body Abstract_Layer.Layer is

   ----------
   -- Make --
   ----------
   procedure Make (This : out Layer; Num : in Natural; F : in Activate_Pack.Activate_Type := null) is
   begin
      --      This := Layer'(Num => Num, F => F, Length => Num, Func => F, FuncDeriv => <>);
      null;
   end Make;

end Abstract_Layer.Layer;
