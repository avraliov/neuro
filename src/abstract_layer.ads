pragma Ada_2022;

with Ada.Numerics.Generic_Real_Arrays;
with Activate;

generic
   type Value_Type is digits <>;
package Abstract_Layer is

   type Abstract_Layer (Num : Natural := 0) is abstract tagged limited private;

   function Is_Transitory (This : in Abstract_Layer) return Boolean is abstract;

private
   type Abstract_Layer (Num : Natural := 0)  is abstract tagged limited
      record
         Length    : Natural := Num;
      end record;

end Abstract_Layer;
