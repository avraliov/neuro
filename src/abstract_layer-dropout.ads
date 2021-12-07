pragma Ada_2012;
  
generic
package Abstract_Layer.Dropout is

   type Layer_Dropout (Num : Natural := 0) is limited private;
   
   procedure Reset_Mask (This : in out Layer_Dropout;  Rate : Value_Type);
   function Is_Transitory (This : in Layer_Dropout) return Boolean
     with Post => Is_Transitory'Result = True;

private
   subtype Mask_Rnd is Value_Type range 0.0 .. 1.0;
   type Layer_Mask_Type is array (Positive range <>) of Mask_Rnd;

   type Layer_Dropout (Num : Natural := 0) is limited new Abstract_Layer (Num => Num) with 
      record
         Layer_Mask : Layer_Mask_Type (1 .. Num) := (others => Mask_Rnd'First);
      end record;

   function Is_Transitory (This : in Layer_Dropout) return Boolean is
     (True);

end Abstract_Layer.Dropout;
