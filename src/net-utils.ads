pragma Ada_2022;

with Ada.Finalization;

generic
package Net.Utils is
   use Layer_Package.Matrix_Pack;

   type Weights_Type is private;

   procedure Save_Waights (This : Net; Name : String; Layer_Num : Positive);
   -- Saves waights of layer(Layer_Num) to file (Name)

   function Is_Empty(Item: Weights_Type) return Boolean;
   function Get_Weights_Acc (This : Net; Layer_Num : Positive) return not null access constant Real_Matrix;
   -- Get access to waights of layer(Layer_Num)
   function Get_Weights_Copy (This : Net; Layer_Num : Positive) return Weights_Type;
   -- Get waights copy of layer(Layer_Num)
   function Get_Value (This : Weights_Type; Row, Col : Positive) return Value_Type;
   -- Get value from the Weights_Type in Row, Col position

   procedure Set_Weights (This : in out Net; Layer_Num : Positive; Weights : Weights_Type) with
     Pre => not Is_Empty(Weights);
   -- Set waights to layer(Layer_Num)
   procedure Delate_Weights (This : in out Weights_Type);

private
   type Weights_Acc is access Layer_Waights;

   type Weights_Type is new Ada.Finalization.Controlled with record
      Weights : Weights_Acc;
   end record;

   procedure Adjust (Obj : in out Weights_Type);
   procedure Finalize (Obj : in out Weights_Type);

   function Is_Empty (Item : Weights_Type) return Boolean is
      (if Item.Weights = null then True else False);

end Net.Utils;
