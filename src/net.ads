pragma Ada_2022;
pragma SPARK_Mode (On);

with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Ada.Numerics.Float_Random;
with Ada.Streams;
with Ada.Numerics.Generic_Real_Arrays;

with Abstract_Layer.Layer;
with Activate;

generic
   type Value_Type is digits <>;
   with package Abstract_Lay_Pack is new Abstract_Layer (Value_Type  => Value_Type);
   with package Lay_Pack is new Abstract_Lay_Pack.Layer;
package Net is
   package Layer_Package renames Lay_Pack;
   package Activate_Pack is new Activate (Value_Type  => Value_Type,
                                          Matrix_Pack => Lay_Pack.Matrix_Pack);

   type Neurons_Nums is array (Positive range <>) of Positive;
   
   type Layer_Acc is access all Layer_Package.Layer;
   type Layers_Acc_Array is array (Positive range <>) of Layer_Acc;
   subtype Train_Type is Boolean;
   type Net (Layers_Num : Positive; Train : Train_Type) is tagged limited private;
   
   subtype Input_Vector is Layer_Package.Matrix_Pack.Real_Vector;
   subtype Target_Vector is Layer_Package.Matrix_Pack.Real_Vector;

   subtype Error_Type is Value_Type;
   type Error_Ep is array(Positive range <>) of Error_Type;
   
   use type Layer_Package.Activate_Pack.Derivative_Type;
   
   function Layers_Neurons_Nums  (This : in out Net) return Neurons_Nums with
     Post => (for all I of Layers_Neurons_Nums'Result => I > 0);
   
   procedure Make (This : in out Net; Layers : in Layers_Acc_Array) with
     Pre => This.Layers_Num = Layers'Length and 
     (if This.Train and (for all I in Layers'First + 1 .. Layers'Last => Layer_Package.Deriv(Layers (I).all) /= null) then True else False); 
   
   function Is_Input_Valid (This : in out Net; Vector : in Input_Vector) return Boolean with 
     Pre => Vector'Length > 0;
   function Is_Target_Valid (This : in out Net; Vector : in Target_Vector) return Boolean with 
     Pre => Vector'Length > 0;
   function Created (This : in Net) return Boolean;

   function Is_Input_Bounds (This : in Net) return Boolean;

   procedure Feed_Forward (This : in out Net; Input : in Input_Vector; Pre_Post_Routine : in Boolean) with 
     Pre => This.Is_Input_Valid (Vector => Input) and This.Is_Input_Bounds;
   
--   function Back_Propogate (This : in out Net; Target : in Target_Vector; Lr : in Value_Type) return Error_Type with
--     Pre => This.Is_Target_Valid (Vector => Target);
   
   --Returns output vector
   function Get_Result (This : in out Net) return Target_Vector with
     Pre => This.Created,
     Post => Is_Target_Valid (This   => This, Vector => Get_Result'Result);

   --Save net to disk
   procedure Save (This : in Net; Name : String := "") with
     Pre => Name /= "" and This.Created;

   --Load net from disk
   function Load (Name : String := "") return Net with
     Pre => Name /= "";

   --Set bounds of input values for normalization proc
   procedure Set_Input_Bounds (This : in out Net; Min, Max : in Value_Type);
   
   function Net_Input (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return Net;
   function Weights_Tensors_Num (This : Net) return Positive;
   
private
   --types declaration of values
   subtype Values_Arr is Layer_Package.Matrix_Pack.Real_Vector;
   --types declaration of biases
   subtype Biases_Arr is Values_Arr;
   --types declaration of waights
   subtype Layer_Waights is Layer_Package.Matrix_Pack.Real_Matrix;
   --types declaration of errors
   subtype Errors_Arr is Layer_Package.Matrix_Pack.Real_Vector;

   --types declaration of tensors
   type Values_Tensor is array (Positive range <>) of access Values_Arr;
   type Waights_Tensor is array (Positive range <>) of access Layer_Waights;
   type Biases_Tensor is array (Positive range <>) of access Biases_Arr;
   type Deltas_Tensor is array (Positive range <>) of access Errors_Arr;

   --types declaration of activate function array
   type Activate_Arr is array (Positive range <>) of Layer_Package.Activate_Pack.Activate_Type;
   type Activate_Enum_Arr is array (Positive range <>) of Layer_Package.Activate_Pack.Function_Enum;
   type Derivative_Arr is array (Positive range <>) of Layer_Package.Activate_Pack.Derivative_Type;
   type Post_Proc is array (Positive range <>) of Layer_Package.Layer_Func_Pack.Pre_Post_ProcType;
   function Aaa (X : Integer)return Integer is (X - X) with Static;
   type Net (Layers_Num : Positive; Train : Train_Type) is tagged limited 
      record
         Values          : Values_Tensor (1 .. Layers_Num) := [others => null];
         Weights         : Waights_Tensor (2 .. Layers_Num) := [others => null];
         Biases          : Biases_Tensor (2 .. Layers_Num) := [others => null];
         Activates       : Activate_Arr (2 .. Layers_Num) := [others => null];
         Post_Procedures : Post_Proc (1 .. Layers_Num) := [others => null];
         Activates_Enum  : Activate_Enum_Arr (2 .. Layers_Num) := [others => Layer_Package.Activate_Pack.None];
         Min_Value       : Value_Type := Value_Type'Last;
         Max_Value       : Value_Type := Value_Type'Last;
      case Train is
            when True =>
               Deltas : Deltas_Tensor (2 .. Layers_Num);
	       Derivatives : Derivative_Arr (2 .. Layers_Num);
            when False =>
               null;
         end case;
      end record
     with Output => Net_Output, Input => Net_Input;
   

   procedure Net_Output (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : in Net);

   --normalization with minmax function
   --formula = (x-xmin)*(d2-d1)/(xmax-xmin)+d1
   --with x - value, [xmin;xmax] - x interlav, [d1;d2] - destination interval
   function Normolize_Input (This : in Net; Item : in Value_Type) return Value_Type is
     ((Item - This.Min_Value) * (1.0 - 0.0) / (This.Max_Value - This.Min_Value) + 0.0);

   function Weights_Tensors_Num (This : Net) return Positive is
      (This.Weights'Length);
   
--   procedure Free is new Ada.Unchecked_Deallocation (Object => Value_Arr,
--                                                     Name   => Values_Arr_Ref);
--   procedure Finalize (Item : in out Net);

end Net;
