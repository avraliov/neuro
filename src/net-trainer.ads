pragma Ada_2012;

with Ada.Numerics.Generic_Real_Arrays;
with Ada.Containers.Vectors;

with Net;
with Abstract_Layer.Layer;
with Activate;
with Utils;

generic
   Input_Num : Positive;
   Output_Num : Positive;
package Net.Trainer is
   type Loss_Type is (MSE, CE, SIMPLE);
   type LR_Type is delta 0.001 range 0.001 .. 1.0;

   subtype Input_Vector is Lay_Pack.Matrix_Pack.Real_Vector (1 .. Input_Num);
   subtype Target_Vector is Lay_Pack.Matrix_Pack.Real_Vector (1 .. Output_Num);
   type Input_Dataset is array (Positive range <>) of Input_Vector;
   type Target_Dataset is array (Positive range <>) of Target_Vector;

   type Parameters is record
      Learn_Rate : LR_Type;
      Loss       : Loss_Type;
      Epochs     : Positive;
      Error      : Value_Type := Value_Type'Last;
   end record;

   package My_Vector is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                    Element_Type => Value_Type);
   use type Ada.Containers.Count_Type;
   type Result is record
      Train_errors : My_Vector.Vector;
      Test_Errors : My_Vector.Vector;
   end record;
   ----------------------------------------------------------
   --ALL THIS EXPIRIMENTAL
   ----------------------------------------------------------
   type Train_Test_Dataset_Indexes is record
      Train_Input_Dataset_Start : Positive := Positive'Last;
      Train_Input_Dataset_End : Positive := Positive'Last;
      Test_Input_Dataset_Start : Positive := Positive'Last;
      Test_Input_Dataset_End : Positive := Positive'Last;
      Train_Target_Dataset_Start : Positive := Positive'Last;
      Train_Target_Dataset_End : Positive := Positive'Last;
      Test_Target_Dataset_Start : Positive := Positive'Last;
      Test_Target_Dataset_End : Positive := Positive'Last;
   end record;

   subtype Train_Test_Rate_Type is Positive range 50 .. 100;
   --rate type discribes relation value of Train and Test datasets
   function Calculate_Rate (Dataset_Size : Positive; Rate : Train_Test_Rate_Type) return Integer is
     (Integer (Float (Dataset_Size) / 100.0 * Float (Rate)));
   --return index of last element by rate
   function Make_Train_Test_Dataset_Indexes (Input : Input_Dataset; Target : Target_Dataset; Rate : Train_Test_Rate_Type) return Train_Test_Dataset_Indexes;

   ----------------------------------------------------------
   ----------------------------------------------------------
   ----------------------------------------------------------

   subtype Train_Test_Index is Float range 0.5 .. 0.9;
   -- This type describes value of training and test data relation
   function Train (This   : in out Net; Input : Input_Dataset; Target : Target_Dataset;
                   Params : in out Parameters; Train_Index : Train_Test_Index) return Result with
     Pre => Input'Length = Target'Length and Params.Epochs > 1,
     Post => Params.Error < Value_Type'Last and
     Train'Result.Test_Errors.Length = Train'Result.Train_Errors.Length;
   ----------------------------------------------------------
   --This - training net
   --Input and Target - data for training
   --Train_Index - rate of training and test data relation
   --Returns result record with errors vector
   ----------------------------------------------------------

   procedure Shaffle is new Utils.Generic_Shuffle (T1 => Input_Vector, Array_Type1 => Input_Dataset,
                                                   T2 => Target_Vector, Array_Type2 => Target_Dataset);
   --Shaffle the dataset

   generic
      type Array_Type is array (Positive range <>) of Value_Type;
   function Make_Single_Input_Dataset (Arr : Array_Type) return Input_Dataset with
     Post => Arr'Length = Make_Single_Input_Dataset'Result'Length;
   --make a single dataset of input

   generic
      type Array_Type is array (Positive range <>) of Value_Type;
   function Make_Single_Target_Dataset (Arr : Array_Type) return Target_Dataset with
     Post => Arr'Length = Make_Single_Target_Dataset'Result'Length;
   --make a single dataset of target

   subtype Error_Type is Value_Type;
   function Calculate_Error (Result : Target_Vector; Target : Target_Vector) return Error_Type with
     Pre => Result'Length = Target'Length;
   --calculate output errors

   procedure Save (Item : in Result) with
     Pre => not Item.Train_Errors.Is_Empty and
     not Item.Test_Errors.Is_Empty and
     Item.Test_Errors.Length = Item.Train_Errors.Length;
   --save the training result to files "Train_errors" and "Test_Errors"

   procedure Save_Test (Arr : Target_Dataset) with
     Pre => Arr'Length > 1;
   --save the test result to "Test_result"

   function Test (This : in out Net; Input : Input_Dataset) return Target_Dataset with
     Pre => Input'Length > 1,
     Post => Input'Length = Test'Result'Length;
   --make result array for the input array

   function Back_Propogate (This : in out Net; Target : in Target_Vector; Lr : in LR_Type) return Error_Type with
     Pre => This.Is_Target_Valid (Vector => Target);

end Net.Trainer;
