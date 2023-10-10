pragma Ada_2022;

with Ada.Numerics.Float_Random;
with Ada.Numerics.Real_Arrays;
with Ada.Streams.Stream_IO;
with GNAT.Random_Numbers;
with Ada.Text_IO;
with Activate.Linear;
with Activate.Logsig;
with Activate.Tansig;
with Activate.Softmax;
with Activate.Re_LU;
with Activate.Leaky_Re_LU;
with Activate.Dropout;
with Activate.Softplus;

package body Net is

   package Logsig_Pack is new Layer_Package.Activate_Pack.Logsig;
   package Tansig_Pack is new Layer_Package.Activate_Pack.Tansig;
   package Linear_Pack is new Layer_Package.Activate_Pack.Linear;
   package Softmax_Pack is new Layer_Package.Activate_Pack.Softmax;
   package ReLU_Pack is new Layer_Package.Activate_Pack.Re_LU;
   package Leaky_Re_LU_Pack is new Layer_Package.Activate_Pack.Leaky_Re_LU;
   package Drop_Pack is new Layer_Package.Activate_Pack.Dropout (Dropout_Rate => 0.5);
   package Softplus_Pack is  new Layer_Package.Activate_Pack.Softplus;
   ----------
   -- Make --
   ----------
   procedure Make (This : in out Net; Layers : in Layers_Acc_Array) is
      type Waight_Tensor_Arr is array (Positive range <>, Positive range <>) of Value_Type;

      --task for random number generator
      task type  Tensor_Randomizer is
         --This entry generate Gaussian distributed init values
         --in mean 0.5 and deviation 0.5
         entry Init_Gaussian (Item : in out  Layer_Waights);

         --This entry generate Xavier distributed init values
         --calculate by formula 2 / (neurons_num_input + neurons_num_output)
         entry Init_Xavier (Item : in out  Layer_Waights; In_Num, Out_Num : in Natural);

         --This entry generate Gaussian distributed init values
         --in mean 0.5 and deviation 0.5
         entry Init_Biases (Item : in out Biases_Arr; In_Num, Out_Num : in Natural);
      end Tensor_Randomizer;

      task body Tensor_Randomizer is
         My_Generator : GNAT.Random_Numbers.Generator;

         --Get function generate normal distributed value in -1 .. 1 and
         --recalculate it in 0 .. 1 by formula (nd * diviation + mean)
         --if mean is 0.5. If mean is 0.0 then in -1 .. 1
         function Get ( Mean, Deviation : Float) return Value_Type with
           Pre => Mean + Deviation in -1.0 .. 1.0 and Deviation /= 0.0,
           Post => (Get'Result in -1.0 .. 1.0)
         is
            use GNAT.Random_Numbers;
         begin
          return Result : Value_Type do
               declare
                  Tmp : Float := Float'Last;
               begin
                  loop
                     Tmp := Random_Gaussian (Gen => My_Generator);
                     if Tmp in -1.0 .. 1.0 then 
                        exit;
                     end if;
                  end loop;
                  Tmp := Tmp * Deviation + Mean;
                  Result := Value_Type (Tmp);
               end;
            end return;
         end Get;
      begin
         GNAT.Random_Numbers.Reset (Gen => My_Generator);
         loop
            select
               accept Init_Gaussian (Item : in out Layer_Waights) do
                  Item := [others => (others => Get (Mean => 0.5, Deviation => 0.5))];
               end Init_Gaussian;
            or
               accept Init_Xavier (Item : in out Layer_Waights; In_Num, Out_Num : in Natural) do
                  declare
                     Deviation : Float := 2.0 / (Float(In_Num) + Float(Out_Num));
                  begin
                  Item := (others => (others => Get (Mean => 0.0, Deviation => Deviation)));
                  end;
               end Init_Xavier;
            or
               accept Init_Biases (Item : in out Biases_Arr; In_Num, Out_Num : in Natural) do
                  declare
                     Deviation : Float := 2.0 / (Float (In_Num) + Float (Out_Num));
                  begin
                     Item := (others => Get (Mean => 0.0, Deviation => Deviation));
                  end;
               end Init_Biases;
            or 
               terminate;
            end select;
         end loop;
      end Tensor_Randomizer;

      type Task_Array_Base_Type is array (This.Waights'Range) of Tensor_Randomizer;
      subtype Task_Array_Type is Task_Array_Base_Type with
        Dynamic_Predicate => Task_Array_Type'Length <= 8;
      --type for waights initialization tasks
      Task_Array : Task_Array_Type;
      --Array of tasks for waights initialization

      --this procedure initializing values tensor to  value
      procedure Init_Values_Tensor with
        Post => (for all I of This.Values (1).all => I = 0.0)
      is
         Idx : Positive := This.Values'First;
      begin
      for I of Layers loop
         declare
               Local_Values_Arr : aliased Values_Arr (1 .. I.Num) := (others => 0.0);
         begin
               This.Values (Idx) := new Values_Arr (1 .. I.Num);
               This.Values (Idx).all := Local_Values_Arr;
               Idx := Idx + 1;
         end;
      end loop;
      end Init_Values_Tensor;

      --this procedure initializing errors tensor
      procedure Init_Errors_Tensor with
        Post => (for all I of This.Deltas =>
                (for all J of I.all => J = 0.0))
      is
         Idx : Positive := This.Deltas'First;
      begin
      for I of This.Deltas loop
         declare
         begin
               I := new Errors_Arr (1 .. Layers (Idx).Num);
               I.all := (others => 0.0);
               Idx := Idx + 1;
         end;
      end loop;
      end Init_Errors_Tensor;

      --this procedure initializing biases tensor to random value
      procedure Init_Biases_Tensor with
        Post => (for all I of This.Biases => (for all J of I.all => J /= 0.0))
      is
         Idx : Positive := This.Values'First;
         Rand : Tensor_Randomizer;
      begin
         for I in This.Biases'Range loop
            declare
            begin
               This.Biases(I) := new Biases_Arr (1 .. Layers (Idx + 1).Num);
--               This.Biases(I).all := (others => 0.5);
               ----------------------------
               --make initialization here
               --of random values
               ----------------------------
               Rand.Init_Biases (Item => This.Biases (I).all, 
                                 In_Num => This.Values (I - 1).all'Length,
                                 Out_Num => (if I + 1 in This.Values'Range then This.Values (I + 1).all'Length else 0));
               Idx := @ + 1;
            end;
         end loop;
      end Init_Biases_Tensor;

      use type Layer_Package.Activate_Pack.Function_Enum;
      --this procedure initializing waights tensor to random value
      procedure Init_Waight_Tensor with 
        Pre => (for all I in This.Activates_Enum'First + 1 .. This.Activates_Enum'Last => 
                  This.Activates_Enum(I) /= Layer_Package.Activate_Pack.None)
      is
         Idx : Positive := This.Values'First;
      begin
         for I in This.Waights'Range loop
            This.Waights (I) := new Layer_Waights (1 .. Layers (Idx + 1).Num, 1 .. Layers (Idx).Num);
            case (This.Activates_Enum (I)) is
               when Layer_Package.Activate_Pack.Log | Layer_Package.Activate_Pack.Lin=>
                  Task_Array (I).Init_Xavier (Item   => This.Waights (I).all, 
                                              In_Num => This.Values (I - 1).all'Length,
                                              Out_Num => (if I + 1 in This.Values'Range then This.Values (I + 1).all'Length else 0));
--                  Task_Array (I).Init_Gaussian (Item => This.Waights (I).all);
               when others =>
                  Task_Array (I).Init_Gaussian (Item => This.Waights (I).all);
            end case;
            --Task_Array (I).Init_Gaussian (Item => This.Waights (I).all);
            Idx := Idx + 1;
         end loop;
      end Init_Waight_Tensor;

      --this procedure initializing activate functions array
      procedure Init_Activates with
        Pre => This.Activates'First = This.Activates_Enum'First and
        This.Activates'Last = This.Activates_Enum'Last
      is
      begin
         for I in This.Activates'Range loop
            This.Activates (I) := Lay_Pack.Func (Layers (I).all);
            This.Activates_Enum (I) := Lay_Pack.Func_Enum (Layers (I).all);
         end loop;
      end Init_Activates;
      
      --this procedure initializing post_procedures functions array
      procedure Init_Post_Proc with
        Pre => This.Post_Procedures'First = This.Values'First and
        This.Post_Procedures'Last = This.Values'Last
      is
      begin
         for I in This.Post_Procedures'Range loop
            This.Post_Procedures (I) := Lay_Pack.Post_Proc (Layers (I).all);
         end loop;
      end Init_Post_Proc;

      --this procedure initializing derivative functions array
      procedure Init_Derivatives is
      begin
         for I in This.derivatives'Range loop
            This.Derivatives (I) := Lay_Pack.Deriv(Layers (I).all);
         end loop;
      end Init_Derivatives;
      
   begin
      Init_Values_Tensor;
      Init_Activates;
      Init_Waight_Tensor;
      Init_Biases_Tensor;
      Init_Post_Proc;
      if(This.Train = True) then
         Init_Errors_Tensor;
         Init_Derivatives;
      end if;
   end Make;

--   procedure Finalize (Item : in out Net) is
--   begin
--      for I of Item.Values loop
--         Free (I);
--      end loop;
--   end Finalize;

   procedure Feed_Forward (This : in out Net; Input : in Input_Vector; Pre_Post_Routine : in Boolean)
   is
      use type Layer_Package.Matrix_Pack.Real_Vector;
      Normolized_Input : Input_Vector (Input'Range) := (others => Value_Type'Last);
      use Lay_Pack.Layer_Func_Pack;
   begin
      Normolize_Input :
      for I in Input'Range loop
         Normolized_Input (I) := This.Normolize_Input (Item => Input (I));
      end loop Normolize_Input;
      -----------------
      This.Values (This.Values'First).all := Normolized_Input;
      for Values_Tensor_Index in This.Waights'Range loop
         declare
            Value_Idx : Positive := 1;
            Result_Vector : Layer_Package.Matrix_Pack.Real_Vector (1 .. This.Values (Values_Tensor_Index)'Length) := (others => Value_Type'Last);
         begin
            Result_Vector := (This.Waights (Values_Tensor_Index).all * This.Values (Values_Tensor_Index - 1).all) + This.Biases (Values_Tensor_Index).all;
            Take_Derivation :
            for I of This.Values (Values_Tensor_Index).all loop
               begin
                   I := Value_Type (This.Activates (Values_Tensor_Index).all (Result_Vector, Value_Idx));
                  Value_Idx := @ + 1;
               end;
            end loop Take_Derivation;
            -----------------------------
            --start post procedure
            if Pre_Post_Routine then
               if This.Post_Procedures (Values_Tensor_Index - 1) /= null then
                  This.Post_Procedures (Values_Tensor_Index - 1).all (This.Values (Values_Tensor_Index - 1).all);
               end if;
            end if;
            -----------------------------
         end;
      end loop;
   end Feed_Forward;


   function Is_Input_Valid (This : in out Net; Vector : in Input_Vector) return Boolean is
   begin
      return Result : Boolean do
         Result := (if Vector'Length = This.Values (This.Values'First).all'Length
                    then True else False);
      end return;
   end Is_Input_Valid;

   function Is_Target_Valid (This : in out Net; Vector : in Target_Vector) return Boolean is
   begin
      return Result : Boolean do
         Result := (if Vector'Length = This.Values (This.Values'Last).all'Length
                    then True else False);
      end return;
   end Is_Target_Valid;

   function Created (This : in Net) return Boolean is
      use type Lay_Pack.Activate_Pack.Activate_Type;
   begin
      return Result : Boolean := False do
         Result := 
           (for all I of This.Values => I /= null) and then
           (for all I of This.Waights => I /= null) and then
           (for all I of This.Biases => I /= null) and then
           (for all I of This.Activates => I /= null);
      end return;
   end Created;
   
   function Get_Result (This : in out Net) return Target_Vector is
   begin
      return Result : constant Target_Vector := This.Values (This.Values'Last).all do
         null;
      end return;
   end Get_Result;

   procedure Save (This : in Net; Name : String := "") is
      F : Ada.Streams.Stream_IO.File_Type;
      S : Ada.Streams.Stream_IO.Stream_Access;
   begin
      Ada.Streams.Stream_IO.Create (File => F,
                                    Mode => Ada.Streams.Stream_IO.Out_File,
                                    Name => Name);
      S := Ada.Streams.Stream_IO.Stream (File => F);
      --
      Net'Output(S, This);
      --
      Ada.Streams.Stream_IO.Flush (File => F);
      Ada.Streams.Stream_IO.Close (File => F);
   end Save;

   procedure Net_Output (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : in Net) is
   begin
      Positive'Write (Stream, Item.Layers_Num);

      --waights writing
      Waights_Tensor'Output (Stream, Item.Waights);
      Waights_Tensor'Write (Stream, Item.Waights);
      for I of Item.Waights loop
         Layer_Waights'Output (Stream, I.all);
         Layer_Waights'Write (Stream, I.all);
      end loop;
      
      --biases writing
      Biases_Tensor'Output (Stream, Item.Biases);
      Biases_Tensor'Write (Stream, Item.Biases);
      for I of Item.Biases loop
         Biases_Arr'Output (Stream, I.all);
         Biases_Arr'Write (Stream, I.all);
      end loop;
      
      --activate functions writing
      Activate_Enum_Arr'Output (Stream, Item.Activates_Enum);
      Activate_Enum_Arr'Write (Stream, Item.Activates_Enum);
   end Net_Output;
   
   function Net_Input (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return Net is
      Layers_Num : Positive;
   begin
      Positive'Read (Stream, Layers_Num);
      return Res_Net : Net (Layers_Num, False) do
         --waights read
         Res_Net.Waights := Waights_Tensor'Input (Stream);
         Waights_Tensor'Read (Stream, Res_Net.Waights);
         for I of Res_Net.Waights loop
            declare
               Local_Layer_W : Layer_Waights := Layer_Waights'Input (Stream);
            begin
               I := new Layer_Waights (Local_Layer_W'Range (1), Local_Layer_W'Range (2));
               Layer_Waights'Read (Stream, I.all);
            end;
         end loop;
         
         --biases read
         Res_Net.Biases := Biases_Tensor'Input (Stream);
         Biases_Tensor'Read (Stream, Res_Net.Biases);
         for I of Res_Net.Biases loop
            declare
               Local_Layer_Biases : Biases_Arr := Biases_Arr'Input (Stream);
            begin
               I := new Biases_Arr (Local_Layer_Biases'Range);
               Biases_Arr'Read (Stream, I.all);
            end;
         end loop;
         
         --activate functions read
         Res_Net.Activates_Enum := Activate_Enum_Arr'Input (Stream);
         Activate_Enum_Arr'Read (Stream, Res_Net.Activates_Enum);
         --activate functions init
         for I in Res_Net.Activates_Enum'Range loop
            case Res_Net.Activates_Enum (I) is
               when Layer_Package.Activate_Pack.Lin =>
                  Res_Net.Activates (I) := Linear_Pack.Get_Activate;
               when Layer_Package.Activate_Pack.Log => 
                  Res_Net.Activates (I) := Logsig_Pack.Get_Activate;
               when Layer_Package.Activate_Pack.Tan => 
                  Res_Net.Activates (I) := Tansig_Pack.Get_Activate;
               when Layer_Package.Activate_Pack.Soft_max => 
                  Res_Net.Activates (I) := Softmax_Pack.Get_Activate;
               when Layer_Package.Activate_Pack.ReLU => 
                  Res_Net.Activates (I) := ReLU_Pack.Get_Activate;
               when Layer_Package.Activate_Pack.Leaky_ReLU => 
                  Res_Net.Activates (I) := Leaky_Re_LU_Pack.Get_Activate;
               when Layer_Package.Activate_Pack.Drop => 
                  Res_Net.Activates (I) := Drop_Pack.Get_Activate;
               when Layer_Package.Activate_Pack.Soft_plus => 
                  Res_Net.Activates (I) := Softplus_Pack.Get_Activate;
               when Layer_Package.Activate_Pack.None => null;
            end case;
         end loop;
         
         --values init
         declare
            First_Iter : Boolean := True;
            Idx        : Positive := 1;
         begin
            for I of Res_Net.Waights loop
               if First_Iter then
                  Res_Net.Values (1) := new Values_Arr (I'Range (2));
                  Res_Net.Values (1).all := (others => 0.0);
                  Res_Net.Values (2) := new Values_Arr (I'Range (1));
                  Res_Net.Values (2).all := (others => 0.0);
               else
                  Res_Net.Values (Idx + 1) := new Values_Arr (I'Range (1));
                  Res_Net.Values (Idx + 1).all := [others => 0.0];
               end if;
               Idx := Idx + 1;
               First_Iter := False;
            end loop;
         end;
         --
      end return; 
   end Net_Input;
   
   function Load (Name : String := "") return Net is
      F : Ada.Streams.Stream_IO.File_Type;
      S : Ada.Streams.Stream_IO.Stream_Access;
   begin
      Ada.Streams.Stream_IO.Open (File => F,
                                  Mode => Ada.Streams.Stream_IO.In_File,
                                  Name => Name);
      S := Ada.Streams.Stream_IO.Stream (File => F);
      
      return This : Net := Net'Input (S) do
         Ada.Streams.Stream_IO.Close (File => F);
      end return;
      --  declare
      --     This :Net := Net'Input (S);
      --  begin
      --     Ada.Streams.Stream_IO.Close (File => F);
      --     return This;
      --  end;
   end Load;

   -------------------
   -- Set_Input_Bounds
   -------------------
   procedure Set_Input_Bounds (This : in out Net; Min, Max : Value_Type) is
   begin
      This.Min_Value := Min;
      This.Max_Value := Max;
   end Set_Input_Bounds;

   function Is_Input_Bounds (This : in Net) return Boolean is
   begin
     return This.Min_Value < This.Max_Value;
   end Is_Input_Bounds;
end Net;
