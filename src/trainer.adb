pragma Ada_2012;

with Ada.Float_Text_IO;
with Ada.Text_IO;
with Ada.Real_Time;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Trainer is

   -----------
   -- Train --
   -----------

   function Train
     (This : in out Net_Pack.Net;
      Input : Input_Dataset;
      Target : Target_Dataset;
      Params : in out Parameters;
      Train_Index : Train_Test_Index)
      return Result
   is
      package My_Value_IO is new Ada.Text_IO.Float_IO (Num => Value_Type);
      package My_Duration_Io is new Ada.Text_IO.Fixed_IO (Num => Duration);
   begin
      return Res : Result do
         declare
            Epoches     : Positive := Positive'First;
            Err         : Error_Type := 0.0;
            Idx         : Positive range Input'Range := Input'First;
            Err_Ep      : Value_Type := Value_Type'Last;
            Err_Ep_Test : Value_Type := Value_Type'Last;
            Err_Str     : String (1 .. 16 ) := (others => ' ');
            Dur_Str     : String (1 .. 16 ) := (others => ' ');
            Start_Time  : Ada.Real_Time.Time;
            Total_Time  : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
            Period      : Duration;
            use type Ada.Real_Time.Time;
         begin
            loop
               Start_Time := Ada.Real_Time.Clock;
               This.Feed_Forward (Input => Input (Idx), Pre_Post_Routine => True);
               Err := Err + This.Back_Propogate (Target => Target (Idx), Lr => Params.Learn_Rate);
               if Idx in Input'First .. Integer(Float(Input'Length) * Train_Index)  then
                  Idx := Idx + 1;
               else
                  Err_Ep := Err / Value_Type (Idx);
                  Res.Train_errors.Append (Err_Ep);
                  My_Value_IO.Put (To   => Err_Str,
                                   Item => Err_Ep,
                                   Aft  => 6,
                                   Exp  => 0);

                  Period := Ada.Real_Time.To_Duration(Ada.Real_Time.Clock - Start_Time) * 1000.0; -- (for ms);
                  My_Duration_Io.Put (To   => Dur_Str,
                                      Item => Period,
                                      Aft  => 6,
                                      Exp  => 0);
                  Ada.Text_IO.Put_Line (Item => "Epoch" & Epoches'Img & ": Error = " &
                                          Ada.Strings.Fixed.Trim (Source => Err_Str, Side   => Ada.Strings.Left) &
                                          ": Time = " & Ada.Strings.Fixed.Trim (Source => Dur_Str, Side   => Ada.Strings.Left) & " ms.");
                  ------------------------------Test Phase
                  Idx := 1;
                  Err := 0.0;
                  for I in Integer (Float (Input'Length) * Train_Index)+1 .. Input'Last loop
                     This.Feed_Forward (Input => Input (I), Pre_Post_Routine => False);
                     Err := Err + Calculate_Error (Result => This.Get_Result, Target => Target (I));
                     Idx := Idx + 1;
                  end loop;
                  Err_Ep_Test := Err / Value_Type (Idx);
                  Res.Test_Errors.Append (Err_Ep_Test);
                  My_Value_IO.Put (To   => Err_Str,
                                   Item => Err_Ep_Test,
                                   Aft  => 6,
                                   Exp  => 0);
                  Ada.Text_IO.Put_Line (Item => "Test Error = " &
                                          Ada.Strings.Fixed.Trim (Source => Err_Str, Side   => Ada.Strings.Left));
                  Idx := 1;
                  Err := 0.0;
                  ------------------------------
                  Epoches := Epoches + 1;
               end if;
               exit when Err_Ep <= Params.Error or Epoches = Params.Epochs;
            end loop;
            Period := Ada.Real_Time.To_Duration (Ada.Real_Time.Clock - Total_Time);-- * 1000.0 (for ms);
            Dur_Str := (others => ' ');
            My_Duration_Io.Put (To   => Dur_Str,
                                Item => Period,
                                Aft  => 2,
                                Exp  => 0);
            Ada.Text_IO.Put_Line (Item => "Total time: " &
                                    Ada.Strings.Fixed.Trim (Source => Dur_Str, Side   => Ada.Strings.Left) & " s.");
         end;
      end return;
   end Train;

   -----------
   --Make_Single_Input_Dataset
   -----------
   function Make_Single_Input_Dataset (Arr : Array_Type) return Input_Dataset is
   begin
      return Res : Input_Dataset (Arr'Range) do
         for I in Res'Range loop
            Res (I) := (1 => Arr (I));
         end loop;
      end return;
   end Make_Single_Input_Dataset;

   ---------------------------
   --Make_Single_Target_Dataset
   ---------------------------
   function Make_Single_Target_Dataset (Arr : Array_Type) return Target_Dataset is
   begin
      return Res : Target_Dataset (Arr'Range) do
         for I in Res'Range loop
            Res (I) := (1 => Arr (I));
         end loop;
      end return;
   end Make_Single_Target_Dataset;

   -----------
   --Calculate_Error
   -----------
   function Calculate_Error (Result : in Target_Vector; Target : in Target_Vector) return Error_Type is
      use type Lay_Pack.Matrix_Pack.Real_Vector;
      Errors_Out : Lay_Pack.Matrix_Pack.Real_Vector (Target'Range) := Target - Result;
   begin
      return Err : Error_Type := Error_Type'Last do
         declare
            Err_Sum : Value_Type := 0.0;
         begin
            for I of Errors_Out loop
               Err_Sum := Err_Sum + (0.5 * (I * I));
            end loop;
            Err := Err_Sum / Error_Type (Errors_Out'Length);
         end;
      end return;
   end Calculate_Error;

   -----------
   --Save
   -----------
   procedure Save (Item : in Result) is
      package My_IO is new Ada.Text_IO.Float_IO (Num => Value_Type);
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File => F,
                          Mode => Ada.Text_IO.Out_File,
                          Name => "train_errors.txt");
      for I of Item.Train_Errors loop
         My_IO.Put (File => F,
                    Item => I,
                    Fore => 2,
                    Aft  => 6,
                    Exp  => 0);
      end loop;
      Ada.Text_IO.Close (File => F);

      Ada.Text_IO.Create (File => F,
                          Mode => Ada.Text_IO.Out_File,
                          Name => "test_errors.txt");
      for I of Item.Test_Errors loop
         My_IO.Put (File => F,
                    Item => I,
                    Fore => 2,
                    Aft  => 6,
                    Exp  => 0);
      end loop;
   end Save;


   -----------
   --Save_Test
   -----------
   procedure Save_Test (Arr : Target_Dataset) is
      package My_IO is new Ada.Text_IO.Float_IO (Num => Value_Type);
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File => F,
                          Mode => Ada.Text_IO.Out_File,
                          Name => "test_result.txt");
      for I of Arr loop
         for J of I loop
            My_IO.Put (File => F,
                       Item => J,
                       Fore => 2,
                       Aft  => 6,
                       Exp  => 0);
         end loop;
      end loop;
   end Save_Test;

   -----------
   --Test
   -----------
   function Test (This : in out Net_Pack.Net; Input : Input_Dataset) return Target_Dataset is
   begin
      return Res : Target_Dataset (Input'Range)do
         for I in Input'Range loop
            This.Feed_Forward (Input => Input (I), Pre_Post_Routine => False);
            Res (I) := This.Get_Result;
         end loop;
      end return;
   end Test;

   function Make_Train_Test_Dataset_Indexes (Input : Input_Dataset; Target : Target_Dataset; Rate : Train_Test_Rate_Type) return Train_Test_Dataset_Indexes is
   begin
      return Res : Train_Test_Dataset_Indexes do
         Res := Train_Test_Dataset_Indexes'(Train_Input_Dataset_Start  => Input'First,
                                            Train_Input_Dataset_End    => Calculate_Rate (Dataset_Size => Input'Last,
                                                                                          Rate         => Rate),
                                            Test_Input_Dataset_Start   => Calculate_Rate (Dataset_Size => Input'Last,
                                                                                          Rate         => Rate) + 1,
                                            Test_Input_Dataset_End     => Input'Last,
                                            Train_Target_Dataset_Start => Target'First,
                                            Train_Target_Dataset_End   => Calculate_Rate (Dataset_Size => Target'Last,
                                                                                          Rate         => Rate),
                                            Test_Target_Dataset_Start  => Calculate_Rate (Dataset_Size => Target'Last,
                                                                                          Rate         => Rate)+1,
                                            Test_Target_Dataset_End    => Target'Last);
      end return;
   end Make_Train_Test_Dataset_Indexes;

end Trainer;
