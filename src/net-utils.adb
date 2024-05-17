pragma Ada_2022;

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Net.Utils is

   procedure Save_Waights (This : Net; Name : String; Layer_Num : Positive) is
      package My_IO is new Ada.Text_IO.Float_IO (Num => Value_Type);
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File => F, Mode => Ada.Text_IO.Out_File, Name => Name);

      Passing_Through_Waights :
      for Waight of This.Weights (Layer_Num).all loop
         My_IO.Put (File => F, Item => Waight, Fore => 2, Aft => 6, Exp => 0);
         Ada.Text_IO.New_Line(File => F);
      end loop Passing_Through_Waights;
      Ada.Text_IO.Flush (File => F);
      Ada.Text_IO.Close (File => F);
   end Save_Waights;

   function Get_Weights_Copy (This : in Net; Layer_Num : in Positive) return Weights_Type is
   begin
      Ada.Text_IO.Put_Line(Item => "Get_Weights_Copy");
      return Result : Weights_Type do
         if Layer_Num in This.Weights'Range then
            Result.Weights     :=
              new Layer_Waights
                (This.Weights (Layer_Num).all'Range (1), This.Weights (Layer_Num).all'Range (2));
            Result.Weights.all := This.Weights (Layer_Num).all;
         end if;
      end return;
   end Get_Weights_Copy;

   function Get_Weights_Acc (This : in Net; Layer_Num : Positive) return not null access constant Real_Matrix is
   begin
      return Result : not null access constant Real_Matrix := This.Weights (Layer_Num) do
         null;
      end return;
   end Get_Weights_Acc;


   procedure Set_Weights (This : in out Net; Layer_Num : Positive; Weights : Weights_Type) is
   begin
      if Layer_Num in This.Weights'Range
        and then This.Weights (Layer_Num) /= null
        and then Weights.Weights'Length (1) = This.Weights (Layer_Num)'Length (1)
        and then Weights.Weights'Length (2) = This.Weights (Layer_Num)'Length (2)
      then
--         Ada.Text_IO.Put_Line (Item => "Net.Weights = " & This.Weights (Layer_Num).all'Image);
--         Ada.Text_IO.Put_Line (Item => "Weights = " & Weights.Weights.all'Image);
         This.Weights (Layer_Num).all := Weights.Weights.all;
      end if;
   end Set_Weights;

   procedure Finalize (Obj : in out Weights_Type) is
      procedure Free is new Ada.Unchecked_Deallocation (Object => Real_Matrix, Name => Weights_Acc);
   begin
      Ada.Text_IO.Put_Line (Item => "Finalize");
      if Obj.Weights /= null then
         Free (Obj.Weights);
      end if;
   end Finalize;

   procedure Adjust (Obj : in out Weights_Type) is
   begin
      if Obj.Weights /= null then
         Obj.Weights := new Layer_Waights'(Obj.Weights.all);
      end if;
   end Adjust;

   function Get_Value (This : Weights_Type; Row, Col : Positive) return Value_Type is
   begin
      return Result : constant Value_Type := This.Weights (Row, Col) do
         null;
      end return;
   end Get_Value;

   procedure Delate_Weights (This : in out Weights_Type) is
   begin
      Finalize(Obj => This);
   end Delate_Weights;

end Net.Utils;
