pragma Ada_2022;

with Ada.Text_IO;

package body Net.Utils is

   procedure Save_Waights (This : Net; Name : String; Layer_Num : Positive) is
      package My_IO is new Ada.Text_IO.Float_IO (Num => Value_Type);
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File => F, Mode => Ada.Text_IO.Out_File, Name => Name);

      Passing_Through_Waights :
      for Waight of This.Waights (Layer_Num).all loop
            My_IO.Put (File => F,
                       Item => Waight,
                       Fore => 2,
                       Aft  => 6,
                       Exp  => 0);
      end loop Passing_Through_Waights;
   end Save_Waights;

   function Get_Weights (This : in Net; Layer_Num : in Positive) return Real_Matrix is
   begin
      return Result : Real_Matrix := This.Waights (Layer_Num).all do
         null;
      end return;
   end Get_Weights;

end Net.Utils;
