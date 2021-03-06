pragma Ada_2012;

with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;

package body Utils is
   --------------------
   --Swap
   --------------------
   procedure Swap (Arr : in out Array_Type; I, J : Integer) is
      Tmp : constant T := Arr (I);
   begin      
      Arr (I) := Arr (J);
      Arr (J) := Tmp;
   end Swap;
   
   --------------------
   --Shaffle
   --------------------
   procedure Generic_Shuffle (Arr1 : in out Array_Type1; Arr2 : in out Array_Type2) is
      procedure Swap1 is new Swap (T => T1, Array_Type => Array_Type1);
      procedure Swap2 is new Swap (T => T2, Array_Type => Array_Type2);
   begin
         
      for I in Arr1'First .. Arr1'Last - 1 loop
         declare
--            subtype Rnd_Range is Positive range Arr1'Range;
            subtype Rnd_Range is Positive range I .. Arr1'Last - 1;
            package Random is new Ada.Numerics.Discrete_Random (Result_Subtype => Rnd_Range);
            My_Generator : Random.Generator;
            Value : Rnd_Range;
         begin
            Random.Reset (Gen => My_Generator);
            Value := Random.Random (Gen => My_Generator);
            Swap1 (Arr => Arr1, I => I, J => Value);
            Swap2 (Arr => Arr2, I => I, J => Value);
         end;
      end loop;
   end Generic_Shuffle;

   --------------------
   --Make_Set
   --------------------
   function Make_Set (Min : in T; Step : in T; Max : in T) return Array_Type is
      Tmp : constant T := (Max - Min) / Step;
      Upper_Bound : constant Integer := Integer (Tmp);
      V           : T := Min;
   begin
      return Res : Array_Type (1 .. Upper_Bound + 1) do
         for I of Res loop
            I := V;
            V := V + Step;
         end loop;
      end return;
   end Make_Set;

   --------------------
   --Make_Sin
   --------------------
   function Make_Sin (Input : in Array_Type) return Array_Type is
      package Functions is new Ada.Numerics.Generic_Elementary_Functions(Float_Type => T);
   begin
      return Res : Array_Type (Input'Range) do
         for I in Res'Range loop
            Res (I) := Functions.Sin (X => Input (I));
         end loop;
      end return;
   end Make_Sin;

   --------------------
   --Make_Noise
   --------------------
   procedure Make_Noise (Arr : in out Array_Type; Coef : in T) is
      Tmp       : T := 0.0;
      Generator : Ada.Numerics.Float_Random.Generator;
   begin
      for I of Arr loop
         Tmp := T (Ada.Numerics.Float_Random.Random (Gen => Generator));
         I := I + Tmp * Coef;
      end loop;
   end Make_Noise;
   
end Utils;
