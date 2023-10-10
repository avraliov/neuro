pragma Ada_2022;

package Utils is
   
   generic
      type T1 is private;
      type Array_Type1 is array (Positive range <>) of T1;
      type T2 is private;
      type Array_Type2 is array (Positive range <>) of T2;
   procedure Generic_Shuffle (Arr1 : in out Array_Type1; Arr2 : in out Array_Type2) with
     Pre => Arr1'Length = Arr2'Length;
   -------------------------------------------
   --Shuffle the arrays with same order
   -------------------------------------------

   generic
      type T is digits <>;
      type Array_Type is array (Positive range <>) of T;
   function Make_Set (Min : in T; Step : in T; Max : in T) return Array_Type with
     Pre => Min < Max and Step /= 0.0,
     Post => (for all I in Make_Set'Result'First .. Make_Set'Result'Last - 1 => 
                Make_Set'Result (I + 1) > Make_Set'Result (I));
   -------------------------------------------
   --Make array of set
   -------------------------------------------

   generic
      type T is digits <>;
      type Array_Type is array (Positive range <>) of T;
   function Make_Sin (Input : in Array_Type) return Array_Type with
     Post => Input'Length = Make_Sin'Result'Length;
   -------------------------------------------
   --Make sin array
   -------------------------------------------

   generic
      type T is digits <>;
      type Array_Type is array (Positive range <>) of T;
   procedure Make_Noise (Arr : in out Array_Type; Coef : in T);
   -------------------------------------------
   --Make noise
   -------------------------------------------

   generic
      type T is digits <>;
      type Array_Type is array (Positive range <>) of T;
   procedure Save_Array_Row (File_Name : String; Arr : Array_Type; Delim : Character := ' ') with
     Pre => File_Name /= "";
   -------------------------------------------
   --Save array to file with serial row sequence
   -------------------------------------------

   generic
      type T is digits <>;
      type Array_Type is array (Positive range <>) of T;
   procedure Save_Array_Column (File_Name : String; Arr : Array_Type) with
     Pre => File_Name /= "";
   -------------------------------------------
   --Save array to file with column sequence
   -------------------------------------------
   
private
   generic
      type T is private;
      type Array_Type is array (Positive range <>) of T;
   procedure Swap (Arr : in out Array_Type; I, J : Integer) with
     Pre => I in Arr'Range and J in Arr'Range;
   -------------------------------------------
   -- internal function for Shuffle procedure
   -------------------------------------------
end Utils;
