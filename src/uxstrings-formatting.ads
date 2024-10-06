with System;
with Ada.Strings;

private with GNAT.Formatted_String;

package UXStrings.Formatting is

   subtype Number_Base is Integer range 2 .. 16;
   -- Range of possible numerical base on integer values
   subtype Alignment is Ada.Strings.Alignment;
   -- List of possible alignment of integer values

   generic
      type T is range <>;
   function Integer_Format
     (Item    :    T; Base : in Number_Base := 10; Put_Plus : in Boolean := False; Field : in Natural := 0;
      Justify : in Alignment := Left; Fill : in Character := ' ') return UXString;
   -- Return the formatted string of the integer Item with respect of specified Base, Put_Plus sign,
   -- output Field size, alignment Justify, padding Fill

   type Formatted_UXString (<>) is private;
   -- See usage in GNAT.Formatted_String source file g-forstr.ads

   function Format (Spec : UXString) return Formatted_UXString;
   -- Create the formatted string with Spec format

   function From (Format : Formatted_UXString) return UXString;
   -- Return the formatted string with the respect of given parameters

   generic
      type T is (<>);
   function Formatted_Scalar (Left : Formatted_UXString; Right : T) return Formatted_UXString;
   -- Return the formatted form of the scalar item matching %c or %s

   function "&" (Left : Formatted_UXString; Right : Boolean) return Formatted_UXString;
   -- Return the formatted form of the Boolean item matching %s

   function "&" (Left : Formatted_UXString; Right : Character) return Formatted_UXString;
   -- Return the formatted form of the Character item matching %c

   generic
      type T is range <>;
   function Formatted_Integer (Left : Formatted_UXString; Right : T) return Formatted_UXString;
   -- Return the formatted form of the generic number item matching %d, %o, %x, %X

   function "&" (Left : Formatted_UXString; Right : Integer) return Formatted_UXString;
   -- Return the formatted form of the Integer item matching %d, %o, %x, %X

   generic
      type T is digits <>;
   function Formatted_Floating (Left : Formatted_UXString; Right : T) return Formatted_UXString;
   -- Return the formatted form of the generic floating item matching %f, %e, %F, %E, %g, %G

   function "&" (Left : Formatted_UXString; Right : Float) return Formatted_UXString;
   -- Return the formatted form of the Float item matching %f, %e, %F, %E, %g, %G

   generic
      type T is delta <>;
   function Formatted_Fixed_Point (Left : Formatted_UXString; Right : T) return Formatted_UXString;
   -- Return the formatted form of the generic fixed point item matching %f, %e, %F, %E, %g, %G

   function "&" (Left : Formatted_UXString; Right : Duration) return Formatted_UXString;
   -- Return the formatted form of the Duration item matching %f, %e, %F, %E, %g, %G

   function "&" (Left : Formatted_UXString; Right : System.Address) return Formatted_UXString;
      -- Return the formatted form of the Address item matching %p

   function "&" (Left : Formatted_UXString; Right : UXString) return Formatted_UXString;
   -- Return the formatted form of the UXString item matching %s

private

   type Formatted_UXString is new GNAT.Formatted_String.Formatted_String;

end UXStrings.Formatting;
