-------------------------------------------------------------------------------
-- NAME (body)                  : uxstrings-formatting.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : UXString formatting implementation.
-- NOTES                        : Ada 2022
--
-- COPYRIGHT                    : (c) Pascal Pignard 2024
-- LICENCE                      : CeCILL-C (https://cecill.info)
-- CONTACT                      : http://blady.chez.com
-------------------------------------------------------------------------------

with Strings_Edit.Integer_Edit;
with GNAT.Formatted_String; use GNAT.Formatted_String;

with Ada.Text_IO;

package body UXStrings.Formatting is

   --------------------
   -- Integer_Format --
   --------------------

   function Integer_Format
     (Item    :    T; Base : in Number_Base := 10; Put_Plus : in Boolean := False; Field : in Natural := 0;
      Justify : in Alignment := Left; Fill : in Character := ' ') return UXString
   is
      package Strings_Edit_T is new Strings_Edit.Integer_Edit (T);
      Text    : String (1 .. 80);
      Pointer : Integer := Text'First;
   begin
      Strings_Edit_T.Put (Text, Pointer, Item, Base, Put_Plus, Field, Justify, Fill);
      return From_ASCII (Text (Text'First .. Pointer - 1));
   end Integer_Format;

   ------------
   -- Format --
   ------------

   function Format (Spec : UXString) return Formatted_UXString is
   begin
      return +To_UTF_8 (Spec);
   end Format;

   ----------
   -- From --
   ----------

   function From (Format : Formatted_UXString) return UXString is
   begin
      return From_UTF_8 (-Format);
   end From;

   ----------------------
   -- Formatted_Scalar --
   ----------------------

   function Formatted_Scalar (Left : Formatted_UXString; Right : T) return Formatted_UXString is
      function S_F is new Enum_Format (T);
   begin
      return Formatted_UXString (S_F (Formatted_String (Left), Right));
   end Formatted_Scalar;

   ---------
   -- "&" --
   ---------

   function "&" (Left : Formatted_UXString; Right : Boolean) return Formatted_UXString is
   begin
      return Formatted_UXString (Formatted_String (Left) & Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Formatted_UXString; Right : Character) return Formatted_UXString is
   begin
      return Formatted_UXString (Formatted_String (Left) & Right);
   end "&";

   -----------------------
   -- Formatted_Integer --
   -----------------------

   function Formatted_Integer (Left : Formatted_UXString; Right : T) return Formatted_UXString is
      package TIO_IIO is new Ada.Text_IO.Integer_IO (T);
      function I_F is new Int_Format (T, TIO_IIO.Put);
   begin
      return Formatted_UXString (I_F (Formatted_String (Left), Right));
   end Formatted_Integer;

   ---------
   -- "&" --
   ---------

   function "&" (Left : Formatted_UXString; Right : Integer) return Formatted_UXString is
   begin
      return Formatted_UXString (Formatted_String (Left) & Right);
   end "&";

   ------------------------
   -- Formatted_Floating --
   ------------------------

   function Formatted_Floating (Left : Formatted_UXString; Right : T) return Formatted_UXString is
      package TIO_FIO is new Ada.Text_IO.Float_IO (T);
      function F_F is new Flt_Format (T, TIO_FIO.Put);
   begin
      return Formatted_UXString (F_F (Formatted_String (Left), Right));
   end Formatted_Floating;

   ---------
   -- "&" --
   ---------

   function "&" (Left : Formatted_UXString; Right : Float) return Formatted_UXString is
   begin
      return Formatted_UXString (Formatted_String (Left) & Right);
   end "&";

   ---------------------------
   -- Formatted_Fixed_Point --
   ---------------------------

   function Formatted_Fixed_Point (Left : Formatted_UXString; Right : T) return Formatted_UXString is
      package TIO_FIO is new Ada.Text_IO.Fixed_IO (T);
      function F_P_F is new Fixed_Format (T, TIO_FIO.Put);
   begin
      return Formatted_UXString (F_P_F (Formatted_String (Left), Right));
   end Formatted_Fixed_Point;

   ---------
   -- "&" --
   ---------

   function "&" (Left : Formatted_UXString; Right : Duration) return Formatted_UXString is
   begin
      return Formatted_UXString (Formatted_String (Left) & Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Formatted_UXString; Right : System.Address) return Formatted_UXString is
   begin
      return Formatted_UXString (Formatted_String (Left) & Right);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Left : Formatted_UXString; Right : UXString) return Formatted_UXString is
   begin
      return Formatted_UXString (Formatted_String (Left) & To_UTF_8 (Right));
   end "&";

end UXStrings.Formatting;
