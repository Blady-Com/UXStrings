with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Wide_Wide_Maps; use Ada.Strings.Wide_Wide_Maps;
with Ada.Strings.UTF_Encoding;
private with Ada.Finalization;
private with Ada.Streams;

package UXStrings is

   type Encoding_Scheme is (Latin_1, UTF_8, UTF_16BE, UTF_16LE);
   -- Supported encoding schemes
   subtype UTF_16_Encoding_Scheme is Encoding_Scheme range UTF_16BE .. UTF_16LE;
   -- Supported UTF-16 encoding schemes

   subtype Latin_1_Character is Character;
   subtype Latin_1_Character_Array is String;
   -- Characters in ISO/IEC 8859-1

   subtype BMP_Character is Wide_Character;
   subtype BMP_Character_Array is Wide_String;
   -- Characters in Unicode Basic Multilingual Plane
   -- (Could be also named UCS_2_Character (Universal Coded Character Set)?)

   subtype Unicode_Character is Wide_Wide_Character;
   subtype Unicode_Character_Array is Wide_Wide_String;
   -- Characters in Unicode planes
   -- (Could be also named UCS_4_Character?)

   subtype UTF_8_Character_Array is Ada.Strings.UTF_Encoding.UTF_String;
   subtype UTF_16_Character_Array is Ada.Strings.UTF_Encoding.UTF_String;
   -- Array of 8 bits values representing UTF encodings (UTF-8, UTF-16BE, or UTF-16LE)

   type UXString is tagged private with
      Constant_Indexing => Element,
      Iterable          => (First => First, Next => Next, Has_Element => Has_Element, Element => Element),
      String_Literal    => From_Unicode;
   -- Container type of Unicode characters with dynamic size usually named string

   Null_UXString : constant UXString;
   -- Represent the null string

   function Length (Source : UXString) return Natural;
   -- Return the number of (Unicode) characters

   function First (Source : UXString) return Positive;
   -- Return the position of the first character of Source (actually 1)
   function Next (Source : UXString; Index : Positive) return Positive;
   -- Return the position of the next character of Source after Index position (actually Index + 1)
   procedure Next (Source : UXString; Index : in out Positive);
   -- Update Index to the position of the next character of Source after Index position (actually Index + 1)
   function Has_Element (Source : UXString; Index : Positive) return Boolean;
   -- Return True if a character of Source is present at Index position (actually Index <= Length (Source))
   function Element (Source : UXString; Index : Positive) return Unicode_Character;
   -- Return the Unicode character of Source at Index position
   function Last (Source : UXString) return Natural;
   -- Return the position of the last character of Source (actually Length (Source))

   function Is_Latin_1 (Source : UXString; Index : Positive) return Boolean;
   -- Return True if the character of Source at Index position is in Latin 1 set
   function Is_Latin_1 (Source : UXString) return Boolean;
   -- Return True if all the characters of Source are in Latin 1 set
   function Get_Latin_1
     (Source : UXString; Index : Positive; Substitute : in Latin_1_Character := '¿') return Latin_1_Character;
   -- Return the Latin 1 character from Source at Index position,
   -- if the character is not in latin 1 set then Substitute is returned
   function To_Latin_1 (Source : UXString; Substitute : in Latin_1_Character := '¿') return Latin_1_Character_Array;
   -- Return an array of Latin 1 characters from Source,
   -- if a character is not in latin 1 set then Substitute is returned
   function From_Latin_1 (Item : Latin_1_Character) return UXString;
   -- Return an UXString from the Latin 1 character parameter Item
   function From_Latin_1 (Source : Latin_1_Character_Array) return UXString;
   -- Return an UXString from the array of Latin 1 characters parameter Source

   function Is_BMP (Source : UXString; Index : Positive) return Boolean;
   -- Return True if the character of Source at Index position is in BMP set
   function Is_BMP (Source : UXString) return Boolean;
   -- Return True if all the characters of Source are in BMP set
   function Get_BMP (Source : UXString; Index : Positive; Substitute : in BMP_Character := '¿') return BMP_Character;
   -- Return the BMP character from Source at Index position,
   -- if the character is not in BMP set then Substitute is returned
   function To_BMP (Source : UXString; Substitute : in BMP_Character := '¿') return BMP_Character_Array;
   -- Return an array of BMP characters from Source,
   -- if a character is not in BMP set then Substitute is returned
   function From_BMP (Item : BMP_Character) return UXString;
   -- Return an UXString from the BMP character parameter Item
   function From_BMP (Source : BMP_Character_Array) return UXString;
   -- Return an UXString from the array of BMP characters parameter Source

   function Is_Unicode (Source : UXString; Index : Positive) return Boolean;
   -- Return True if the character of Source at Index position is in Unicode set (actually True)
   function Is_Unicode (Source : UXString) return Boolean;
   -- Return True if all the characters of Source are in Unicode set (actually True)
   function Get_Unicode (Source : UXString; Index : Positive) return Unicode_Character;
   -- Return the Unicode character from Source at Index position
   function To_Unicode (Source : UXString) return Unicode_Character_Array;
   -- Return an array of Unicode characters from Source
   function From_Unicode (Item : Unicode_Character) return UXString;
   -- Return an UXString from the Unicode character parameter Item
   function From_Unicode (Source : Unicode_Character_Array) return UXString;
   -- Return an UXString from the array of Unicode characters parameter Source

   function To_UTF_8 (Source : UXString; Output_BOM : Boolean := False) return UTF_8_Character_Array;
   -- Return an array of UTF-8 characters from Source, prepend UTF-8 BOM if Output_BOM is set to True
   function From_UTF_8 (Source : UTF_8_Character_Array) return UXString;
   -- Return an UXString from the array of UTF-8 characters parameter Source,
   -- leading BOM characters are suppressed if any

   function To_UTF_16
     (Source : UXString; Output_Scheme : UTF_16_Encoding_Scheme; Output_BOM : Boolean := False)
      return UTF_16_Character_Array;
   -- Return an array of UTF-16 characters from Source according to the encoding scheme specified by Output_Scheme,
   -- prepend UTF-16 BOM if Output_BOM is set to True
   function From_UTF_16 (Source : UTF_16_Character_Array; Input_Scheme : UTF_16_Encoding_Scheme) return UXString;
   -- Return an UXString from the array of UTF-16 characters parameter Source
   -- according to the encoding scheme specified by Input_Scheme,
   -- leading BOM characters are suppressed if any

   procedure Set (Target : out UXString; Source : Unicode_Character_Array);
   -- Set Target to an UXString from the array of Unicode characters parameter Source

   procedure Append (Source : in out UXString; New_Item : UXString);
   -- Update Source to the concatenation of Source and New_Item
   procedure Append (Source : in out UXString; New_Item : Unicode_Character);
   -- Update Source to the concatenation of Source and New_Item

   procedure Prepend (Source : in out UXString; New_Item : UXString);
   -- Update Source to the concatenation of New_Item and Source
   procedure Prepend (Source : in out UXString; New_Item : Unicode_Character);
   -- Update Source to the concatenation of New_Item and Source

   function "&" (Left : UXString; Right : UXString) return UXString;
   -- Return the concatenation of Left and Right
   function "&" (Left : UXString; Right : Unicode_Character) return UXString;
   -- Return the concatenation of Left and Right
   function "&" (Left : Unicode_Character; Right : UXString) return UXString;
   -- Return the concatenation of Left and Right

   procedure Replace_Latin_1 (Source : in out UXString; Index : Positive; By : Latin_1_Character);
   -- Update Source such as the character at Index position is set to the Latin 1 character parameter By
   procedure Replace_BMP (Source : in out UXString; Index : Positive; By : BMP_Character);
   -- Update Source such as the character at Index position is set to the BMP character parameter By
   procedure Replace_Unicode (Source : in out UXString; Index : Positive; By : Unicode_Character);
   -- Update Source such as the character at Index position is set to the Unicode character parameter By

   function Slice (Source : UXString; Low : Positive; High : Natural) return UXString;
   -- Return the slice at positions Low through High from Source
   procedure Slice (Source : UXString; Target : out UXString; Low : Positive; High : Natural);
   -- Set Target to the slice at positions Low through High from Source

   function "=" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left equals Right
   function "<" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left is less than Right
   function "<=" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left is less or equal than Right
   function ">" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left is greater than Right
   function ">=" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left greater or equal than Right

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : UXString; Pattern : UXString; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping := Identity) return Natural;
   -- Return the position of the first character where Pattern matches Source
   -- with respect of Going direction and Mapping
   function Index
     (Source  : UXString; Pattern : UXString; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping_Function) return Natural;
   -- Return the position of the first character where Pattern matches Source
   -- with respect of Going direction and Mapping
   function Index
     (Source : UXString; Set : Wide_Wide_Character_Set; Test : Membership := Inside; Going : Direction := Forward)
      return Natural;
   -- Return the position of the first character inside or outside Set matches Source
   -- with respect of Going direction and Test membership
   function Index
     (Source  : UXString; Pattern : UXString; From : Positive; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping := Identity) return Natural;
   -- Return the position of the first character where Pattern matches Source starting at From position
   -- with respect of Going direction and Mapping
   function Index
     (Source  : UXString; Pattern : UXString; From : Positive; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping_Function) return Natural;
   -- Return the position of the first character where Pattern matches Source starting at From position
   -- with respect of Going direction and Mapping
   function Index
     (Source : UXString; Set : Wide_Wide_Character_Set; From : Positive; Test : Membership := Inside;
      Going  : Direction := Forward) return Natural;
   -- Return the position of the first character inside or outside Set matches Source starting at From position
   -- with respect of Test membership

   function Index_Non_Blank (Source : UXString; Going : Direction := Forward) return Natural;
   -- Return the position of the first non space character of Source with respect of Going direction
   function Index_Non_Blank (Source : UXString; From : Positive; Going : Direction := Forward) return Natural;
   -- Return the position of the first non space character of Source starting at From position
   -- with respect of Going direction

   function Count
     (Source : UXString; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping := Identity) return Natural;
   -- Return the number of non overlapping occurrences of Pattern matching Source with respect of Mapping
   function Count
     (Source : UXString; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping_Function) return Natural;
   -- Return the number of non overlapping occurrences of Pattern matching Source with respect of Mapping
   function Count (Source : UXString; Set : Wide_Wide_Character_Set) return Natural;
   -- Return the number of occurrences of characters in parameter Set matching Source

   procedure Find_Token
     (Source :     UXString; Set : Wide_Wide_Character_Set; From : Positive; Test : Membership; First : out Positive;
      Last   : out Natural);
   -- Set First to position of the first character inside or outside Set matches Source starting at From position
   -- Set Last to position of the last character inside or outside Set matches Source with respect of Test membership
   procedure Find_Token
     (Source : UXString; Set : Wide_Wide_Character_Set; Test : Membership; First : out Positive; Last : out Natural);
   -- Set First to position of the first character inside or outside Set matches Source
   -- Set Last to position of the last character inside or outside Set matches Source with respect of Test membership

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate (Source : UXString; Mapping : Wide_Wide_Character_Mapping) return UXString;
   -- Return Source updated with respect of Mapping
   procedure Translate (Source : in out UXString; Mapping : Wide_Wide_Character_Mapping);
   -- Update Source with respect of Mapping
   function Translate (Source : UXString; Mapping : Wide_Wide_Character_Mapping_Function) return UXString;
   -- Return Source updated with respect of Mapping
   procedure Translate (Source : in out UXString; Mapping : Wide_Wide_Character_Mapping_Function);
   -- Update Source with respect of Mapping

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice (Source : UXString; Low : Positive; High : Natural; By : UXString) return UXString;
   -- Return Source whom characters with positions from Low to High are replaced with parameter By
   procedure Replace_Slice (Source : in out UXString; Low : Positive; High : Natural; By : UXString);
   -- Update Source whom characters with positions from Low to High are replaced with parameter By
   function Insert (Source : UXString; Before : Positive; New_Item : UXString) return UXString;
   -- Return Source with New_Item inserted at position ahead of parameter Before
   procedure Insert (Source : in out UXString; Before : Positive; New_Item : UXString);
   -- Update Source with New_Item inserted at position ahead of parameter Before
   function Overwrite (Source : UXString; Position : Positive; New_Item : UXString) return UXString;
   -- Return Source whom characters starting at Position are replaced with parameter New_Item
   procedure Overwrite (Source : in out UXString; Position : Positive; New_Item : UXString);
   -- Update Source whom characters starting at Position are replaced with parameter New_Item
   function Delete (Source : UXString; From : Positive; Through : Natural) return UXString;
   -- Return Source whom characters with positions from Low to High are removed
   procedure Delete (Source : in out UXString; From : Positive; Through : Natural);
   -- Update Source whom characters with positions from Low to High are removed
   function Trim (Source : UXString; Side : Trim_End) return UXString;
   -- Return Source with Space characters removed from left, right or both with respect of Side
   procedure Trim (Source : in out UXString; Side : Trim_End);
   -- Update Source with Space characters removed from left, right or both with respect of Side
   function Trim (Source : UXString; Left : Wide_Wide_Character_Set; Right : Wide_Wide_Character_Set) return UXString;
   -- Return Source with leading characters in Left and trailing characters in Right removed
   procedure Trim (Source : in out UXString; Left : Wide_Wide_Character_Set; Right : Wide_Wide_Character_Set);
   -- Update Source with leading characters in Left and trailing characters in Right removed
   function Head (Source : UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space) return UXString;
   -- Return the first characters from Source up to Count concatenated with Pad characters if needed
   procedure Head (Source : in out UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space);
   -- Update Source to the first characters from Source up to Count concatenated with Pad characters if needed
   function Tail (Source : UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space) return UXString;
   -- Return the last characters from Source up to Count concatenated with Pad characters if needed
   procedure Tail (Source : in out UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space);
   -- Update Source to the last characters from Source up to Count concatenated with Pad characters if needed

   function "*" (Left : Natural; Right : UXString) return UXString;
   -- Return Right string duplicated Left times
   function "*" (Left : Natural; Right : Unicode_Character) return UXString;
   -- Return Right character duplicated Left times

private

   type UTF_8_Characters_Access is access UTF_8_Character_Array;
   type UXString is new Ada.Finalization.Controlled with record
      Chars : UTF_8_Characters_Access := new UTF_8_Character_Array (2 .. 1);
   end record;

   procedure Adjust (Object : in out UXString);
   procedure Finalize (Object : in out UXString);

   procedure Bounded_Move (Source : in out UXString; Target : out UXString; Max : Natural; Last : out Natural);

   procedure UXString_Read (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out UXString);
   for UXString'Read use UXString_Read;

   procedure UXString_Write (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : UXString);
   for UXString'Write use UXString_Write;

   Null_UXString : constant UXString := (Ada.Finalization.Controlled with Chars => new UTF_8_Character_Array (2 .. 1));

end UXStrings;
