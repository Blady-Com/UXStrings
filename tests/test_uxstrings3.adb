pragma Wide_Character_Encoding (UTF8);
with UXStrings;         use UXStrings;
with UXStrings.Text_IO; use UXStrings.Text_IO;
with UXStrings.Conversions;
with UXStrings.Hash;
with UXStrings.Formatting;
with UXStrings.Lists;
with Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;

procedure Test_UXStrings3 is

   function Image is new UXStrings.Conversions.Scalar_Image (Boolean);
   function Image is new UXStrings.Conversions.Integer_Image (Integer);
   function Value is new UXStrings.Conversions.Integer_Value (Integer);

   function Format is new UXStrings.Formatting.Integer_Format (Natural);
   use all type UXStrings.Formatting.Alignment;

   procedure Send (Msg : UTF_8_Character_Array) is
   begin
      for Code of Msg loop
         Put (Image (Character'Pos (Code), 16));
      end loop;
      New_Line;
   end;
   function Receive return UTF_8_Character_Array is (To_UTF_8("données"));

   S1, S2, S3 : UXString;
   C          : Character;
   WC         : Wide_Character;
   WWC        : Wide_Wide_Character;
   F          : Boolean;
   First_S3   : Positive;
   Last_S3    : Natural;
   D          : constant array (Positive range <>) of Natural := (16#0075#, 16#003E#, 16#30E3#, 16#03A3#);
   --     Data : constant BMP_Character_array := (for I in D'Range => BMP_Character'val (D(I)));

   UXSL1 : constant UXStrings.Lists.UXString_List := ["Ada", "Strings", "Wide_Wide_Maps", "Wide_Wide_Constants", "Lower_Case_Map"];

   S4 : constant UXString := "Ada.Strings.Wide_Wide_Maps.Is_Subset(Item, My_Set)";
   S5 : constant UXString := "/Applications/Xcode.app/Contents/Developer/Library/Frameworks/Python3.framework/Versions/Current/lib/libpython3.9.dylib";

   SA1 : constant String (9 .. 38) := "était blah blahétait blah blah";
   SA2 : constant BMP_Character_Array (57 .. 106) := "une soirée passée à étudier la physique ω=Δθ/Δt...";
   SA3 : constant Unicode_Character_Array (109 .. 160) :="une soirée passée à étudier les mathématiques ℕ⊂𝕂...";

begin
   -- Change the default to LF and UTF-8
   Ending (Current_Output, LF_Ending);
   Line_Mark (LF_Ending);
   Scheme (Current_Output, UTF_8);

   S1 := From_Latin_1 (SA1 (9..23));
   S2 := From_BMP (SA2);
   S3 := From_Unicode (SA3);
   Put_Line (S1 & Line_Mark & S2 & Line_Mark & S3);
   Send (To_UTF_8 (S1) & To_UTF_8 (S3));
   S2 := "Received: " & From_UTF_8 (Receive);
   S3 := S1 & " - Sent ok";
   Put_Line (S1 & Line_Mark & S2 & Line_Mark & S3);
   S1 := 4 * '.';
   --     S2 := From_BMP (Data);
   --     S3 := 4*'.';
   --     for I in Data'Range loop
   --        S3(I) := Data (I); -- discriminant check failed
   --     end loop;
   Put_Line (S1 & Line_Mark & S2 & Line_Mark & S3);
   S1 := From_Latin_1 (SA1 (24..38));
   S2 := From_BMP (SA2);
   S3 := From_Unicode (SA3);
   Put_Line (S1 & Line_Mark & S2 & Line_Mark & S3);
   Put_Line (Image(Index (S1, "ée")) & Image(Index (S2, "ée"),prefix=> ' ') & Image(index (S3, "ée", 10),prefix =>' '));
   Find_Token (S3, Ada.Strings.Wide_Wide_Maps.To_Set(" "), Ada.Strings.Inside, First_S3, Last_S3);
   Put_Line (Image(First_S3) & ',' & Image(Last_S3) & ',' & Remove (S3, "passée ") & ',' & Replace (S3, "soirée", "journée"));
   C   := S1.Get_Latin_1 (5);
   WC  := S1.Get_BMP (7);
   WWC := S1 (1);
   Put_Line
     (Image (Character'Pos (C), 16) & ',' & Image (Wide_Character'Pos (WC), 16) & ',' &
      Image (Wide_Wide_Character'Pos (WWC), 16));
   for I in S3 loop
      F := S3.Get_Latin_1 (I) = 'é';
      if F then
         Replace_Latin_1 (S3 ,I, 'e');
      end if;
      WWC := S3 (I);
      Put_Line (Image (I) & ':' & Image (Wide_Wide_Character'Pos (WWC), 16) & ',' & Image (F));
   end loop;
   Put_Line (S3);
   for CC of S2 loop
      WWC := CC;
      F   := CC = 'é';
      Put_Line (Image (Wide_Wide_Character'Pos (WWC), 16) & ',' & Image (F));
   end loop;
   Replace_Unicode (S1 ,3, WWC);
   S1.Replace_BMP (2, WC);
   S1.Replace_Latin_1 (1, C);
   S1.Replace_ASCII (4, 'Z');
   Put_Line (S1);
   if S1 /= "test" then
      S1 := Null_UXString;
      S2 := 2 * 'z';
      S3 := 4 * "po";
      Put_Line (S1 & Line_Mark & S2 & Line_Mark & S3);
   end if;
   S3 := "Riri";
   S2 := "Loulou";
   S1 := " et Fifi";
   S2.Append (S1);
   S1.Prepend (S3);
   Put_Line (S1 & Line_Mark & S2 & Line_Mark & S3);
   Put_Line (Image(Integer(UXStrings.Hash(S1)),16));
   Put_Line (Image (Value("  + 73")));
   C := S1.To_Latin_1 (3);
   Put_Line (Image (Character'Pos (C), 16));
   C := S1.Get_Latin_1 (3); -- same result but avoid all string conversion
   Put_Line (Image (Character'Pos (C), 16));
   Put_Line (Format (5, 2, True, 10, Center, '@'));

   Put_Line (Image(S1.Contains ("Riri")));
   Put_Line (Image(S1.Contains ("et", Insensitive)));
   Put_Line (Image(S2.Contains ("Riri")));
   Put_Line (Image(S2.Count ("lo", Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants.Lower_Case_Map)));
   Put_Line (Image(S2.Ends_With ("Fifi")));
   Put_Line (Image(S3.Ends_With ("Loulou")));
   Put_Line (Image(S3.Starts_With ("riri", Insensitive)));
   Put_Line (Image(S2.Is_Lower ));
   Put_Line (Image(S2.Is_Upper) );
   Put_Line (Image(S2.Is_Basic ));
   Put_Line (Image(S2.Is_Empty ));
   Put_Line (S2.Remove ("ou"));
   Put_Line (S2.Remove ("fi", Insensitive) & '.');
   Put_Line (S2.Replace ("lou", "Coin", Insensitive));

   for E of UXSL1 loop
      Put_Line (E);
   end loop;
   for E of S4.Split(Ada.Strings.Wide_Wide_Maps.To_Set(".(,) "), Keep_Empty_Parts => False).Sort loop
      Put_Line (E);
   end loop;
   Put_Line (UXSL1.Join ('-').To_Lower);
   for E of S5.Split('/').Filter ("py", Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants.Lower_Case_Map) loop
      Put_Line (E);
   end loop;

   declare
      A : constant UXStrings.Lists.UXString_Array := UXStrings.Lists.From_UXString_List (UXSL1);
   begin
      Put_Line ("First item: " & A(1));
   end;

   declare
      use UXStrings.Formatting;
      A : constant Formatted_UXString := Format ("Valeurs %X %s %c") & 22 & True & 'e';
      My_Format : constant Formatted_UXString := Format ("%s");
   begin
      Put_Line (From (Format ("%s : %E") & "a well known float" & Float'(3.14)));
      Put_Line (From (My_Format & "a string"));
      Put_Line (From (A));
   end;

   declare
      use UXStrings.Formatting;
      F : Formatted_UXString := Format ("['%c' ; %10d]");
      C : constant Character := 'v';
      I : constant Integer   := 98;
   begin
      F := F & C & I;
      Put_Line (From (F));
   end;

   Put_Line ("--end--");
end Test_UXStrings3;
