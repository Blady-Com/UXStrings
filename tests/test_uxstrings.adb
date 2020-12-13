with UXStrings;   use UXStrings;
with UXStrings.Text_IO; use UXStrings.Text_IO;
with Strings_Edit.Integers; use Strings_Edit.Integers;

procedure Test_UXStrings is

   procedure Send (Msg : UTF_8_Character_Array) is
   begin
      for Code of Msg loop
         Put (From_Latin_1(Image(Character'pos (Code), 16)));
      end loop;
      New_Line;
   end;
   function Receive return UTF_8_Character_Array is (To_UTF_8("données"));

   S1, S2, S3 : UXString;
   C          : Character;
   WC         : Wide_Character;
   WWC        : Wide_Wide_Character;
   F          : Boolean;
   D : constant array (Positive range <>) of Natural := (16#0075#, 16#003E#, 16#30E3#, 16#03A3#);
   Data : constant BMP_Character_array := (for I in D'Range => BMP_Character'val (D(I)));

begin
   -- Change the default to LF and UTF-8
   Ending (Current_Output, LF);
   Line_Mark (LF);
   Scheme (Current_Output, UTF_8);

   S1 := From_Latin_1 ("était blah blah");
   S2 := From_BMP ("une soirée passée à étudier la physique ω=Δθ/Δt...");
   S3 := From_Unicode ("une soirée passée à étudier les mathématiques ℕ⊂𝕂...");
   Put_Line (S1 & Line_Mark & S2 & Line_Mark & S3);
   Send (To_UTF_8 (S1) & To_UTF_8 (S3));
   S2  := "Received: " & From_UTF_8 (Receive);
   S3 := S1 & " - Sent ok";
   Put_Line (S1 & Line_Mark & S2 & Line_Mark & S3);
   S1 := 4*'.';
   S2 := From_BMP (Data);
--     S3 := 4*'.';
--     for I in Data'Range loop
--        S3(I) := Data (I); -- discriminant check failed
--     end loop;
   Put_Line (S1 & Line_Mark & S2 & Line_Mark & S3);
   S1 := "était blah blah";
   S2 := "une soirée passée à étudier la physique ω=Δθ/Δt...";
   S3 := "une soirée passée à étudier les mathématiques ℕ⊂𝕂...";
   Put_Line (S1 & Line_Mark & S2 & Line_Mark & S3);
   C   := S1 (6);
   WC  := S1 (7);
   WWC := S1 (1);
   Put_Line (From_Latin_1(Image(Character'pos (C), 16) & ',' & Image(Wide_Character'pos (WC), 16)  & ','& Image(Wide_Wide_Character'pos (WWC), 16)));
   for I in S3 loop
      F   := S3 (I) = Character'('é');
      if F then
         S2 (I) := Character'('e');
      end if;
      WWC := S3 (I);
      Put_Line (From_Latin_1(I'Image & ':' & Image(Wide_Wide_Character'pos (WWC), 16) & ',' & F'Image));
   end loop;
   for CC : Wide_Wide_Character of S2 loop
      WWC := CC;
      F   := CC = 'é';
      Put_Line (From_Latin_1(Image(Wide_Wide_Character'pos (WWC), 16)  & ','& F'img));
   end loop;
--     S1 (3) := WWC; -- discriminant check failed
--     S1 (2) := WC; -- discriminant check failed
   S1 (1) := C;
   if S1 /= "test" then
      S1 := Null_UXString;
      S2 := 2 * 'z';
      S3 := 4 * "po";
   end if;
   S3 := "Riri";
   S2 := "Loulou";
   S1 := " et Fifi";
   S2.Append (S1);
   S1.Prepend(S3);
   Put_Line (S1 & Line_Mark & S2 & Line_Mark & S3);
end Test_UXStrings;

