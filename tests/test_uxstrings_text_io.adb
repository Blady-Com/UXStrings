pragma Wide_Character_Encoding (UTF8);
with UXStrings;         use UXStrings;
with UXStrings.Text_IO; use UXStrings.Text_IO;
with UXStrings.Text_IO.Text_Streams; use UXStrings.Text_IO.Text_Streams;
with UXStrings.Conversions;

procedure Test_UXStrings_Text_IO is
   function Image is new UXStrings.Conversions.Scalar_Image (Encoding_Scheme);
   function Value is new UXStrings.Conversions.Scalar_Value (Encoding_Scheme);

   procedure Write (Encoding : Encoding_Scheme) is
      F : File_Type;
   begin
      Create (F,Out_File, "test_" & Image (Encoding) & ".txt", Encoding);
      Put_BOM (F);
      Put_Line (F,"Test_" & Image (Encoding));
      Put_Line (F,"une soirée passée à étudier la physique ω=Δθ/Δt...");
      Put_Line (F,"une soirée passée à étudier les mathématiques ℕ⊂𝕂...");
      Put (F,"Test_End");
      Close (F);
      Put_Line ("File written.");
   end;

   procedure Read (Encoding : Encoding_Scheme) is
      F : File_Type;
   begin
      Open (F, In_File, "test_" & Image(Encoding) & ".txt", Encoding);
      while not End_Of_File(F) loop
         Put_Line (Get_Line (F));
      end loop;
      Close (F);
      Put_Line ("File read.");
   end;

   procedure Get (Encoding : Encoding_Scheme) is
      F : File_Type;
      Ch : Unicode_Character;
   begin
      Open (F, In_File, "test_" & Image(Encoding) & ".txt", Encoding);
      while not End_Of_File(F) loop
         if End_Of_Line(F) then
            Put_line ("<EOL>");
            Skip_Line(F);
         else
            Get (F, Ch);
            Put (Ch);
         end if;
      end loop;
      Close (F);
      Put_Line ("File with get.");
   end;

   procedure Write_Stream is
      F : File_Type;
      S : Stream_Access;
   begin
      Create (F,Out_File, "test_stream.txt", Latin_1);
      S := Stream (F);
      for C of To_Latin_1 ("une soirée passée à étudier la physique ω=Δθ/Δt...") loop
         Character'Write (S, C);
      end loop;
      Close (F);
      Put_Line ("File written.");
   end;

   procedure Read_Stream is
      F : File_Type;
      T : Latin_1_Character_Array (1..40);
      S : Stream_Access;
   begin
      Open (F, In_File, "test_stream.txt", Latin_1);
      S := Stream (F);
      Latin_1_Character_Array'Read (S, T);
      Put_Line (From_Latin_1 (T));
      Close (F);
      Put_Line ("File read.");
   end;

   procedure Read_Text is
      F : File_Type;
   begin
      Open (F, In_File, "test_UTF_8.txt", UTF_8);
      Put_Text (Get_Text (F));
      Close (F);
      Put_Line ("Text read.");
   end;

   S1 : UXString;

begin
   -- Change the default to LF and UTF-8
   Ending (Current_Output, LF_Ending);
   Line_Mark (LF_Ending);
   Scheme (Current_Output, UTF_8);
   Ending (Current_Input, LF_Ending);
   Scheme (Current_Input, UTF_8);
   loop
      Put ("-->");
      Get_Line (S1);
      Put_Line (S1);
      exit when S1 = "exit";
      if S1.Index ("fwrite") = S1.First then
         Write (if S1.index ("utf_") > 0 then Value (S1.Slice (8, S1.Length)) else Latin_1);
      end if;
      if S1.Index ("fread") = S1.First then
         Read(if S1.index ("utf_") > 0 then Value (S1.Slice (7, S1.Length)) else Latin_1);
      end if;
      if S1.Index ("fget") = S1.First then
         Get(if S1.index ("utf_") > 0 then Value (S1.Slice (6, S1.Length)) else Latin_1);
      end if;
      if S1.Index ("swrite")= S1.First then
         Write_Stream;
      end if;
      if S1.Index ("sread")= S1.First then
         Read_Stream;
      end if;
      if S1.Index ("rtext")= S1.First then
         Read_Text;
      end if;
   end loop;
   Put_Line ("<-->");
end Test_UXStrings_Text_IO;
