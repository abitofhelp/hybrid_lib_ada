pragma Ada_2022;
with Domain;
--  ======================================================================
--  Test_Domain_Person
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit tests for Domain.Value_Object.Person value object.
--    Tests Create validation and Get_Name accessor.
--    (Greeting format moved to Application layer)
--  ======================================================================

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Domain.Error;
with Domain.Value_Object.Person;
with Test_Framework;

procedure Test_Domain_Person is

   use Ada.Text_IO;
   use Domain.Error;
   use Domain.Value_Object.Person;

   --  Test statistics
   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;

   --  Helper procedure to run a test
   pragma Style_Checks (Off);
   procedure Run_Test (Name : String; Passed : Boolean) is
   begin
      Total_Tests := Total_Tests + 1;
      if Passed then
         Passed_Tests := Passed_Tests + 1;
         Put_Line ("[PASS] " & Name);
      else
         Put_Line ("[FAIL] " & Name);
      end if;
   end Run_Test;
   pragma Style_Checks (On);

begin
   Put_Line ("========================================");
   Put_Line ("Testing: Domain.Value_Object.Person");
   Put_Line ("========================================");
   New_Line;

   --  ========================================================================
   --  Test: Create with valid name
   --  ========================================================================

   declare
      Result : constant Person_Result.Result := Create ("Alice");
   begin
      Run_Test ("Create valid name - Is_Ok", Person_Result.Is_Ok (Result));
      if Person_Result.Is_Ok (Result) then
         declare
            P : constant Person := Person_Result.Value (Result);
         begin
            Run_Test
              ("Create valid name - Get_Name correct", Get_Name (P) = "Alice");
         end;
      end if;
   end;

   --  ========================================================================
   --  Test: Create with empty name (validation failure)
   --  ========================================================================

   declare
      Result : constant Person_Result.Result := Create ("");
   begin
      Run_Test
        ("Create empty name - Is_Error", Person_Result.Is_Error (Result));
      if Person_Result.Is_Error (Result) then
         declare
            Info : constant Error_Type := Person_Result.Error_Info (Result);
         begin
            Run_Test
              ("Create empty name - error kind is Validation_Error",
               Info.Kind = Validation_Error);
            Run_Test
              ("Create empty name - error message contains 'empty'",
               Ada.Strings.Fixed.Index
                 (Error_Strings.To_String (Info.Message), "empty") > 0);
         end;
      end if;
   end;

   --  ========================================================================
   --  Test: Create with name exceeding max length (validation failure)
   --  ========================================================================

   declare
      Long_Name :
        constant String (1 .. Max_Name_Length + 1) := [others => 'X'];
      Result : constant Person_Result.Result := Create (Long_Name);
   begin
      Run_Test
        ("Create name too long - Is_Error", Person_Result.Is_Error (Result));
      if Person_Result.Is_Error (Result) then
         declare
            Info : constant Error_Type := Person_Result.Error_Info (Result);
         begin
            Run_Test
              ("Create name too long - error kind is Validation_Error",
               Info.Kind = Validation_Error);
            Run_Test
              ("Create name too long - error message contains 'maximum'",
               Ada.Strings.Fixed.Index
                 (Error_Strings.To_String (Info.Message), "maximum") > 0
                 or else
               Ada.Strings.Fixed.Index
                 (Error_Strings.To_String (Info.Message), "exceeds") > 0);
         end;
      end if;
   end;

   --  ========================================================================
   --  Test: Create with name at max length (boundary test)
   --  ========================================================================

   declare
      Max_Length_Name :
        constant String (1 .. Max_Name_Length) := [others => 'A'];
      Result :
        constant Person_Result.Result := Create (Max_Length_Name);
   begin
      Run_Test
        ("Create name at max length - Is_Ok", Person_Result.Is_Ok (Result));
      if Person_Result.Is_Ok (Result) then
         declare
            P : constant Person := Person_Result.Value (Result);
         begin
            Run_Test
              ("Create name at max length - correct length",
               Get_Name (P)'Length = Max_Name_Length);
         end;
      end if;
   end;

   --  ========================================================================
   --  Test: Create with single character name
   --  ========================================================================

   declare
      Result : constant Person_Result.Result := Create ("X");
   begin
      Run_Test
        ("Create single char name - Is_Ok", Person_Result.Is_Ok (Result));
      if Person_Result.Is_Ok (Result) then
         declare
            P : constant Person := Person_Result.Value (Result);
         begin
            Run_Test
              ("Create single char name - Get_Name correct",
               Get_Name (P) = "X");
         end;
      end if;
   end;

   --  ========================================================================
   --  Test: Create with name containing spaces
   --  ========================================================================

   declare
      Result : constant Person_Result.Result := Create ("Bob Smith");
   begin
      Run_Test
        ("Create name with spaces - Is_Ok", Person_Result.Is_Ok (Result));
      if Person_Result.Is_Ok (Result) then
         declare
            P : constant Person := Person_Result.Value (Result);
         begin
            Run_Test
              ("Create name with spaces - Get_Name correct",
               Get_Name (P) = "Bob Smith");
         end;
      end if;
   end;

   --  ========================================================================
   --  Test: Create with name containing special characters
   --  ========================================================================

   declare
      Result : constant Person_Result.Result := Create ("Anne-Marie O'Brien");
   begin
      Run_Test
        ("Create name with special chars - Is_Ok",
         Person_Result.Is_Ok (Result));
      if Person_Result.Is_Ok (Result) then
         declare
            P : constant Person := Person_Result.Value (Result);
         begin
            Run_Test
              ("Create name with special chars - Get_Name correct",
               Get_Name (P) = "Anne-Marie O'Brien");
         end;
      end if;
   end;

   --  ========================================================================
   --  Test: Is_Valid_Person predicate
   --  ========================================================================

   declare
      Result : constant Person_Result.Result := Create ("Valid Person");
   begin
      if Person_Result.Is_Ok (Result) then
         declare
            P : constant Person := Person_Result.Value (Result);
         begin
            Run_Test
              ("Is_Valid_Person - valid person returns true",
               Is_Valid_Person (P));
         end;
      else
         Run_Test ("Is_Valid_Person test setup failed", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Multiple Person instances don't interfere
   --  ========================================================================

   declare
      Result1 : constant Person_Result.Result := Create ("Person1");
      Result2 : constant Person_Result.Result := Create ("Person2");
      Result3 : constant Person_Result.Result := Create ("Person3");
   begin
      if Person_Result.Is_Ok (Result1) and then Person_Result.Is_Ok (Result2)
         and then Person_Result.Is_Ok (Result3)
      then
         declare
            P1 : constant Person := Person_Result.Value (Result1);
            P2 : constant Person := Person_Result.Value (Result2);
            P3 : constant Person := Person_Result.Value (Result3);
         begin
            Run_Test
              ("Multiple instances - P1 correct", Get_Name (P1) = "Person1");
            Run_Test
              ("Multiple instances - P2 correct", Get_Name (P2) = "Person2");
            Run_Test
              ("Multiple instances - P3 correct", Get_Name (P3) = "Person3");
         end;
      else
         Run_Test ("Multiple instances test setup failed", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Create with Unicode characters (if supported)
   --  ========================================================================

   declare
      Result : constant Person_Result.Result := Create ("José María");
   begin
      Run_Test ("Create with Unicode - Is_Ok", Person_Result.Is_Ok (Result));
      if Person_Result.Is_Ok (Result) then
         declare
            P : constant Person := Person_Result.Value (Result);
         begin
            Run_Test
              ("Create with Unicode - Get_Name correct",
               Get_Name (P) = "José María");
         end;
      end if;
   end;

   --  Print summary
   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: Domain.Value_Object.Person");
   Put_Line ("========================================");
   Put_Line ("Total tests: " & Total_Tests'Image);
   Put_Line ("Passed:      " & Passed_Tests'Image);
   Put_Line ("Failed:      " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   --  Register results with test framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);

end Test_Domain_Person;
