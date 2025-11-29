pragma Ada_2022;
with Domain;
--  ======================================================================
--  Test_Domain_Error_Result
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit tests for Domain.Error.Result monad functionality.
--    Tests Ok/Error constructors, Is_Ok/Is_Error queries, and then value
--    extraction.
--  ======================================================================

with Ada.Text_IO;
with Domain.Error;
with Domain.Error.Result;
with Test_Framework;

procedure Test_Domain_Error_Result is

   use Ada.Text_IO;
   use Domain.Error;

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

   --  Instantiate Result for Integer (for testing)
   package Int_Result is new Domain.Error.Result.Generic_Result (T => Integer);

   --  Instantiate Result for Boolean (for testing)
   package Bool_Result is new
     Domain.Error.Result.Generic_Result (T => Boolean);

begin
   Put_Line ("========================================");
   Put_Line ("Testing: Domain.Error.Result");
   Put_Line ("========================================");
   New_Line;

   --  ========================================================================
   --  Test: Ok construction and then Is_Ok query
   --  ========================================================================

   declare
      R : constant Int_Result.Result := Int_Result.Ok (42);
   begin
      Run_Test
        ("Ok construction - Is_Ok returns true", Int_Result.Is_Ok (R));
      Run_Test
        ("Ok construction - Is_Error returns false",
         not Int_Result.Is_Error (R));
   end;

   --  ========================================================================
   --  Test: Ok value extraction
   --  ========================================================================

   declare
      R   : constant Int_Result.Result := Int_Result.Ok (123);
      Val : Integer;
   begin
      if Int_Result.Is_Ok (R) then
         Val := Int_Result.Value (R);
         Run_Test ("Ok value extraction - correct value", Val = 123);
      else
         Run_Test ("Ok value extraction - Result should be Ok", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Error construction and then Is_Error query
   --  ========================================================================

   declare
      R : constant Int_Result.Result :=
        Int_Result.Error
          (Kind => Validation_Error, Message => "Test validation error");
   begin
      Run_Test
        ("Error construction - Is_Error returns true",
         Int_Result.Is_Error (R));
      Run_Test
        ("Error construction - Is_Ok returns false", not Int_Result.Is_Ok (R));
   end;

   --  ========================================================================
   --  Test: Error info extraction
   --  ========================================================================

   declare
      R    : constant Int_Result.Result :=
        Int_Result.Error
          (Kind => IO_Error, Message => "Test IO error");
      Info : Error_Type;
   begin
      if Int_Result.Is_Error (R) then
         Info := Int_Result.Error_Info (R);
         Run_Test
           ("Error info - correct kind", Info.Kind = IO_Error);
         Run_Test
           ("Error info - correct message",
            Error_Strings.To_String (Info.Message) = "Test IO error");
      else
         Run_Test ("Error info extraction - Result should be Error", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Result with Boolean type
   --  ========================================================================

   declare
      R : constant Bool_Result.Result := Bool_Result.Ok (True);
   begin
      Run_Test
        ("Boolean Result - Is_Ok returns true", Bool_Result.Is_Ok (R));
      if Bool_Result.Is_Ok (R) then
         Run_Test
           ("Boolean Result - correct value",
            Bool_Result.Value (R) = True);
      end if;
   end;

   --  ========================================================================
   --  Test: Error with empty message
   --  ========================================================================

   declare
      R    : constant Int_Result.Result :=
        Int_Result.Error (Kind => Validation_Error, Message => "");
      Info : Error_Type;
   begin
      Run_Test
        ("Error with empty message - Is_Error", Int_Result.Is_Error (R));
      if Int_Result.Is_Error (R) then
         Info := Int_Result.Error_Info (R);
         Run_Test
           ("Error with empty message - message is empty",
            Error_Strings.Length (Info.Message) = 0);
      end if;
   end;

   --  ========================================================================
   --  Test: Multiple Ok values don't interfere
   --  ========================================================================

   declare
      R1 : constant Int_Result.Result := Int_Result.Ok (100);
      R2 : constant Int_Result.Result := Int_Result.Ok (200);
   begin
      Run_Test
        ("Multiple Ok values - R1 has correct value",
         Int_Result.Is_Ok (R1) and then Int_Result.Value (R1) = 100);
      Run_Test
        ("Multiple Ok values - R2 has correct value",
         Int_Result.Is_Ok (R2) and then Int_Result.Value (R2) = 200);
   end;

   --  ========================================================================
   --  Test: Multiple Error values don't interfere
   --  ========================================================================

   declare
      R1   : constant Int_Result.Result :=
        Int_Result.Error (Kind => Validation_Error, Message => "Error 1");
      R2   : constant Int_Result.Result :=
        Int_Result.Error (Kind => IO_Error, Message => "Error 2");
      Info1 : Error_Type;
      Info2 : Error_Type;
   begin
      if Int_Result.Is_Error (R1) and then Int_Result.Is_Error (R2) then
         Info1 := Int_Result.Error_Info (R1);
         Info2 := Int_Result.Error_Info (R2);
         Run_Test
           ("Multiple errors - R1 has correct kind",
            Info1.Kind = Validation_Error);
         Run_Test
           ("Multiple errors - R1 has correct message",
            Error_Strings.To_String (Info1.Message) = "Error 1");
         Run_Test
           ("Multiple errors - R2 has correct kind",
            Info2.Kind = IO_Error);
         Run_Test
           ("Multiple errors - R2 has correct message",
            Error_Strings.To_String (Info2.Message) = "Error 2");
      else
         Run_Test ("Multiple errors test failed", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Long error message (boundary test)
   --  ========================================================================

   declare
      Long_Message : constant String (1 .. 500) := [others => 'X'];
      R            : constant Int_Result.Result :=
        Int_Result.Error (Kind => Validation_Error, Message => Long_Message);
      Info : Error_Type;
   begin
      Run_Test
        ("Long error message - Is_Error", Int_Result.Is_Error (R));
      if Int_Result.Is_Error (R) then
         Info := Int_Result.Error_Info (R);
         --  Message should be truncated to max length (512)
         Run_Test
           ("Long error message - message stored",
            Error_Strings.Length (Info.Message) > 0);
      end if;
   end;

   --  Print summary
   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: Domain.Error.Result");
   Put_Line ("========================================");
   Put_Line ("Total tests: " & Total_Tests'Image);
   Put_Line ("Passed:      " & Passed_Tests'Image);
   Put_Line ("Failed:      " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   --  Register results with test framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);

end Test_Domain_Error_Result;
