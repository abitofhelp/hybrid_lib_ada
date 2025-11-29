pragma Ada_2022;
--  ======================================================================
--  Unit_Runner - Main test runner for unit tests
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Runs all unit tests and reports cumulative results.
--  ======================================================================

with Ada.Command_Line;
with Ada.Text_IO;
with Test_Framework;

--  Import all test procedures
with Test_Domain_Error_Result;
with Test_Domain_Person;
with Test_Application_Command_Greet;
with Test_Application_Usecase_Greet;
with Test_API_Operations;

procedure Unit_Runner is

   use Ada.Text_IO;
   use Ada.Command_Line;

   Total  : Natural;
   Passed : Natural;

begin
   Put_Line ("");
   Put_Line ("========================================");
   Put_Line ("     HYBRID_LIB_ADA UNIT TEST SUITE");
   Put_Line ("========================================");
   Put_Line ("");

   --  Reset test framework before running tests
   Test_Framework.Reset;

   --  Run all unit test procedures
   --  Each test registers its results with Test_Framework

   Test_Domain_Error_Result;
   Test_Domain_Person;
   Test_Application_Command_Greet;
   Test_Application_Usecase_Greet;
   Test_API_Operations;

   --  Get cumulative results
   Total  := Test_Framework.Grand_Total_Tests;
   Passed := Test_Framework.Grand_Total_Passed;

   --  Print grand summary
   Put_Line ("");
   Put_Line ("========================================");
   Put_Line ("        GRAND TOTAL - ALL UNIT TESTS");
   Put_Line ("========================================");
   Put_Line ("Total tests:  " & Total'Image);
   Put_Line ("Passed:       " & Passed'Image);
   Put_Line ("Failed:       " & Natural'Image (Total - Passed));

   --  Print professional color-coded summary and get exit status
   declare
      Exit_Code : constant Integer :=
        Test_Framework.Print_Category_Summary ("UNIT TESTS", Total, Passed);
   begin
      Set_Exit_Status (if Exit_Code = 0 then Success else Failure);
   end;

end Unit_Runner;
