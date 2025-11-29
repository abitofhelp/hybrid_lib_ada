pragma Ada_2022;
--  ======================================================================
--  Test_API_Greet
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Integration tests for Hybrid_Lib_Ada.API.Greet operation.
--    Tests the complete flow through API facade → API.Desktop → API.Operations
--    → Application.Usecase → Domain with real Console_Writer adapter.
--  ======================================================================

with Ada.Text_IO;
with Domain.Error;
with Hybrid_Lib_Ada.API;
with Test_Framework;

procedure Test_API_Greet is

   use Ada.Text_IO;
   use Domain.Error;
   use Hybrid_Lib_Ada.API;

   --  Test statistics
   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;

   --  Helper procedure to run a test
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

begin
   Put_Line ("========================================");
   Put_Line ("Testing: Hybrid_Lib_Ada.API.Greet");
   Put_Line ("========================================");
   New_Line;

   --  ========================================================================
   --  Test: API.Greet with valid name
   --  ========================================================================

   declare
      Cmd    : constant Greet_Command := Create_Greet_Command ("Alice");
      Result : constant Unit_Result.Result := Greet (Cmd);
   begin
      Run_Test ("API.Greet valid name - Is_Ok", Unit_Result.Is_Ok (Result));
   end;

   --  ========================================================================
   --  Test: API.Greet with another valid name
   --  ========================================================================

   declare
      Cmd    : constant Greet_Command := Create_Greet_Command ("Bob");
      Result : constant Unit_Result.Result := Greet (Cmd);
   begin
      Run_Test ("API.Greet 'Bob' - Is_Ok", Unit_Result.Is_Ok (Result));
   end;

   --  ========================================================================
   --  Test: API.Greet with name containing spaces
   --  ========================================================================

   declare
      Cmd    : constant Greet_Command := Create_Greet_Command ("Jane Doe");
      Result : constant Unit_Result.Result := Greet (Cmd);
   begin
      Run_Test ("API.Greet 'Jane Doe' - Is_Ok", Unit_Result.Is_Ok (Result));
   end;

   --  ========================================================================
   --  Test: API.Greet with special characters
   --  ========================================================================

   declare
      Cmd : constant Greet_Command :=
        Create_Greet_Command ("Anne-Marie O'Brien");
      Result : constant Unit_Result.Result := Greet (Cmd);
   begin
      Run_Test ("API.Greet special chars - Is_Ok", Unit_Result.Is_Ok (Result));
   end;

   --  ========================================================================
   --  Test: API.Greet with Unicode characters
   --  ========================================================================

   declare
      Cmd    : constant Greet_Command := Create_Greet_Command ("José María");
      Result : constant Unit_Result.Result := Greet (Cmd);
   begin
      Run_Test ("API.Greet Unicode - Is_Ok", Unit_Result.Is_Ok (Result));
   end;

   --  ========================================================================
   --  Test: API.Create_Greet_Command and Get_Command_Name round-trip
   --  ========================================================================

   declare
      Cmd : constant Greet_Command := Create_Greet_Command ("TestUser");
   begin
      Run_Test
        ("Create_Greet_Command round-trip",
         Get_Command_Name (Cmd) = "TestUser");
   end;

   --  ========================================================================
   --  Test: API.Create_Person and Get_Name round-trip
   --  ========================================================================

   declare
      Result : constant Person_Result.Result := Create_Person ("PersonTest");
   begin
      Run_Test ("Create_Person valid - Is_Ok", Person_Result.Is_Ok (Result));
      if Person_Result.Is_Ok (Result) then
         declare
            P : constant Person_Type := Person_Result.Value (Result);
         begin
            Run_Test ("Create_Person round-trip", Get_Name (P) = "PersonTest");
         end;
      end if;
   end;

   --  ========================================================================
   --  Test: API.Create_Person with empty name (validation failure)
   --  ========================================================================

   declare
      Result : constant Person_Result.Result := Create_Person ("");
   begin
      Run_Test
        ("Create_Person empty - Is_Error", Person_Result.Is_Error (Result));
      if Person_Result.Is_Error (Result) then
         declare
            Info : constant Domain.Error.Error_Type :=
              Person_Result.Error_Info (Result);
         begin
            Run_Test
              ("Create_Person empty - Validation_Error",
               Info.Kind = Validation_Error);
         end;
      end if;
   end;

   --  Print summary
   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: Hybrid_Lib_Ada.API.Greet");
   Put_Line ("========================================");
   Put_Line ("Total tests: " & Total_Tests'Image);
   Put_Line ("Passed:      " & Passed_Tests'Image);
   Put_Line ("Failed:      " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   --  Register results with test framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);

end Test_API_Greet;
