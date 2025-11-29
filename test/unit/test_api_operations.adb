pragma Ada_2022;
--  ======================================================================
--  Test_API_Operations
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit tests for Hybrid_Lib_Ada.API.Operations generic package.
--    Tests the SPARK-safe operations in isolation using a mock Writer.
--  ======================================================================

with Ada.Strings.Bounded;
with Ada.Text_IO;
with Application.Command.Greet;
with Application.Port.Outbound.Writer;
with Domain.Error;
with Domain.Unit;
with Hybrid_Lib_Ada.API.Operations;
with Test_Framework;

procedure Test_API_Operations is

   use Ada.Text_IO;
   use Application.Port.Outbound.Writer;
   use Domain.Error;
   use Domain.Unit;

   --  Bounded string for capturing mock output
   package Message_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max => 256);
   use Message_Strings;

   --  State for mock writer
   Captured_Message : Bounded_String := Null_Bounded_String;
   Write_Call_Count : Natural := 0;
   Mock_Should_Fail : Boolean := False;

   --  ========================================================================
   --  Mock Writer Implementation
   --  ========================================================================

   pragma Style_Checks (Off);
   function Mock_Writer
     (Message : String)
      return Unit_Result.Result
   is
   begin
      Write_Call_Count := Write_Call_Count + 1;

      if Mock_Should_Fail then
         return Unit_Result.Error
           (Kind    => IO_Error,
            Message => "Mock I/O failure");
      end if;

      Captured_Message := To_Bounded_String (Message);
      return Unit_Result.Ok (Unit_Value);
   end Mock_Writer;
   pragma Style_Checks (On);

   --  Reset mock state between tests
   procedure Reset_Mock is
   begin
      Captured_Message := Null_Bounded_String;
      Write_Call_Count := 0;
      Mock_Should_Fail := False;
   end Reset_Mock;

   --  Instantiate API.Operations with mock writer
   package Test_Ops is new Hybrid_Lib_Ada.API.Operations
     (Writer => Mock_Writer);

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
   Put_Line ("Testing: Hybrid_Lib_Ada.API.Operations");
   Put_Line ("        (with Mock Writer)");
   Put_Line ("========================================");
   New_Line;

   --  ========================================================================
   --  Test: Greet with valid name produces correct message
   --  ========================================================================

   Reset_Mock;
   declare
      Cmd    : constant Application.Command.Greet.Greet_Command :=
        Application.Command.Greet.Create ("Alice");
      Result : constant Unit_Result.Result := Test_Ops.Greet (Cmd);
   begin
      Run_Test ("Greet 'Alice' - Is_Ok", Unit_Result.Is_Ok (Result));
      Run_Test ("Greet 'Alice' - Writer called once", Write_Call_Count = 1);
      Run_Test
        ("Greet 'Alice' - Message is 'Hello, Alice!'",
         To_String (Captured_Message) = "Hello, Alice!");
   end;

   --  ========================================================================
   --  Test: Greet with another name
   --  ========================================================================

   Reset_Mock;
   declare
      Cmd    : constant Application.Command.Greet.Greet_Command :=
        Application.Command.Greet.Create ("Bob");
      Result : constant Unit_Result.Result := Test_Ops.Greet (Cmd);
   begin
      Run_Test ("Greet 'Bob' - Is_Ok", Unit_Result.Is_Ok (Result));
      Run_Test
        ("Greet 'Bob' - Message is 'Hello, Bob!'",
         To_String (Captured_Message) = "Hello, Bob!");
   end;

   --  ========================================================================
   --  Test: Greet with name containing spaces
   --  ========================================================================

   Reset_Mock;
   declare
      Cmd    : constant Application.Command.Greet.Greet_Command :=
        Application.Command.Greet.Create ("Jane Doe");
      Result : constant Unit_Result.Result := Test_Ops.Greet (Cmd);
   begin
      Run_Test ("Greet 'Jane Doe' - Is_Ok", Unit_Result.Is_Ok (Result));
      Run_Test
        ("Greet 'Jane Doe' - Message correct",
         To_String (Captured_Message) = "Hello, Jane Doe!");
   end;

   --  ========================================================================
   --  Test: Greet with special characters
   --  ========================================================================

   Reset_Mock;
   declare
      Cmd    : constant Application.Command.Greet.Greet_Command :=
        Application.Command.Greet.Create ("Anne-Marie O'Brien");
      Result : constant Unit_Result.Result := Test_Ops.Greet (Cmd);
   begin
      Run_Test ("Greet special chars - Is_Ok", Unit_Result.Is_Ok (Result));
      Run_Test
        ("Greet special chars - Message correct",
         To_String (Captured_Message) = "Hello, Anne-Marie O'Brien!");
   end;

   --  ========================================================================
   --  Test: Writer failure propagates as error
   --  ========================================================================

   Reset_Mock;
   Mock_Should_Fail := True;
   declare
      Cmd    : constant Application.Command.Greet.Greet_Command :=
        Application.Command.Greet.Create ("Test");
      Result : constant Unit_Result.Result := Test_Ops.Greet (Cmd);
   begin
      Run_Test ("Greet with failing writer - Is_Error",
                Unit_Result.Is_Error (Result));
      if Unit_Result.Is_Error (Result) then
         declare
            Info : constant Error_Type := Unit_Result.Error_Info (Result);
         begin
            Run_Test ("Greet with failing writer - IO_Error kind",
                      Info.Kind = IO_Error);
         end;
      end if;
   end;

   --  ========================================================================
   --  Test: Multiple greet calls accumulate correctly
   --  ========================================================================

   Reset_Mock;
   declare
      Cmd1   : constant Application.Command.Greet.Greet_Command :=
        Application.Command.Greet.Create ("First");
      Cmd2   : constant Application.Command.Greet.Greet_Command :=
        Application.Command.Greet.Create ("Second");
      Result1 : constant Unit_Result.Result := Test_Ops.Greet (Cmd1);
      Result2 : constant Unit_Result.Result := Test_Ops.Greet (Cmd2);
   begin
      Run_Test ("Multiple calls - Both succeed",
                Unit_Result.Is_Ok (Result1) and then Unit_Result.Is_Ok (Result2));
      Run_Test ("Multiple calls - Writer called twice", Write_Call_Count = 2);
      Run_Test ("Multiple calls - Last message captured",
                To_String (Captured_Message) = "Hello, Second!");
   end;

   --  Print summary
   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: API.Operations");
   Put_Line ("========================================");
   Put_Line ("Total tests: " & Total_Tests'Image);
   Put_Line ("Passed:      " & Passed_Tests'Image);
   Put_Line ("Failed:      " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   --  Register results with test framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);

end Test_API_Operations;
