pragma Ada_2022;
with Hybrid_Lib_Ada.Domain;
--  ======================================================================
--  Test_Application_Usecase_Greet
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit tests for Hybrid_Lib_Ada.Application.Usecase.Greet use case.
--    Tests use case execution with mock writer port.
--  ======================================================================

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Hybrid_Lib_Ada.Application.Command.Greet;
with Hybrid_Lib_Ada.Application.Port.Outbound.Writer;
with Hybrid_Lib_Ada.Application.Usecase.Greet;
with Hybrid_Lib_Ada.Domain.Error;
with Hybrid_Lib_Ada.Domain.Unit;
with Hybrid_Lib_Ada.Domain.Value_Object.Person;
with Test_Framework;

procedure Test_Application_Usecase_Greet is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Hybrid_Lib_Ada.Application.Command.Greet;
   use Hybrid_Lib_Ada.Application.Port.Outbound.Writer;
   use Hybrid_Lib_Ada.Domain.Unit;
   use Hybrid_Lib_Ada.Domain.Error;

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
         Test_Framework.Print_Pass (Name);
      else
         Test_Framework.Print_Fail (Name);
      end if;
   end Run_Test;
   pragma Style_Checks (On);

   --  ========================================================================
   --  Mock Writer Implementation (Success)
   --  ========================================================================

   --  Capture the last written message for testing
   Last_Written_Message : Unbounded_String;

   pragma Style_Checks (Off);
   function Mock_Writer_Success (Message : String) return Unit_Result.Result is
   begin
      Last_Written_Message := To_Unbounded_String (Message);
      return Unit_Result.Ok (Unit_Value);
   end Mock_Writer_Success;
   pragma Style_Checks (On);

   --  Instantiate use case with success writer
   package Greet_UseCase_Success is new Hybrid_Lib_Ada.Application.Usecase.Greet
     (Writer => Mock_Writer_Success);

   --  ========================================================================
   --  Mock Writer Implementation (Failure)
   --  ========================================================================

   pragma Style_Checks (Off);
   function Mock_Writer_Failure (Message : String) return Unit_Result.Result is
      pragma Unreferenced (Message);
   begin
      return
        Unit_Result.Error
          (Kind    => IO_Error,
           Message => "Mock writer IO failure");
   end Mock_Writer_Failure;
   pragma Style_Checks (On);

   --  Instantiate use case with failure writer
   package Greet_UseCase_Failure is new Hybrid_Lib_Ada.Application.Usecase.Greet
     (Writer => Mock_Writer_Failure);

begin
   Put_Line ("========================================");
   Put_Line ("Testing: Hybrid_Lib_Ada.Application.Usecase.Greet");
   Put_Line ("========================================");
   New_Line;

   --  ========================================================================
   --  Test: Execute with valid name (success path)
   --  ========================================================================

   declare
      Cmd    : constant Greet_Command := Create ("Alice");
      Result : constant Unit_Result.Result :=
        Greet_UseCase_Success.Execute (Cmd);
   begin
      Run_Test
        ("Execute valid name - Is_Ok", Unit_Result.Is_Ok (Result));
      Run_Test
        ("Execute valid name - message written",
         To_String (Last_Written_Message) = "Hello, Alice!");
   end;

   --  ========================================================================
   --  Test: Execute with different valid names
   --  ========================================================================

   declare
      Cmd1    : constant Greet_Command := Create ("Bob");
      Result1 : constant Unit_Result.Result :=
        Greet_UseCase_Success.Execute (Cmd1);
   begin
      Run_Test
        ("Execute different name (Bob) - Is_Ok", Unit_Result.Is_Ok (Result1));
      Run_Test
        ("Execute different name (Bob) - correct message",
         To_String (Last_Written_Message) = "Hello, Bob!");
   end;

   declare
      Cmd2    : constant Greet_Command := Create ("Carol Smith");
      Result2 : constant Unit_Result.Result :=
        Greet_UseCase_Success.Execute (Cmd2);
   begin
      Run_Test
        ("Execute name with space - Is_Ok", Unit_Result.Is_Ok (Result2));
      Run_Test
        ("Execute name with space - correct message",
         To_String (Last_Written_Message) = "Hello, Carol Smith!");
   end;

   --  ========================================================================
   --  Test: Execute with writer failure (infrastructure error)
   --  ========================================================================

   declare
      Cmd    : constant Greet_Command := Create ("Dave");
      Result : constant Unit_Result.Result :=
        Greet_UseCase_Failure.Execute (Cmd);
   begin
      Run_Test
        ("Execute with writer failure - Is_Error",
         Unit_Result.Is_Error (Result));
      if Unit_Result.Is_Error (Result) then
         declare
            Info : constant Error_Type := Unit_Result.Error_Info (Result);
         begin
            Run_Test
              ("Execute with writer failure - error kind is IO_Error",
               Info.Kind = IO_Error);
            Run_Test
              ("Execute with writer failure - error message contains 'Mock'",
               Ada.Strings.Fixed.Index
                 (Error_Strings.To_String (Info.Message), "Mock") > 0);
         end;
      end if;
   end;

   --  ========================================================================
   --  Test: Execute with special characters in name
   --  ========================================================================

   declare
      Cmd    : constant Greet_Command := Create ("Anne-Marie O'Brien");
      Result : constant Unit_Result.Result :=
        Greet_UseCase_Success.Execute (Cmd);
   begin
      Run_Test
        ("Execute with special chars - Is_Ok", Unit_Result.Is_Ok (Result));
      Run_Test
        ("Execute with special chars - correct message",
         To_String (Last_Written_Message) = "Hello, Anne-Marie O'Brien!");
   end;

   --  ========================================================================
   --  Test: Execute with Unicode characters
   --  ========================================================================

   declare
      Cmd    : constant Greet_Command := Create ("José María");
      Result : constant Unit_Result.Result :=
        Greet_UseCase_Success.Execute (Cmd);
   begin
      Run_Test ("Execute with Unicode - Is_Ok", Unit_Result.Is_Ok (Result));
      Run_Test
        ("Execute with Unicode - correct message",
         To_String (Last_Written_Message) = "Hello, José María!");
   end;

   --  ========================================================================
   --  Test: Execute with max length name
   --  ========================================================================

   declare
      use Hybrid_Lib_Ada.Domain.Value_Object.Person;
      Max_Name : constant String (1 .. Max_Name_Length) := [others => 'X'];
      Cmd      : constant Greet_Command := Create (Max_Name);
      Result   : constant Unit_Result.Result :=
        Greet_UseCase_Success.Execute (Cmd);
      Expected : constant String := "Hello, " & Max_Name & "!";
   begin
      Run_Test ("Execute with max length - Is_Ok", Unit_Result.Is_Ok (Result));
      Run_Test
        ("Execute with max length - correct message",
         To_String (Last_Written_Message) = Expected);
   end;

   --  ========================================================================
   --  Test: Execute multiple times (stateless behavior)
   --  ========================================================================

   declare
      Cmd1    : constant Greet_Command := Create ("First");
      Result1 : constant Unit_Result.Result :=
        Greet_UseCase_Success.Execute (Cmd1);
      Msg1    : constant String := To_String (Last_Written_Message);

      Cmd2    : constant Greet_Command := Create ("Second");
      Result2 : constant Unit_Result.Result :=
        Greet_UseCase_Success.Execute (Cmd2);
      Msg2    : constant String := To_String (Last_Written_Message);
   begin
      Run_Test
        ("Multiple executions - first Is_Ok", Unit_Result.Is_Ok (Result1));
      Run_Test
        ("Multiple executions - second Is_Ok", Unit_Result.Is_Ok (Result2));
      Run_Test ("Multiple executions - first message", Msg1 = "Hello, First!");
      Run_Test
        ("Multiple executions - second message", Msg2 = "Hello, Second!");
      Run_Test ("Multiple executions - messages different", Msg1 /= Msg2);
   end;

   --  Print summary
   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: Hybrid_Lib_Ada.Application.Usecase.Greet");
   Put_Line ("========================================");
   Put_Line ("Total tests: " & Total_Tests'Image);
   Put_Line ("Passed:      " & Passed_Tests'Image);
   Put_Line ("Failed:      " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   --  Register results with test framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);

end Test_Application_Usecase_Greet;
