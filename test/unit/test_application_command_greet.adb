pragma Ada_2022;
--  ======================================================================
--  Test_Application_Command_Greet
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit tests for Hybrid_Lib_Ada.Application.Command.Greet DTO.
--    Tests Create function and then Get_Name accessor.
--  ======================================================================

with Ada.Text_IO;
with Hybrid_Lib_Ada.Application.Command.Greet;
with Test_Framework;

procedure Test_Application_Command_Greet is

   use Ada.Text_IO;
   use Hybrid_Lib_Ada.Application.Command.Greet;

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

begin
   Put_Line ("========================================");
   Put_Line ("Testing: Hybrid_Lib_Ada.Application.Command.Greet");
   Put_Line ("========================================");
   New_Line;

   --  ========================================================================
   --  Test: Create and then Get_Name with simple name
   --  ========================================================================

   declare
      Cmd  : constant Greet_Command := Create ("Alice");
      Name : constant String        := Get_Name (Cmd);
   begin
      Run_Test
        ("Create simple name - Get_Name returns correct value",
         Name = "Alice");
   end;

   --  ========================================================================
   --  Test: Create and then Get_Name with name containing spaces
   --  ========================================================================

   declare
      Cmd  : constant Greet_Command := Create ("Bob Smith");
      Name : constant String        := Get_Name (Cmd);
   begin
      Run_Test
        ("Create name with spaces - Get_Name correct", Name = "Bob Smith");
   end;

   --  ========================================================================
   --  Test: Create and then Get_Name with single character
   --  ========================================================================

   declare
      Cmd  : constant Greet_Command := Create ("X");
      Name : constant String        := Get_Name (Cmd);
   begin
      Run_Test ("Create single char - Get_Name correct", Name = "X");
   end;

   --  ========================================================================
   --  Test: Create and then Get_Name with max length name
   --  ========================================================================

   declare
      Max_Name : constant String (1 .. Max_DTO_Name_Length) := [others => 'A'];
      Cmd      : constant Greet_Command := Create (Max_Name);
      Name     : constant String        := Get_Name (Cmd);
   begin
      Run_Test
        ("Create max length name - Get_Name correct length",
         Name'Length = Max_DTO_Name_Length);
      Run_Test
        ("Create max length name - Get_Name correct content",
         Name = Max_Name);
   end;

   --  ========================================================================
   --  Test: Create with special characters
   --  ========================================================================

   declare
      Special_Name : constant String        := "Anne-Marie O'Brien";
      Cmd          : constant Greet_Command := Create (Special_Name);
      Name         : constant String        := Get_Name (Cmd);
   begin
      Run_Test
        ("Create with special chars - Get_Name correct", Name = Special_Name);
   end;

   --  ========================================================================
   --  Test: Create with Unicode characters
   --  ========================================================================

   declare
      Unicode_Name : constant String        := "José María";
      Cmd          : constant Greet_Command := Create (Unicode_Name);
      Name         : constant String        := Get_Name (Cmd);
   begin
      Run_Test ("Create with Unicode - Get_Name correct", Name = Unicode_Name);
   end;

   --  ========================================================================
   --  Test: Multiple commands don't interfere
   --  ========================================================================

   declare
      Cmd1  : constant Greet_Command := Create ("Person1");
      Cmd2  : constant Greet_Command := Create ("Person2");
      Cmd3  : constant Greet_Command := Create ("Person3");
      Name1 : constant String        := Get_Name (Cmd1);
      Name2 : constant String        := Get_Name (Cmd2);
      Name3 : constant String        := Get_Name (Cmd3);
   begin
      Run_Test ("Multiple commands - Cmd1 correct", Name1 = "Person1");
      Run_Test ("Multiple commands - Cmd2 correct", Name2 = "Person2");
      Run_Test ("Multiple commands - Cmd3 correct", Name3 = "Person3");
      Run_Test
        ("Multiple commands - all different",
         Name1 /= Name2 and then Name2 /= Name3);
   end;

   --  ========================================================================
   --  Test: Round-trip (Create → Get_Name → Create)
   --  ========================================================================

   declare
      Original_Name : constant String        := "Round Trip Test";
      Cmd1          : constant Greet_Command := Create (Original_Name);
      Retrieved     : constant String        := Get_Name (Cmd1);
      Cmd2          : constant Greet_Command := Create (Retrieved);
      Final_Name    : constant String        := Get_Name (Cmd2);
   begin
      Run_Test
        ("Round-trip test - name preserved",
         Final_Name = Original_Name);
   end;

   --  ========================================================================
   --  Test: Create preserves exact string content
   --  ========================================================================

   declare
      Test_String : constant String        := "  Trim Test  ";
      Cmd         : constant Greet_Command := Create (Test_String);
      Result      : constant String        := Get_Name (Cmd);
   begin
      --  The DTO should preserve the exact string (no trimming)
      Run_Test ("Create preserves exact content", Result = Test_String);
   end;

   --  Print summary
   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: Hybrid_Lib_Ada.Application.Command.Greet");
   Put_Line ("========================================");
   Put_Line ("Total tests: " & Total_Tests'Image);
   Put_Line ("Passed:      " & Passed_Tests'Image);
   Put_Line ("Failed:      " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   --  Register results with test framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);

end Test_Application_Command_Greet;
