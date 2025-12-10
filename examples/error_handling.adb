pragma Ada_2022;
--  ===========================================================================
--  Error_Handling - Demonstrates Result monad error handling
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates how the library handles validation errors using the
--    Result monad pattern (railway-oriented programming):
--    1. Valid name - greeting succeeds
--    2. Empty name - validation error returned
--    3. Name too long - validation error returned
--
--  Usage:
--    ./bin/error_handling
--  ===========================================================================

with Ada.Text_IO;
with Hybrid_Lib_Ada.API;

procedure Error_Handling is
   use Ada.Text_IO;

   package API renames Hybrid_Lib_Ada.API;

   --  Helper to display result
   procedure Show_Result (Label : String; Result : API.Unit_Result.Result) is
   begin
      Put (Label & ": ");
      if API.Unit_Result.Is_Ok (Result) then
         Put_Line ("OK - Greeting printed");
      else
         declare
            Err : constant API.Error_Type :=
              API.Unit_Result.Error_Info (Result);
         begin
            Put_Line ("ERROR - " &
                      API.Error_Kind'Image (Err.Kind) & ": " &
                      API.Error_Strings.To_String (Err.Message));
         end;
      end if;
   end Show_Result;

   --  Test cases
   Result_Valid    : API.Unit_Result.Result;
   Result_Empty    : API.Unit_Result.Result;
   Result_Too_Long : API.Unit_Result.Result;

   --  Create a string longer than Max_Name_Length (100 chars)
   Long_Name : constant String (1 .. 150) := [others => 'X'];
begin
   Put_Line ("=== Error Handling Example ===");
   New_Line;

   --  Test 1: Valid name
   Put_Line ("Test 1: Valid name 'Alice'");
   Result_Valid := API.Greet (API.Create_Greet_Command ("Alice"));
   Show_Result ("  Result", Result_Valid);
   New_Line;

   --  Test 2: Empty name (validation error)
   Put_Line ("Test 2: Empty name ''");
   Result_Empty := API.Greet (API.Create_Greet_Command (""));
   Show_Result ("  Result", Result_Empty);
   New_Line;

   --  Test 3: Name too long (validation error)
   Put_Line ("Test 3: Name too long (150 chars)");
   Result_Too_Long := API.Greet (API.Create_Greet_Command (Long_Name));
   Show_Result ("  Result", Result_Too_Long);
   New_Line;

   Put_Line ("=== End Example ===");
end Error_Handling;
