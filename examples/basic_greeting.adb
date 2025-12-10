pragma Ada_2022;
--  ===========================================================================
--  Basic_Greeting - Simple example of Hybrid_Lib_Ada usage
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates the most basic usage of the Hybrid_Lib_Ada library:
--    1. Create a greet command with a name
--    2. Execute the greeting via the API facade
--    3. Handle the Result (success or error)
--
--  Usage:
--    ./bin/basic_greeting
--  ===========================================================================

with Ada.Text_IO;
with Hybrid_Lib_Ada.API;

procedure Basic_Greeting is
   use Ada.Text_IO;

   package API renames Hybrid_Lib_Ada.API;

   --  Create a greet command for "World"
   Cmd : constant API.Greet_Command := API.Create_Greet_Command ("World");

   --  Execute the greeting - returns Result[Unit]
   Result : constant API.Unit_Result.Result := API.Greet (Cmd);
begin
   Put_Line ("=== Basic Greeting Example ===");
   New_Line;

   if API.Unit_Result.Is_Ok (Result) then
      Put_Line ("Greeting executed successfully!");
   else
      Put_Line ("Error: " &
                API.Error_Strings.To_String
                  (API.Unit_Result.Error_Info (Result).Message));
   end if;

   New_Line;
   Put_Line ("=== End Example ===");
end Basic_Greeting;
