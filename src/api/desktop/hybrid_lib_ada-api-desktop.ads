pragma Ada_2022;
--  ===========================================================================
--  Hybrid_Lib_Ada.API.Desktop - Desktop Platform Composition Root
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Desktop/server platform composition root. Wires infrastructure adapters
--    (Console_Writer) to the generic operations, providing a ready-to-use
--    API for desktop applications.
--
--  Architecture:
--    - COMPOSITION ROOT for libraries (4-layer model)
--    - SPARK-Mode Off: Contains I/O wiring, not formally verifiable
--    - Instantiates API.Operations with Infrastructure.Adapter.Console_Writer
--    - Re-exports Greet as a simple callable function
--
--  Target Platform:
--    - Linux / macOS / Windows
--    - Full Ada runtime with Ada.Text_IO
--
--  Usage:
--    with Hybrid_Lib_Ada.API;
--    with Hybrid_Lib_Ada.API.Desktop;
--
--    Cmd : constant API.Greet_Command :=
--      API.Create_Greet_Command ("Alice");
--    Result : constant API.Unit_Result.Result := API.Desktop.Greet (Cmd);
--
--  ===========================================================================

pragma SPARK_Mode (Off);

with Application.Command.Greet;
with Application.Port.Outbound.Writer;
with Infrastructure.Adapter.Console_Writer;
with Hybrid_Lib_Ada.API.Operations;

package Hybrid_Lib_Ada.API.Desktop is

   --  ========================================================================
   --  Composition: Wire Infrastructure to Operations
   --  ========================================================================

   --  Instantiate Operations with Console_Writer adapter
   package Console_Ops is new Hybrid_Lib_Ada.API.Operations
     (Writer => Infrastructure.Adapter.Console_Writer.Write);

   --  ========================================================================
   --  Greet Operation (Convenience Re-export)
   --  ========================================================================

   --  Execute the greet operation using console output.
   --
   --  Returns:
   --    - Ok(Unit) if greeting succeeded
   --    - Error(Validation_Error) if name invalid
   --    - Error(Infrastructure_Error) if write failed

   function Greet
     (Cmd : Application.Command.Greet.Greet_Command)
      return Application.Port.Outbound.Writer.Unit_Result.Result
   renames Console_Ops.Greet;

end Hybrid_Lib_Ada.API.Desktop;
