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
--    - Instantiates API.Operations with Hybrid_Lib_Ada.Infrastructure.Adapter.Console_Writer
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

with Hybrid_Lib_Ada.Application.Command.Greet;
with Hybrid_Lib_Ada.Application.Port.Outbound.Writer;
with Hybrid_Lib_Ada.Infrastructure.Adapter.Console_Writer;
with Hybrid_Lib_Ada.API.Operations;

package Hybrid_Lib_Ada.API.Desktop is

   --  ========================================================================
   --  Composition: Wire Hybrid_Lib_Ada.Infrastructure to Operations
   --  ========================================================================

   --  Instantiate Operations with Console_Writer adapter
   package Console_Ops is new Hybrid_Lib_Ada.API.Operations
     (Writer => Hybrid_Lib_Ada.Infrastructure.Adapter.Console_Writer.Write);

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
     (Cmd : Hybrid_Lib_Ada.Application.Command.Greet.Greet_Command)
      return Hybrid_Lib_Ada.Application.Port.Outbound.Writer.Unit_Result.Result
   renames Console_Ops.Greet;

end Hybrid_Lib_Ada.API.Desktop;
