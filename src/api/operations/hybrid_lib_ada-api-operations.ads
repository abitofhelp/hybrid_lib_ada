pragma Ada_2022;
--  ===========================================================================
--  Hybrid_Lib_Ada.API.Operations - Generic Operations (SPARK-Safe)
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Generic operations facade. This package takes a Writer function as a
--    generic parameter and provides operations that use it. SPARK-Mode is
--    enabled for formal verification of the operation logic.
--
--  Architecture:
--    - Generic package parameterized by Writer port
--    - Wires Greet_Command to Application.Usecase.Greet
--    - SPARK-safe: no side effects in logic, I/O through port parameter
--    - Depends ONLY on Application/Domain - NOT on Infrastructure
--
--  Instantiation:
--    See Hybrid_Lib_Ada.API.Desktop for desktop/console instantiation.
--
--  ===========================================================================

pragma SPARK_Mode (On);

--  Only Application/Domain dependencies - NOT Infrastructure
with Application.Command.Greet;
with Application.Port.Outbound.Writer;
with Application.Usecase.Greet;

generic
   with function Writer
     (Message : String)
      return Application.Port.Outbound.Writer.Unit_Result.Result;
package Hybrid_Lib_Ada.API.Operations is

   --  Instantiate the Greet use case with the provided Writer
   package Greet_Use_Case is new Application.Usecase.Greet (Writer => Writer);

   --  ========================================================================
   --  Greet Operation
   --  ========================================================================

   --  Execute the greet operation with the given command.
   --
   --  This delegates to the instantiated use case, which will:
   --  1. Validate and create Person from command name
   --  2. Generate greeting message
   --  3. Write message using provided Writer port
   --  4. Return Result[Unit] (Ok or Error)

   function Greet
     (Cmd : Application.Command.Greet.Greet_Command)
      return Application.Port.Outbound.Writer.Unit_Result.Result
   renames Greet_Use_Case.Execute;

end Hybrid_Lib_Ada.API.Operations;
