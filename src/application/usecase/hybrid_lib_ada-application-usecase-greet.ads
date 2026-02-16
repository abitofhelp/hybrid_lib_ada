pragma Ada_2022;
--  =========================================================================
--  Hybrid_Lib_Ada.Application.Usecase.Greet - Greet use case orchestration
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Use case orchestrating the greeting workflow. This is application-
--    specific business logic orchestration. Coordinates domain objects
--    (Person) and depends on output ports (Console) without knowing
--    implementation details.
--
--  Architecture Notes:
--    - Use case = application business logic orchestration
--    - Coordinates domain objects
--    - Depends on output ports defined in application layer
--    - Never imports infrastructure layer
--
--  Dependency Flow (all pointing INWARD toward Hybrid_Lib_Ada.Domain):
--    Greet_Use_Case -> Hybrid_Lib_Ada.Domain.Value_Object.Person
--    Greet_Use_Case -> Hybrid_Lib_Ada.Application.Port.Out.Writer
--    (port interface)
--    Hybrid_Lib_Ada.Infrastructure.Console_Adapter -> Hybrid_Lib_Ada.Application.Port.Out.Writer
--    (implements)
--    - Takes Console_Write function as generic parameter
--    - Hybrid_Lib_Ada.Infrastructure provides the implementation
--    - api/desktop (composition root) wires them together
--
--  See Also:
--    Hybrid_Lib_Ada.Domain.Value_Object.Person - Person value object
--    Hybrid_Lib_Ada.Application.Port.Out.Writer - Output port interface
--  =========================================================================

with Hybrid_Lib_Ada.Application.Port.Outbound.Writer;
with Hybrid_Lib_Ada.Application.Command.Greet;

--  ========================================================================
--  Generic Use Case Package
--  ========================================================================

--  This generic package is instantiated with a console output port.
--  The port is provided by infrastructure, injected at instantiation time.
--
--  This demonstrates STATIC DEPENDENCY INJECTION via generics:
--  - No runtime overhead (compared to interface dispatch)
--  - Compile-time verification of port compatibility
--  - Dependencies still point inbound (Hybrid_Lib_Ada.Application doesn't import
--    Hybrid_Lib_Ada.Infrastructure)

generic
   with
     function Writer
       (Message : String)
        return Hybrid_Lib_Ada.Application.Port.Outbound.Writer.Unit_Result.Result;
package Hybrid_Lib_Ada.Application.Usecase.Greet with Preelaborate is

   --  ======================================================================
   --  Execute: Run the greeting use case
   --  ======================================================================

   --  Orchestration workflow:
   --  1. Extract name from GreetCommand DTO
   --  2. Validate and create Person from name (domain validation)
   --  3. Generate greeting message (application-level formatting)
   --  4. Write greeting to console via output port
   --  5. Propagate any errors via railway-oriented programming
   --
   --  Input: GreetCommand DTO crossing API -> application boundary
   --
   --  Error scenarios:
   --  - Validation_Error: Invalid person name (empty, too long)
   --  - IO_Error: Console write failure (rare, but possible)

   function Execute
     (Cmd : Hybrid_Lib_Ada.Application.Command.Greet.Greet_Command)
      return Hybrid_Lib_Ada.Application.Port.Outbound.Writer.Unit_Result.Result;

end Hybrid_Lib_Ada.Application.Usecase.Greet;
