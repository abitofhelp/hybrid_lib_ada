pragma Ada_2022;
--  =========================================================================
--  Application.Usecase.Greet - Greet use case orchestration
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
--  Dependency Flow (all pointing INWARD toward Domain):
--    Greet_Use_Case -> Domain.Value_Object.Person
--    Greet_Use_Case -> Application.Port.Out.Writer
--    (port interface)
--    Infrastructure.Console_Adapter -> Application.Port.Out.Writer
--    (implements)
--    - Takes Console_Write function as generic parameter
--    - Infrastructure provides the implementation
--    - Bootstrap wires them together
--
--  See Also:
--    Domain.Value_Object.Person - Person value object
--    Application.Port.Out.Writer - Output port interface
--  =========================================================================

with Application.Port.Outbound.Writer;
with Application.Command.Greet;

--  ========================================================================
--  Generic Use Case Package
--  ========================================================================

--  This generic package is instantiated with a console output port.
--  The port is provided by infrastructure, injected at instantiation time.
--
--  This demonstrates STATIC DEPENDENCY INJECTION via generics:
--  - No runtime overhead (compared to interface dispatch)
--  - Compile-time verification of port compatibility
--  - Dependencies still point inbound (Application doesn't import
--    Infrastructure)

generic
   with
     function Writer
       (Message : String)
        return Application.Port.Outbound.Writer.Unit_Result.Result;
package Application.Usecase.Greet with Preelaborate is

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
   --  Input: GreetCommand DTO crossing presentation -> application boundary
   --
   --  Error scenarios:
   --  - Validation_Error: Invalid person name (empty, too long)
   --  - IO_Error: Console write failure (rare, but possible)

   function Execute
     (Cmd : Application.Command.Greet.Greet_Command)
      return Application.Port.Outbound.Writer.Unit_Result.Result;

end Application.Usecase.Greet;
