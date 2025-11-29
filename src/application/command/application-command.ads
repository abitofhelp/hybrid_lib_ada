pragma Ada_2022;
--  =========================================================================
--  Application.Command - Parent package for command DTOs
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Parent package for command Data Transfer Objects (DTOs).
--    Commands are simple data structures that cross the
--    presentation -> application boundary, carrying parameters
--    for use cases.
--  Architecture Notes:
--    - Part of the APPLICATION layer
--    - Commands = DTOs for use case input
--    - No business logic in commands (just data)
--    - Separate from domain entities (different concerns)
--
--  Design Pattern:
--    Command pattern / DTO pattern for decoupling presentation from
--    application. Presentation sends commands, application executes them.
--
--  See Also:
--    Application.Command.Greet - Example command DTO
--    Application.Usecase - Use cases that process commands
--  =========================================================================

package Application.Command
  with Pure
is

end Application.Command;
