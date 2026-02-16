pragma Ada_2022;
--  =========================================================================
--  Hybrid_Lib_Ada.Application.Command.Greet - DTO for greet use case
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Data Transfer Object (DTO) for the greet use case. DTOs cross
--    architectural boundaries and should be simple, serializable data
--    structures with no business logic.
--
--  Educational Notes:
--    - No business logic in DTOs
--    - DTOs are different from domain entities
--    - This separates external API from internal domain model
--    - Crosses boundary: API -> Hybrid_Lib_Ada.Application
--
--  Mapping to Go:
--    Go: application/port/greet_command.go
--    type GreetCommand struct { Name string }
--
--  See Also:
--    Hybrid_Lib_Ada.Domain.Value_Object.Person - Hybrid_Lib_Ada.Domain entity (different from DTO)
--  =========================================================================

with Ada.Strings.Bounded;

package Hybrid_Lib_Ada.Application.Command.Greet
  with Preelaborate
is

   --  Maximum name length for DTO boundary (larger than domain constraint).
   --  DTO accepts up to 256 chars; Hybrid_Lib_Ada.Domain validates and rejects > 100 chars.
   --  This intentional boundary isolation:
   --    1. Allows Hybrid_Lib_Ada.Domain to shrink constraints without breaking Command
   --    2. Defense in depth (two validation points)
   --    3. SPARK-compatible (bounded strings throughout)
   Max_DTO_Name_Length : constant := 256;

   --  Bounded string for name in DTO. The DTO itself does not enforce
   --  validity rules; domain logic is responsible for validation.
   --  Named after parent package per Ada agent naming convention
   package Greet_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => Max_DTO_Name_Length);

   --  ========================================================================
   --  Greet_Command DTO
   --  ========================================================================

   --  Data Transfer Object for greet use case.
   --  Simple data structure that crosses API -> Hybrid_Lib_Ada.Application
   --  boundary. It may carry invalid data; the domain layer is responsible
   --  for validating the name and returning appropriate Result errors.

   type Greet_Command is record
      Name : Greet_Strings.Bounded_String;
   end record;

   --  Helper function to create DTO from String. This function does not
   --  perform validation; it simply packages the raw input. Validation is
   --  performed in Hybrid_Lib_Ada.Domain.Value_Object.Person.Create via Result.
   function Create (Name : String) return Greet_Command;

   --  Helper function to extract name as String
   function Get_Name (Cmd : Greet_Command) return String;

end Hybrid_Lib_Ada.Application.Command.Greet;
