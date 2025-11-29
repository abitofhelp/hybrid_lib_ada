pragma Ada_2022;
--  ===========================================================================
--  Hybrid_Lib_Ada - Greeter Library Root Package
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Root package for the Hybrid_Lib_Ada library. This library demonstrates
--    hexagonal architecture, functional error handling, and dependency
--    injection in Ada 2022.
--
--  Quick Start:
--    with Hybrid_Lib_Ada.API;
--
--    procedure Main is
--       use Hybrid_Lib_Ada.API;
--       Cmd : constant Greet_Command := Create_Greet_Command ("World");
--       Res : constant Unit_Result.Result := Greet (Cmd);
--    begin
--       if not Unit_Result.Is_Ok (Res) then
--          -- Handle error
--       end if;
--    end Main;
--
--  Architecture:
--    This library follows hybrid DDD/Clean/Hexagonal architecture:
--    - Domain: Pure business logic (Person value object)
--    - Application: Use cases and ports (Greet use case)
--    - Infrastructure: Adapters (Console_Writer)
--    - API: Public facade (Hybrid_Lib_Ada.API)
--
--  Public API:
--    See Hybrid_Lib_Ada.API for the main library interface
--
--  Embedded Safety:
--    This library is designed for use in embedded systems. All packages
--    use bounded types, static allocation, and avoid heap allocation.
--    The restrictions below enforce embedded-safe patterns.
--
--  ===========================================================================

--  ==========================================================================
--  Embedded Safety Restrictions
--  ==========================================================================
--  Per Ada Agent and SPARK Agent requirements, library projects MUST be
--  embedded-safe by default. These restrictions ensure the library can be
--  used in safety-critical and resource-constrained environments.
--
--  Note: Restrictions must be listed BEFORE the package declaration.
--  ==========================================================================

pragma Restrictions (No_Implicit_Heap_Allocations);
--  Prevents hidden heap allocations from language features

pragma Restrictions (No_Anonymous_Allocators);
--  Prevents anonymous access types from allocating

pragma Restrictions (No_Coextensions);
--  Prevents coextension allocations

package Hybrid_Lib_Ada with Pure is

   --  Library version information
   --  Note: Actual version is in Hybrid_Lib_Ada.Version package
   Lib_Version : constant String := "1.0.0";

end Hybrid_Lib_Ada;
