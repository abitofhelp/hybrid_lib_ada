pragma Ada_2022;
--  =========================================================================
--  Hybrid_Lib_Ada.Domain.Error - Error handling types and utilities
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Public facade for domain error handling. Defines error types used
--    throughout the application for consistent error reporting.
--
--  Architecture Notes:
--    - Part of the DOMAIN layer (innermost, zero dependencies)
--    - Error types are concrete (not generic) for consistency
--    - Used with Hybrid_Lib_Ada.Domain.Error.Result monad for functional error handling
--    - All errors use bounded strings (no heap allocation)
--
--  Usage:
--    with Hybrid_Lib_Ada.Domain.Error;           -- Gets error types
--    with Hybrid_Lib_Ada.Domain.Error.Result;    -- Gets Result[T] monad
--
--    use Hybrid_Lib_Ada.Domain.Error;  -- Makes Error_Kind, Error_Type visible
--
--  Design Pattern:
--    Hybrid_Lib_Ada.Domain error types as building blocks for Result monad:
--    - Error_Kind: Enumeration of error categories
--    - Error_Type: Record containing kind + message
--    - Result[T]: Either monad that wraps T or Error_Type
--
--  See Also:
--    Hybrid_Lib_Ada.Domain.Error.Result - Generic Result monad using these error types
--  =========================================================================

with Ada.Strings.Bounded;

package Hybrid_Lib_Ada.Domain.Error
  with Preelaborate
is

   --  ========================================================================
   --  Error String Type
   --  ========================================================================

   --  Using bounded string for error messages (memory safe, no heap)
   package Error_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 512);

   --  ========================================================================
   --  Error Kind Enumeration
   --  ========================================================================

   --  Categories of errors that can occur in the application
   --  More granular than a single "Infrastructure_Error" for better handling
   type Error_Kind is
     (Validation_Error,  --  Hybrid_Lib_Ada.Domain validation failures (invalid input)
      Parse_Error,       --  Malformed data/parsing failures
      Not_Found_Error,   --  Resource not found (file, record, etc.)
      IO_Error,          --  I/O operations (file, network, console)
      Internal_Error);   --  Bugs, invariant violations, preconditions

   --  ========================================================================
   --  Error Type Record
   --  ========================================================================

   --  Concrete error type used throughout the application
   --  Combines error category with descriptive message
   type Error_Type is record
      Kind    : Error_Kind;
      Message : Error_Strings.Bounded_String;
   end record;

end Hybrid_Lib_Ada.Domain.Error;
