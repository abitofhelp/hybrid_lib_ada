pragma Ada_2022;
--  =========================================================================
--  Hybrid_Lib_Ada.Domain.Unit - Unit type for Result with no meaningful value
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Unit type for operations that return Result but no meaningful value.
--    Represents "void" or "no value" in the Result monad. Used for
--    operations with side effects (like Console.Write).
--
--  Architecture Notes:
--    - DOMAIN layer (innermost, zero dependencies)
--    - Allows consistent Result[Unit] return type instead of procedures
--    - Similar to () in Rust, void in C, or Unit in Scala
--    - Located in Hybrid_Lib_Ada.Domain so ANY layer can use Result[Unit]
--
--  Usage:
--    with Hybrid_Lib_Ada.Domain.Unit;
--    with Hybrid_Lib_Ada.Domain.Error.Result;
--
--    package Unit_Result is new
--      Hybrid_Lib_Ada.Domain.Error.Result.Generic_Result (T => Hybrid_Lib_Ada.Domain.Unit.Unit);
--
--    function Write (Message : String) return Unit_Result.Result;
--
--  See Also:
--    Hybrid_Lib_Ada.Domain.Error.Result - Generic Result monad
--  =========================================================================

package Hybrid_Lib_Ada.Domain.Unit
  with Pure
is

   --  ========================================================================
   --  Unit Type: Represents "no meaningful value"
   --  ========================================================================

   --  Used when we need Result[T] but T carries no information
   type Unit is null record;

   --  Singleton instance for convenience
   Unit_Value : constant Unit := (null record);

end Hybrid_Lib_Ada.Domain.Unit;
