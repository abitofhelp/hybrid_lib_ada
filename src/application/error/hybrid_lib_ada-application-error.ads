pragma Ada_2022;
--  =========================================================================
--  Hybrid_Lib_Ada.Application.Error - Error type facade for outer layers
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Re-exports Hybrid_Lib_Ada.Domain.Error types for use by API layer.
--    Maintains architectural boundary: API -> Hybrid_Lib_Ada.Application -> Hybrid_Lib_Ada.Domain
--  Architecture Notes:
--    - Part of the APPLICATION layer (orchestration/contract layer)
--    - Re-exports Hybrid_Lib_Ada.Domain error types without wrapping (zero overhead)
--    - Allows API layer to access error types without depending on Hybrid_Lib_Ada.Domain
--    - Hybrid_Lib_Ada.Infrastructure may use Hybrid_Lib_Ada.Domain.Error directly (it's allowed)
--
--  Why This Exists:
--    In our hybrid architecture:
--    - Hybrid_Lib_Ada.Domain is the only shareable layer across applications
--    - Hybrid_Lib_Ada.Application/Hybrid_Lib_Ada.Infrastructure/API are library-specific
--    - API layer must NOT depend on Hybrid_Lib_Ada.Domain directly (to prevent coupling)
--    - Hybrid_Lib_Ada.Application acts as the contract/facade layer for API layer
--
--  Usage (API layer):
--    with Hybrid_Lib_Ada.Application.Error;  -- NOT with Hybrid_Lib_Ada.Domain.Error
--
--    use Hybrid_Lib_Ada.Application.Error;
--
--    Error : constant Error_Type := ...
--    case Error.Kind is
--       when Validation_Error => ...
--       when Parse_Error => ...
--       when Not_Found_Error => ...
--       when IO_Error => ...
--       when Internal_Error => ...
--    end case;
--
--  See Also:
--    Hybrid_Lib_Ada.Domain.Error - The canonical error type definitions
--    Hybrid_Lib_Ada.Application.Port.Outbound.Writer - Uses Result[Unit] with these errors
--  =========================================================================

with Hybrid_Lib_Ada.Domain.Error;

package Hybrid_Lib_Ada.Application.Error
  with Preelaborate
is

   --  =======================================================================
   --  Re-exported Types from Hybrid_Lib_Ada.Domain.Error
   --  =======================================================================

   --  Bounded string package for error messages
   package Error_Strings renames Hybrid_Lib_Ada.Domain.Error.Error_Strings;

   --  Error categories
   subtype Error_Kind is Hybrid_Lib_Ada.Domain.Error.Error_Kind;

   --  Bring Error_Kind values into scope for convenience
   Validation_Error : constant Error_Kind := Hybrid_Lib_Ada.Domain.Error.Validation_Error;
   Parse_Error      : constant Error_Kind := Hybrid_Lib_Ada.Domain.Error.Parse_Error;
   Not_Found_Error  : constant Error_Kind := Hybrid_Lib_Ada.Domain.Error.Not_Found_Error;
   IO_Error         : constant Error_Kind := Hybrid_Lib_Ada.Domain.Error.IO_Error;
   Internal_Error   : constant Error_Kind := Hybrid_Lib_Ada.Domain.Error.Internal_Error;

   --  Concrete error record (kind + message)
   subtype Error_Type is Hybrid_Lib_Ada.Domain.Error.Error_Type;

end Hybrid_Lib_Ada.Application.Error;
