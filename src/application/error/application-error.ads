pragma Ada_2022;
--  =========================================================================
--  Application.Error - Error type facade for outer layers
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Re-exports Domain.Error types for use by API layer.
--    Maintains architectural boundary: API -> Application -> Domain
--  Architecture Notes:
--    - Part of the APPLICATION layer (orchestration/contract layer)
--    - Re-exports Domain error types without wrapping (zero overhead)
--    - Allows API layer to access error types without depending on Domain
--    - Infrastructure may use Domain.Error directly (it's allowed)
--
--  Why This Exists:
--    In our hybrid architecture:
--    - Domain is the only shareable layer across applications
--    - Application/Infrastructure/API are library-specific
--    - API layer must NOT depend on Domain directly (to prevent coupling)
--    - Application acts as the contract/facade layer for API layer
--
--  Usage (API layer):
--    with Application.Error;  -- NOT with Domain.Error
--
--    use Application.Error;
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
--    Domain.Error - The canonical error type definitions
--    Application.Port.Outbound.Writer - Uses Result[Unit] with these errors
--  =========================================================================

with Domain.Error;

package Application.Error
  with Preelaborate
is

   --  =======================================================================
   --  Re-exported Types from Domain.Error
   --  =======================================================================

   --  Bounded string package for error messages
   package Error_Strings renames Domain.Error.Error_Strings;

   --  Error categories
   subtype Error_Kind is Domain.Error.Error_Kind;

   --  Bring Error_Kind values into scope for convenience
   Validation_Error : constant Error_Kind := Domain.Error.Validation_Error;
   Parse_Error      : constant Error_Kind := Domain.Error.Parse_Error;
   Not_Found_Error  : constant Error_Kind := Domain.Error.Not_Found_Error;
   IO_Error         : constant Error_Kind := Domain.Error.IO_Error;
   Internal_Error   : constant Error_Kind := Domain.Error.Internal_Error;

   --  Concrete error record (kind + message)
   subtype Error_Type is Domain.Error.Error_Type;

end Application.Error;
