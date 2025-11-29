pragma Ada_2022;
--  =========================================================================
--  Application.Port.Outbound.Writer - Output port for writing operations
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Output port specification for writing operations (console, file, etc).
--    This is an OUTPUT PORT - application defines it,
--    infrastructure implements it.
--
--  Architecture Notes:
--    - Application layer defines the interface it NEEDS
--    - Infrastructure layer CONFORMS to this interface
--    - This breaks the dependency: Infrastructure ->
--      Application (not Application -> Infrastructure)
--    - STATIC DISPATCH via generics - resolved at compile time
--
--  Generic Parameter Flow:
--    1. Infrastructure implements Write function matching signature
--    2. Bootstrap instantiates generic with Infrastructure's Write
--    3. Use case receives instantiated port, calls Write without knowing impl
--    4. Zero runtime overhead - compile-time resolution
--
--  See Also:
--    Domain.Error.Result - Result type definition
--    Domain.Unit - Unit type for void returns
--    Infrastructure.Adapter.Console_Writer - Example
--    implementation of this port

with Domain.Error.Result;
with Domain.Unit;

package Application.Port.Outbound.Writer
  with Preelaborate
is

   --  Instantiate Result[Unit] for write operations
   package Unit_Result is new
     Domain.Error.Result.Generic_Result (T => Domain.Unit.Unit);

   --  ========================================================================
   --  Generic Writer
   --  ========================================================================

   --  This generic formal function is the OUTPUT PORT CONTRACT
   --
   --  Any infrastructure adapter that wants to provide write output must:
   --  1. Define a function matching this signature
   --  2. Be used to instantiate this generic package
   --
   --  The Message parameter uses String (not bounded) for flexibility at the
   --  boundary - infrastructure adapters handle the conversion

   generic
      with function Write (Message : String) return Unit_Result.Result;
   package Generic_Writer is

      --  Re-export the Write function for use case to call
      function Write_Message (Message : String) return Unit_Result.Result
      renames Write;

   end Generic_Writer;

end Application.Port.Outbound.Writer;
