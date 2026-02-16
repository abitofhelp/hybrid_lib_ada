pragma Ada_2022;
--  =========================================================================
--  Hybrid_Lib_Ada.Application.Port.Outbound.Writer - Output port for writing operations
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
--    - Hybrid_Lib_Ada.Application layer defines the interface it NEEDS
--    - Hybrid_Lib_Ada.Infrastructure layer CONFORMS to this interface
--    - This breaks the dependency: Hybrid_Lib_Ada.Infrastructure ->
--      Hybrid_Lib_Ada.Application (not Hybrid_Lib_Ada.Application -> Hybrid_Lib_Ada.Infrastructure)
--    - STATIC DISPATCH via generics - resolved at compile time
--
--  Generic Parameter Flow:
--    1. Hybrid_Lib_Ada.Infrastructure implements Write function matching signature
--    2. api/desktop (composition root) instantiates generic with Write
--    3. Use case receives instantiated port, calls Write without knowing impl
--    4. Zero runtime overhead - compile-time resolution
--
--  See Also:
--    Hybrid_Lib_Ada.Domain.Error.Result - Result type definition
--    Hybrid_Lib_Ada.Domain.Unit - Unit type for void returns
--    Hybrid_Lib_Ada.Infrastructure.Adapter.Console_Writer - Example
--    implementation of this port

with Hybrid_Lib_Ada.Domain.Error.Result;
with Hybrid_Lib_Ada.Domain.Unit;

package Hybrid_Lib_Ada.Application.Port.Outbound.Writer
  with Preelaborate
is

   --  Instantiate Result[Unit] for write operations
   package Unit_Result is new
     Hybrid_Lib_Ada.Domain.Error.Result.Generic_Result (T => Hybrid_Lib_Ada.Domain.Unit.Unit);

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

end Hybrid_Lib_Ada.Application.Port.Outbound.Writer;
