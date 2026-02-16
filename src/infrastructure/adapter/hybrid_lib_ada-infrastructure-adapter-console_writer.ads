pragma Ada_2022;
--  =========================================================================
--  Hybrid_Lib_Ada.Infrastructure.Adapter.Console_Writer - Console output adapter
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Concrete implementation of console output (adapter pattern).
--    Implements the signature required by Hybrid_Lib_Ada.Application.Port.Out.Writer.
--    Uses Functional.Try to convert exception-prone I/O to Result types.
--
--  Architecture Notes:
--    - ADAPTER that implements the Writer output port
--    - Conforms to interface defined by Hybrid_Lib_Ada.Application.Port.Out.Writer
--    - Uses Functional.Try.Map_To_Result_With_Param for exception handling
--    - Depends on Ada.Text_IO (standard library IO)
--    - This layer depends on Hybrid_Lib_Ada.Application + Hybrid_Lib_Ada.Domain + Functional
--
--  Dependency Flow:
--    Console_Writer -> Hybrid_Lib_Ada.Application.Port.Out.Writer (implements interface)
--    Console_Writer -> Functional.Try (exception boundary conversion)
--    Console_Writer -> Hybrid_Lib_Ada.Domain.Result (for error handling)
--    Console_Writer -> Ada.Text_IO (external IO library)
--
--  See Also:
--    Hybrid_Lib_Ada.Application.Port.Out.Writer - Port interface this implements
--    Functional.Try - Exception-to-Result bridge used internally
--  =========================================================================

with Hybrid_Lib_Ada.Application.Port.Outbound.Writer;

package Hybrid_Lib_Ada.Infrastructure.Adapter.Console_Writer is

   --  ========================================================================
   --  Write: Console output implementation
   --  ========================================================================

   --  Implements the signature required by Hybrid_Lib_Ada.Application.Port.Out.Writer
   --
   --  This function:
   --  1. Writes message to standard output using Ada.Text_IO
   --  2. Catches any IO exceptions (rare, but possible)
   --  3. Converts exceptions to Result[Unit] for consistent error handling
   --
   --  Returns:
   --  - Ok(Unit) if write successful
   --  - Error(IO_Error) if IO operation fails

   function Write
     (Message : String)
      return Hybrid_Lib_Ada.Application.Port.Outbound.Writer.Unit_Result.Result;

end Hybrid_Lib_Ada.Infrastructure.Adapter.Console_Writer;
