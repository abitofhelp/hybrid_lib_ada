pragma Ada_2022;
--  =========================================================================
--  Hybrid_Lib_Ada.Infrastructure - Root package for infrastructure layer
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Root package for the infrastructure layer containing concrete
--    implementations of application output ports (adapters pattern).
--
--  Architecture Notes:
--    - Part of the INFRASTRUCTURE layer (outermost with Hybrid_Lib_Ada.Application)
--    - Contains ADAPTERS that implement output ports
--    - Depends on: Hybrid_Lib_Ada.Domain, Hybrid_Lib_Ada.Application (to implement their interfaces)
--    - Examples: Console writer, file I/O, database access, HTTP clients
--
--  Design Pattern:
--    ADAPTER pattern - wraps external dependencies and conforms to
--    application-defined port interfaces. Converts exceptions to Results.
--
--  See Also:
--    Hybrid_Lib_Ada.Infrastructure.Adapter - Adapter implementations
--    Hybrid_Lib_Ada.Application.Port.Outbound - Port interfaces that adapters implement
--  =========================================================================

package Hybrid_Lib_Ada.Infrastructure
  with Pure
is

end Hybrid_Lib_Ada.Infrastructure;
