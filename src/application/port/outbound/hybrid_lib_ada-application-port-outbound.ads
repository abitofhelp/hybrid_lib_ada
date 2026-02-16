pragma Ada_2022;
--  =========================================================================
--  Hybrid_Lib_Ada.Application.Port.Outbound - Parent package for outbound/output ports
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Parent package for outbound-facing ports (output ports).
--    Outbound ports define what external services the application needs.
--
--  Architecture Notes:
--    - Outbound ports = Hybrid_Lib_Ada.Application's REQUIRED dependencies
--    - Implemented by: Hybrid_Lib_Ada.Infrastructure layer (adapters)
--    - Hybrid_Lib_Ada.Application DEFINES the interface it needs
--    - Hybrid_Lib_Ada.Infrastructure CONFORMS to that interface
--    - This achieves DEPENDENCY INVERSION (dependencies point inward)
--
--  Design Pattern:
--    - Hybrid_Lib_Ada.Application: "I need something that can Write(message)"
--    - Hybrid_Lib_Ada.Infrastructure: "I have a ConsoleWriter that implements Write"
--    - api/desktop (composition root): Wires them together via generics
--
--  See Also:
--    Hybrid_Lib_Ada.Application.Port.Outbound.Writer - Writer output port
--    Hybrid_Lib_Ada.Infrastructure.Adapter - Implementations of output ports
--  =========================================================================

package Hybrid_Lib_Ada.Application.Port.Outbound
  with Pure
is

end Hybrid_Lib_Ada.Application.Port.Outbound;
