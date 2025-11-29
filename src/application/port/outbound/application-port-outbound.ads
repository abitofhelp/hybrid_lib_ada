pragma Ada_2022;
--  =========================================================================
--  Application.Port.Outbound - Parent package for outbound/output ports
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Parent package for outbound-facing ports (output ports).
--    Outbound ports define what external services the application needs.
--
--  Architecture Notes:
--    - Outbound ports = Application's REQUIRED dependencies
--    - Implemented by: Infrastructure layer (adapters)
--    - Application DEFINES the interface it needs
--    - Infrastructure CONFORMS to that interface
--    - This achieves DEPENDENCY INVERSION (dependencies point inward)
--
--  Design Pattern:
--    - Application: "I need something that can Write(message)"
--    - Infrastructure: "I have a ConsoleWriter that implements Write"
--    - Bootstrap: Wires them together via generic instantiation
--
--  See Also:
--    Application.Port.Outbound.Writer - Writer output port
--    Infrastructure.Adapter - Implementations of output ports
--  =========================================================================

package Application.Port.Outbound
  with Pure
is

end Application.Port.Outbound;
