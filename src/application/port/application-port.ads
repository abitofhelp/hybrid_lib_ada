pragma Ada_2022;
--  =========================================================================
--  Application.Port - Parent package for ports (hexagonal architecture)
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Parent package for application ports (interfaces).
--    Ports define how the application interacts with external systems.
--
--  Architecture Notes:
--    - Part of the APPLICATION layer
--    - Implements HEXAGONAL/PORTS & ADAPTERS pattern
--    - Inbound ports: How external actors use the application (use cases)
--    - Outbound ports: How the application uses external systems
--
--  See Also:
--    Application.Port.Inbound - Input ports (how to call the app)
--    Application.Port.Outbound - Output ports (what the app needs)
--  =========================================================================

package Application.Port
  with Pure
is

end Application.Port;
