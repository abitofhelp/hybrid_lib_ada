pragma Ada_2022;
--  =========================================================================
--  Application.Port.Inbound - Parent package for inbound/input ports
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Parent package for inbound-facing ports (input ports).
--    Inbound ports define how external actors invoke the application.
--    In this project, use cases ARE the inbound ports.
--
--  Architecture Notes:
--    - Inbound ports = Application's PUBLIC API
--    - Called by: API layer (or Presentation layer in apps)
--    - Examples: Execute use case functions
--    - Flow: API → Inbound Port (Use Case) → Domain
--
--  Design Pattern:
--    Use cases themselves serve as inbound ports, accepting DTOs
--    (commands) from the API layer.
--
--  See Also:
--    Application.Usecase - Use cases implement inbound ports
--  =========================================================================

package Application.Port.Inbound
  with Pure
is

end Application.Port.Inbound;
