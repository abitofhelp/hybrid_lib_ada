pragma Ada_2022;
--  =========================================================================
--  Infrastructure.Adapter - Parent package for concrete adapters
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Parent package for concrete adapter implementations.
--    Adapters implement application output ports using real external systems.
--
--  Architecture Notes:
--    - Adapters implement port interfaces defined by application
--    - Each adapter wraps an external dependency
--    - Adapters convert exceptions to Result types
--    - Responsibility: Bridge between app and external world
--
--  Design Pattern:
--    ADAPTER pattern (Hexagonal Architecture / Ports & Adapters):
--    - Application defines interface it needs (port)
--    - Adapter implements that interface using external system
--    - api/desktop (composition root) wires adapter to application
--
--  See Also:
--    Infrastructure.Adapter.Console_Writer - Console output adapter
--    Application.Port.Outbound - Port interfaces
--  =========================================================================

package Infrastructure.Adapter
  with Pure
is

end Infrastructure.Adapter;
