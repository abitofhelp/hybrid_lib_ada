pragma Ada_2022;
--  ===========================================================================
--  Hybrid_Lib_Ada.API - Public Library Interface (Body)
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Implementation of public API facade. Delegates operations to the
--    desktop composition root (API.Desktop).
--
--  Architecture Note:
--    API body imports API.Desktop (same layer child package), which is
--    the composition root. API does NOT import Infrastructure directly.
--    This maintains clean hexagonal boundaries.
--
--  ===========================================================================

with Hybrid_Lib_Ada.API.Desktop;

package body Hybrid_Lib_Ada.API is

   --  ========================================================================
   --  Greet Operation
   --  ========================================================================

   function Greet
     (Cmd : Greet_Command)
      return Unit_Result.Result
   is
   begin
      return Hybrid_Lib_Ada.API.Desktop.Greet (Cmd);
   end Greet;

end Hybrid_Lib_Ada.API;
