pragma Ada_2022;
--  =========================================================================
--  Application.Command.Greet - Implementation of DTO helpers
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Implements factory function Create and accessor Get_Name for the
--    Greet_Command DTO used to pass data between API layer and use case.
--  =========================================================================

package body Application.Command.Greet is

   use Name_Strings;

   ------------
   -- Create --
   ------------

   function Create (Name : String) return Greet_Command is
   begin
      return (Name => To_Bounded_String (Name));
   end Create;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Cmd : Greet_Command) return String is
   begin
      return To_String (Cmd.Name);
   end Get_Name;

end Application.Command.Greet;
