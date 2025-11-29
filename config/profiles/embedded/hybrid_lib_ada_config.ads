pragma Ada_2022;
--  ==========================================================================
--  Hybrid_Lib_Ada_Config - Embedded Profile
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Configuration profile for Ravenscar-compatible embedded systems.
--    Balanced configuration for memory-constrained devices.
--
--  Target Hardware:
--    - STM32F769 or similar (Cortex-M7 @ 200+ MHz)
--    - RAM: 512KB - 1MB
--    - Ravenscar runtime
--
--  Design Philosophy:
--    Conservative sizing with safety margins for IoT devices and
--    industrial control systems.
--  ==========================================================================

package Hybrid_Lib_Ada_Config is

   pragma Pure;

   --  =======================================================================
   --  Profile Metadata
   --  =======================================================================

   Profile_Name    : constant String := "embedded";
   Target_Platform : constant String := "Embedded (Ravenscar)";

   --  =======================================================================
   --  Build Profile (Alire standard)
   --  =======================================================================

   type Build_Profile_Kind is (release, validation, development);
   Build_Profile : constant Build_Profile_Kind := release;

   --  =======================================================================
   --  Bounded String Configuration
   --  =======================================================================

   --  Maximum length of person names
   --  Embedded profile: 64 characters (conservative for memory)
   Max_Name_Length : constant := 64;

   --  Maximum length of greeting messages
   --  Embedded profile: 128 characters
   Max_Message_Length : constant := 128;

   --  Maximum length of error messages
   --  Embedded profile: 256 characters
   Max_Error_Length : constant := 256;

   --  =======================================================================
   --  Runtime Configuration
   --  =======================================================================

   --  Enable assertions and contracts in this profile
   Enable_Contracts : constant Boolean := True;

   --  Disable debug output for production embedded
   Enable_Debug : constant Boolean := False;

end Hybrid_Lib_Ada_Config;
