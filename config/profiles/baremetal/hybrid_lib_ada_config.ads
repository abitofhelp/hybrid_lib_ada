pragma Ada_2022;
--  ==========================================================================
--  Hybrid_Lib_Ada_Config - Bare Metal Profile
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Configuration profile for Zero Footprint Profile (ZFP) systems.
--    Minimal configuration for bare metal microcontrollers.
--
--  Target Hardware:
--    - STM32F4xx or similar (Cortex-M4 @ 100+ MHz)
--    - RAM: 128KB - 256KB
--    - ZFP runtime (no OS, minimal stdlib)
--
--  Design Philosophy:
--    Minimal sizing for extremely constrained memory environments.
--    Every byte counts in ZFP systems.
--  ==========================================================================

package Hybrid_Lib_Ada_Config is

   pragma Pure;

   --  =======================================================================
   --  Profile Metadata
   --  =======================================================================

   Profile_Name    : constant String := "baremetal";
   Target_Platform : constant String := "Bare Metal (ZFP)";

   --  =======================================================================
   --  Build Profile (Alire standard)
   --  =======================================================================

   type Build_Profile_Kind is (release, validation, development);
   Build_Profile : constant Build_Profile_Kind := release;

   --  =======================================================================
   --  Bounded String Configuration
   --  =======================================================================

   --  Maximum length of person names
   --  Bare metal profile: 32 characters (minimal)
   Max_Name_Length : constant := 32;

   --  Maximum length of greeting messages
   --  Bare metal profile: 64 characters
   Max_Message_Length : constant := 64;

   --  Maximum length of error messages
   --  Bare metal profile: 128 characters
   Max_Error_Length : constant := 128;

   --  =======================================================================
   --  Runtime Configuration
   --  =======================================================================

   --  Disable contracts to minimize overhead on ZFP
   Enable_Contracts : constant Boolean := False;

   --  Disable debug output for bare metal
   Enable_Debug : constant Boolean := False;

end Hybrid_Lib_Ada_Config;
