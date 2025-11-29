pragma Ada_2022;
--  ==========================================================================
--  Hybrid_Lib_Ada_Config - Standard Profile
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Configuration profile for desktop/server environments.
--    Maximum flexibility, generous string limits.
--
--  Target Platform:
--    - Linux / macOS / Windows
--    - RAM: 1+ GB
--    - Full Ada runtime
--
--  Design Philosophy:
--    Generous limits suitable for desktop applications with no memory
--    pressure. All bounded strings sized for typical use cases plus margin.
--  ==========================================================================

package Hybrid_Lib_Ada_Config is

   pragma Pure;

   --  =======================================================================
   --  Profile Metadata
   --  =======================================================================

   Profile_Name    : constant String := "standard";
   Target_Platform : constant String := "Desktop/Server";

   --  =======================================================================
   --  Build Profile (Alire standard)
   --  =======================================================================

   type Build_Profile_Kind is (release, validation, development);
   Build_Profile : constant Build_Profile_Kind := development;

   --  =======================================================================
   --  Bounded String Configuration
   --  =======================================================================

   --  Maximum length of person names
   --  Standard profile: 128 characters (generous for international names)
   Max_Name_Length : constant := 128;

   --  Maximum length of greeting messages
   --  Standard profile: 256 characters
   Max_Message_Length : constant := 256;

   --  Maximum length of error messages
   --  Standard profile: 512 characters
   Max_Error_Length : constant := 512;

   --  =======================================================================
   --  Runtime Configuration
   --  =======================================================================

   --  Enable assertions and contracts in this profile
   Enable_Contracts : constant Boolean := True;

   --  Enable debug output in this profile
   Enable_Debug : constant Boolean := True;

end Hybrid_Lib_Ada_Config;
