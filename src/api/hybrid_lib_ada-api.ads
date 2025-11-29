pragma Ada_2022;
--  ===========================================================================
--  Hybrid_Lib_Ada.API - Public Library Interface
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Public API facade for the Hybrid_Lib_Ada library. Re-exports domain
--    types and application ports. Provides Greet operation that delegates
--    to the desktop composition root.
--
--  Architecture:
--    - Facade layer: Re-exports Domain and Application types
--    - Greet delegates to API.Desktop (composition root)
--    - API does NOT directly import Infrastructure
--
--  Usage:
--    with Hybrid_Lib_Ada.API;
--
--    procedure Main is
--       use Hybrid_Lib_Ada.API;
--       Cmd    : constant Greet_Command := Create_Greet_Command ("Alice");
--       Result : constant Unit_Result.Result := Greet (Cmd);
--    begin
--       if Unit_Result.Is_Ok (Result) then
--          -- Success - greeting was written to console
--       end if;
--    end Main;
--
--  ===========================================================================

--  Import domain types
with Domain.Value_Object.Person;
with Domain.Error;
with Domain.Unit;

--  Import application types
with Application.Command.Greet;
with Application.Port.Outbound.Writer;

package Hybrid_Lib_Ada.API is

   --  ========================================================================
   --  Re-export Domain Types
   --  ========================================================================

   --  Person value object
   subtype Person_Type is Domain.Value_Object.Person.Person;

   --  Person validation and creation
   function Create_Person
     (Name : String)
      return Domain.Value_Object.Person.Person_Result.Result
   renames Domain.Value_Object.Person.Create;

   function Get_Name (P : Person_Type) return String
   renames Domain.Value_Object.Person.Get_Name;

   --  Person Result type
   package Person_Result renames Domain.Value_Object.Person.Person_Result;

   --  Error types
   subtype Error_Type is Domain.Error.Error_Type;
   subtype Error_Kind is Domain.Error.Error_Kind;

   package Error_Strings renames Domain.Error.Error_Strings;

   --  ========================================================================
   --  Re-export Application Types
   --  ========================================================================

   --  Greet command DTO
   subtype Greet_Command is Application.Command.Greet.Greet_Command;

   function Create_Greet_Command
     (Name : String) return Greet_Command
   renames Application.Command.Greet.Create;

   function Get_Command_Name (Cmd : Greet_Command) return String
   renames Application.Command.Greet.Get_Name;

   --  Unit type for void operations
   subtype Unit is Domain.Unit.Unit;
   Unit_Value : Unit renames Domain.Unit.Unit_Value;

   --  Unit Result type
   package Unit_Result renames Application.Port.Outbound.Writer.Unit_Result;

   --  ========================================================================
   --  Greet Operation
   --  ========================================================================

   --  Execute the greet operation using default (console) output.
   --
   --  This is a convenience function that delegates to API.Desktop.Greet.
   --  For custom output adapters, use API.Operations generic directly.
   --
   --  Returns:
   --    - Ok(Unit) if greeting succeeded
   --    - Error(Validation_Error) if name invalid
   --    - Error(Infrastructure_Error) if write failed

   function Greet
     (Cmd : Greet_Command)
      return Unit_Result.Result;

end Hybrid_Lib_Ada.API;
