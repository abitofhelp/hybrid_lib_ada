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
--    - Facade layer: Re-exports Hybrid_Lib_Ada.Domain and Hybrid_Lib_Ada.Application types
--    - Greet delegates to API.Desktop (composition root)
--    - API does NOT directly import Hybrid_Lib_Ada.Infrastructure
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
with Hybrid_Lib_Ada.Domain.Value_Object.Person;
with Hybrid_Lib_Ada.Domain.Error;
with Hybrid_Lib_Ada.Domain.Unit;

--  Import application types
with Hybrid_Lib_Ada.Application.Command.Greet;
with Hybrid_Lib_Ada.Application.Port.Outbound.Writer;

package Hybrid_Lib_Ada.API is

   --  ========================================================================
   --  Re-export Hybrid_Lib_Ada.Domain Types
   --  ========================================================================

   --  Person value object
   subtype Person_Type is Hybrid_Lib_Ada.Domain.Value_Object.Person.Person;

   --  Person validation and creation
   function Create_Person
     (Name : String)
      return Hybrid_Lib_Ada.Domain.Value_Object.Person.Person_Result.Result
   renames Hybrid_Lib_Ada.Domain.Value_Object.Person.Create;

   function Get_Name (P : Person_Type) return String
   renames Hybrid_Lib_Ada.Domain.Value_Object.Person.Get_Name;

   --  Person Result type
   package Person_Result renames Hybrid_Lib_Ada.Domain.Value_Object.Person.Person_Result;

   --  Error types
   subtype Error_Type is Hybrid_Lib_Ada.Domain.Error.Error_Type;
   subtype Error_Kind is Hybrid_Lib_Ada.Domain.Error.Error_Kind;

   package Error_Strings renames Hybrid_Lib_Ada.Domain.Error.Error_Strings;

   --  ========================================================================
   --  Re-export Hybrid_Lib_Ada.Application Types
   --  ========================================================================

   --  Greet command DTO
   subtype Greet_Command is Hybrid_Lib_Ada.Application.Command.Greet.Greet_Command;

   function Create_Greet_Command
     (Name : String) return Greet_Command
   renames Hybrid_Lib_Ada.Application.Command.Greet.Create;

   function Get_Command_Name (Cmd : Greet_Command) return String
   renames Hybrid_Lib_Ada.Application.Command.Greet.Get_Name;

   --  Unit type for void operations
   subtype Unit is Hybrid_Lib_Ada.Domain.Unit.Unit;
   Unit_Value : Unit renames Hybrid_Lib_Ada.Domain.Unit.Unit_Value;

   --  Unit Result type
   package Unit_Result renames Hybrid_Lib_Ada.Application.Port.Outbound.Writer.Unit_Result;

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
