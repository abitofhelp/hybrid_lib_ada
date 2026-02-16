pragma Ada_2022;
--  =========================================================================
--  Hybrid_Lib_Ada.Application.Usecase.Greet - Implementation of greeting use case
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Implements Execute function: validates Person, formats greeting message,
--    and writes output via injected Writer port using railway-oriented flow.
--  =========================================================================

with Hybrid_Lib_Ada.Domain.Error;
with Hybrid_Lib_Ada.Domain.Value_Object.Person;

package body Hybrid_Lib_Ada.Application.Usecase.Greet is

   use Hybrid_Lib_Ada.Application.Port.Outbound.Writer;

   ---------------------
   -- Format_Greeting --
   ---------------------

   --  Application-level greeting format (orchestration, not domain logic)
   --  The format "Hello, <name>!" is an application decision
   function Format_Greeting (Name : String) return String is
   begin
      return "Hello, " & Name & "!";
   end Format_Greeting;

   -------------
   -- Execute --
   -------------

   function Execute
     (Cmd : Hybrid_Lib_Ada.Application.Command.Greet.Greet_Command)
      return Hybrid_Lib_Ada.Application.Port.Outbound.Writer.Unit_Result.Result
   is
      --  Step 1: Extract name from DTO
      Name : constant String := Hybrid_Lib_Ada.Application.Command.Greet.Get_Name (Cmd);

      --  Step 2: Validate and create Person from name (domain validation)
      Person_Result :
        constant Hybrid_Lib_Ada.Domain.Value_Object.Person.Person_Result.Result :=
          Hybrid_Lib_Ada.Domain.Value_Object.Person.Create (Name);
   begin
      --  Check if person creation failed (railway-oriented programming)
      if Hybrid_Lib_Ada.Domain.Value_Object.Person.Person_Result.Is_Error (Person_Result) then
         --  Propagate validation error to caller
         declare
            Error_Info : constant Hybrid_Lib_Ada.Domain.Error.Error_Type :=
              Hybrid_Lib_Ada.Domain.Value_Object.Person.Person_Result.Error_Info
                (Person_Result);
         begin
            return
              Unit_Result.Error
                (Kind    => Error_Info.Kind,
                 Message =>
                   Hybrid_Lib_Ada.Domain.Error.Error_Strings.To_String (Error_Info.Message));
         end;
      end if;

      --  Extract validated Person
      declare
         Person : constant Hybrid_Lib_Ada.Domain.Value_Object.Person.Person :=
           Hybrid_Lib_Ada.Domain.Value_Object.Person.Person_Result.Value (Person_Result);

         --  Step 3: Generate greeting message (application-level formatting)
         Message : constant String :=
           Format_Greeting (Hybrid_Lib_Ada.Domain.Value_Object.Person.Get_Name (Person));

         --  Step 4: Write to console via output port (injected dependency)
         Write_Result : constant Unit_Result.Result := Writer (Message);
      begin
         --  Propagate result (success or failure) to caller
         return Write_Result;
      end;
   end Execute;

end Hybrid_Lib_Ada.Application.Usecase.Greet;
