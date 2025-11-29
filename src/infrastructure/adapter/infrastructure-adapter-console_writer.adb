pragma Ada_2022;
--  =========================================================================
--  Infrastructure.Adapter.Console_Writer - Console output implementation
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Implements Write function that outputs text to console via Ada.Text_IO,
--    wrapping exceptions into Result type for railway-oriented error handling.
--
--  Implementation Notes:
--    Uses Functional.Try.Try_To_Result_With_Param to wrap exception-prone
--    Ada.Text_IO operations, converting them to Result type for
--    railway-oriented flow.
--  =========================================================================

with Ada.Text_IO;
with Ada.Exceptions;
with Domain.Error;
with Domain.Unit;
with Functional.Result;
with Functional.Try;

package body Infrastructure.Adapter.Console_Writer is

   use Application.Port.Outbound.Writer;
   use Domain.Unit;
   use Ada.Exceptions;

   --  ========================================================================
   --  Internal: Exception-prone Write Action
   --  ========================================================================

   --  This function performs the actual I/O and may raise exceptions.
   --  It will be wrapped by Functional.Try.Try_To_Result_With_Param.
   pragma Style_Checks (Off);
   function Write_Action (Message : String) return Unit is
   pragma Style_Checks (On);
   begin
      Ada.Text_IO.Put_Line (Message);
      return Unit_Value;
   end Write_Action;

   --  ========================================================================
   --  Internal: Map Exception to Domain Error
   --  ========================================================================

   --  Converts Ada exception to domain error type
   pragma Style_Checks (Off);
   function Map_Exception
     (Exc : Exception_Occurrence) return Domain.Error.Error_Type is
   pragma Style_Checks (On);
   begin
      return
        (Kind    => Domain.Error.IO_Error,
         Message =>
           Domain.Error.Error_Strings.To_Bounded_String
             ("Console write failed: " & Exception_Name (Exc)));
   end Map_Exception;

   --  ========================================================================
   --  Instantiate Functional.Result for Unit
   --  ========================================================================

   package Unit_Functional_Result is new
     Functional.Result (T => Unit, E => Domain.Error.Error_Type);

   --  ========================================================================
   --  Instantiate Try.Try_To_Result_With_Param for Write Operation
   --  ========================================================================

   function Write_With_Try is new
     Functional.Try.Try_To_Result_With_Param
       (T             => Unit,
        E             => Domain.Error.Error_Type,
        Param         => String,
        Result_Pkg    => Unit_Functional_Result,
        Map_Exception => Map_Exception,
        Action        => Write_Action);

   --  ========================================================================
   --  Convert Functional.Result to Domain.Result
   --  ========================================================================

   pragma Style_Checks (Off);
   function To_Domain_Result
     (FR : Unit_Functional_Result.Result) return Unit_Result.Result is
   pragma Style_Checks (On);
   begin
      if Unit_Functional_Result.Is_Ok (FR) then
         return Unit_Result.Ok (Unit_Functional_Result.Value (FR));
      else
         declare
            Err : constant Domain.Error.Error_Type :=
              Unit_Functional_Result.Error (FR);
         begin
            return
              Unit_Result.Error
                (Kind    => Err.Kind,
                 Message =>
                   Domain.Error.Error_Strings.To_String (Err.Message));
         end;
      end if;
   end To_Domain_Result;

   -----------
   -- Write --
   -----------

   function Write
     (Message : String)
      return Application.Port.Outbound.Writer.Unit_Result.Result
   is
      FR : constant Unit_Functional_Result.Result := Write_With_Try (Message);
   begin
      --  Convert from Functional.Result to Domain.Result
      return To_Domain_Result (FR);
   end Write;

end Infrastructure.Adapter.Console_Writer;
