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
--    Uses Functional.Try.Map_To_Result_With_Param to wrap exception-prone
--    Ada.Text_IO operations, converting them to Result type for
--    railway-oriented flow.
--  =========================================================================

with Ada.Text_IO;
with Ada.IO_Exceptions;
with Domain.Error;
with Domain.Unit;
with Functional.Try.Map_To_Result_With_Param;
pragma Elaborate_All (Functional.Try.Map_To_Result_With_Param);

package body Infrastructure.Adapter.Console_Writer is

   use Application.Port.Outbound.Writer;
   use Domain.Unit;

   --  ========================================================================
   --  Internal: Write Action (may raise exceptions)
   --  ========================================================================

   --  This function performs the actual I/O and may raise exceptions.
   --  It returns the domain Result type directly; exceptions are caught
   --  by Map_To_Result_With_Param.
   function Write_Action (Message : String) return Unit_Result.Result is
   begin
      Ada.Text_IO.Put_Line (Message);
      return Unit_Result.Ok (Unit_Value);
   end Write_Action;

   --  ========================================================================
   --  Error Factory for Map_To_Result_With_Param
   --  ========================================================================

   function Make_Write_Error
     (Kind : Domain.Error.Error_Kind; Message : String)
      return Unit_Result.Result
   is
   begin
      return Unit_Result.Error (Kind => Kind, Message => Message);
   end Make_Write_Error;

   --  ========================================================================
   --  Instantiate Map_To_Result_With_Param for Write Operation
   --  ========================================================================

   package Try_Write is new Functional.Try.Map_To_Result_With_Param
     (Error_Kind_Type    => Domain.Error.Error_Kind,
      Param_Type         => String,
      Result_Type        => Unit_Result.Result,
      Make_Error         => Make_Write_Error,
      Default_Error_Kind => Domain.Error.IO_Error,
      Action             => Write_Action);

   --  Exception mappings (declarative, not procedural)
   Write_Mappings : constant Try_Write.Mapping_Array :=
     [(Ada.IO_Exceptions.Device_Error'Identity, Domain.Error.IO_Error),
      (Ada.IO_Exceptions.Use_Error'Identity, Domain.Error.IO_Error)];

   -----------
   -- Write --
   -----------

   function Write
     (Message : String)
      return Application.Port.Outbound.Writer.Unit_Result.Result
   is
   begin
      return Try_Write.Run (Message, Write_Mappings);
   end Write;

end Infrastructure.Adapter.Console_Writer;
