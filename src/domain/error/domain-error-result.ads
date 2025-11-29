pragma Ada_2022;
--  =========================================================================
--  Domain.Error.Result - Generic Result monad for error handling
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Result[T] monad for railway-oriented programming. This is the core
--    functional error handling primitive for the entire application.
--
--  Architecture Notes:
--    - Generic over success type T
--    - Uses Domain.Error.Error_Type for all errors
--    - Pure domain implementation (no external dependencies)
--    - Enables functional composition and error propagation
--
--  Usage:
--    with Domain.Error.Result;
--
--    package String_Result is new Domain.Error.Result.Generic_Result
--      (T => String);
--
--    R : String_Result.Result := String_Result.Ok ("success");
--    if String_Result.Is_Ok (R) then
--       Value := String_Result.Value (R);
--    end if;
--
--  Design Pattern:
--    Railway-Oriented Programming:
--    - Ok track: Successful computation continues
--    - Error track: Error propagates (short-circuit)
--    - Forces explicit error handling at compile time
--
--  See Also:
--    Domain.Error - Error types used by this monad
--  =========================================================================

package Domain.Error.Result
  with Preelaborate
is

   --  ========================================================================
   --  Generic Result Type: Either monad for T or Error_Type
   --  ========================================================================

   --  This generic package must be instantiated for each success type T
   --  Example: package String_Result is new Generic_Result (T => String);
   --
   --  Flow:
   --  1. Operations that can fail return Result[T] instead of raising
   --  2. Caller checks Is_Ok/Is_Error before extracting value
   --  3. Forces explicit error handling at compile time

   generic
      type T is private;  --  The success value type
   package Generic_Result is

      --  Opaque result type - internal representation hidden
      type Result is private;

      --  =====================================================================
      --  Constructors
      --  =====================================================================

      function Ok (Value : T) return Result
      with
         Inline,
         Post => Is_Ok (Ok'Result);

      function Error (Kind : Error_Kind; Message : String) return Result
      with
         Inline,
         Post => Is_Error (Error'Result);

      --  =====================================================================
      --  Query functions
      --  =====================================================================

      function Is_Ok (Self : Result) return Boolean
      with Inline;

      function Is_Error (Self : Result) return Boolean
      with Inline;

      --  =====================================================================
      --  Value extraction
      --  =====================================================================

      function Value (Self : Result) return T
      with Pre => Is_Ok (Self), Inline;

      function Error_Info (Self : Result) return Error_Type
      with Pre => Is_Error (Self), Inline;

      --  =====================================================================
      --  Unwrap operations (extract value or use default)
      --  =====================================================================

      function Unwrap_Or (Self : Result; Default : T) return T
      with
         Post => (if Is_Ok (Self) then Unwrap_Or'Result = Value (Self)
                  else Unwrap_Or'Result = Default);
      --  Get the value if Ok, otherwise return Default
      --  Use this to provide a safe fallback value

      generic
         with function F return T;
      function Unwrap_Or_With (Self : Result) return T;
      --  Get the value if Ok, otherwise compute default lazily via F
      --  Use when default is expensive to compute

      function Expect (Self : Result; Message : String) return T;
      --  Get the value if Result is Ok; otherwise raise Program_Error with
      --  the provided custom message. Use only when you can document why
      --  Result should be Ok at the call site.

      --  =====================================================================
      --  Functional operations (transform and chain)
      --  =====================================================================

      generic
         with function F (X : T) return T;
      function Map (Self : Result) return Result
      with
         Post => (if Is_Ok (Self) then Is_Ok (Map'Result)
                  else Is_Error (Map'Result) and then
                       Error_Info (Map'Result) = Error_Info (Self));
      --  Transform the success value if Ok, propagate error if Error
      --  Example: Double_Result := Int_Result.Map (Double'Access)

      generic
         with function F (X : T) return Result;
      function And_Then (Self : Result) return Result
      with
         Post => (if Is_Error (Self) then And_Then'Result = Self);
      --  Chain fallible operations (monadic bind)
      --  If Self is Error, propagates error without calling F
      --  If Self is Ok, calls F with value (F might return Error)
      --  Example: Parse_File (Path).And_Then (Validate'Access)

      generic
         with function F (E : Error_Type) return Error_Type;
      function Map_Error (Self : Result) return Result
      with
         Post => (if Is_Ok (Self) then Is_Ok (Map_Error'Result) and then
                                       Value (Map_Error'Result) = Value (Self)
                  else Is_Error (Map_Error'Result));
      --  Transform the error value if Error, propagate Ok if Ok
      --  Use to add context to errors as they propagate up call stack

      --  =====================================================================
      --  Fallback and recovery
      --  =====================================================================

      function Fallback (Primary : Result; Alternative : Result) return Result
      with
         Post => (if Is_Ok (Primary) then Fallback'Result = Primary
                  else Fallback'Result = Alternative);
      --  Try Primary, if Error then use Alternative
      --  Both are eagerly evaluated

      generic
         with function F return Result;
      function Fallback_With (Self : Result) return Result;
      --  Try Self, if Error then compute alternative lazily via F
      --  Use when alternative is expensive to compute

      generic
         with function Handle (E : Error_Type) return T;
      function Recover (Self : Result) return T;
      --  Turn error into value via Handle function
      --  Always returns T (never fails)

      generic
         with function Handle (E : Error_Type) return Result;
      function Recover_With (Self : Result) return Result;
      --  Turn error into another Result via Handle function
      --  Handle might succeed or return different error

      --  =====================================================================
      --  Side effects (for logging/debugging)
      --  =====================================================================

      generic
         with procedure On_Ok (V : T);
         with procedure On_Error (E : Error_Type);
      function Tap (Self : Result) return Result
      with Post => Tap'Result = Self;
      --  Execute side effects without changing the Result
      --  Returns the same Result for chaining
      --  Example: Result.Tap (Log_Success'Access, Log_Error'Access)

   private

      --  Internal representation: discriminated record (tagged union pattern)
      type Result_State is (Ok_State, Error_State);

      type Result (State : Result_State := Error_State) is record
         case State is
            when Ok_State =>
               Success_Value : T;

            when Error_State =>
               Error_Value : Error_Type;
         end case;
      end record;

   end Generic_Result;

   --  ========================================================================
   --  Cross-Type Chaining: And_Then_Into
   --  ========================================================================
   --
   --  This generic function enables chaining fallible operations that return
   --  DIFFERENT Result types. This is essential for railway-oriented
   --  programming when transforming between types.
   --
   --  Example Usage:
   --    -- Given:
   --    --   Person_Result : package is new Generic_Result (T => Person);
   --    --   Unit_Result   : package is new Generic_Result (T => Unit);
   --    --   function Write_Greeting (P : Person) return Unit_Result.Result;
   --    --
   --    -- Chain Person creation to greeting output:
   --    function Chain_To_Unit is new And_Then_Into
   --      (T             => Person,
   --       U             => Unit,
   --       Source_Result => Person_Result,
   --       Target_Result => Unit_Result,
   --       F             => Write_Greeting);
   --    --
   --    -- Usage:
   --    Final_Result : Unit_Result.Result :=
   --      Chain_To_Unit (Person.Create (Name));
   --
   --  Design Pattern:
   --    This is the monadic bind (>>=) operation that allows crossing type
   --    boundaries while maintaining error propagation semantics.

   generic
      type T is private;
      type U is private;
      with package Source_Result is new Generic_Result (T => T);
      with package Target_Result is new Generic_Result (T => U);
      with function F (X : T) return Target_Result.Result;
   function And_Then_Into
     (Self : Source_Result.Result) return Target_Result.Result;
   --  Chain fallible operations that return different Result types
   --  If Self is Error, converts to Target_Result.Error (same error info)
   --  If Self is Ok, calls F with value (F might return Error)

end Domain.Error.Result;
