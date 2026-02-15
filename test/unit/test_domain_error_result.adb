pragma Ada_2022;
--  ======================================================================
--  Test_Domain_Error_Result
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Domain.Error.Result monad functionality.
--    Tests Ok/Error constructors, Is_Ok/Is_Error queries, and then value
--    extraction.
--  ======================================================================

with Domain;
with Ada.Text_IO;
with Domain.Error;
with Domain.Error.Result;
with Test_Framework;

procedure Test_Domain_Error_Result is

   use Ada.Text_IO;
   use Domain.Error;

   --  Test statistics
   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;

   --  Helper procedure to run a test
   pragma Style_Checks (Off);
   procedure Run_Test (Name : String; Passed : Boolean) is
   begin
      Total_Tests := Total_Tests + 1;
      if Passed then
         Passed_Tests := Passed_Tests + 1;
         Test_Framework.Print_Pass (Name);
      else
         Test_Framework.Print_Fail (Name);
      end if;
   end Run_Test;
   pragma Style_Checks (On);

   --  Instantiate Result for Integer (for testing)
   package Int_Result is new Domain.Error.Result.Generic_Result (T => Integer);

   --  Instantiate Result for Boolean (for testing)
   package Bool_Result is new
     Domain.Error.Result.Generic_Result (T => Boolean);

begin
   Put_Line ("========================================");
   Put_Line ("Testing: Domain.Error.Result");
   Put_Line ("========================================");
   New_Line;

   --  ========================================================================
   --  Test: Ok construction and then Is_Ok query
   --  ========================================================================

   declare
      R : constant Int_Result.Result := Int_Result.Ok (42);
   begin
      Run_Test
        ("Ok construction - Is_Ok returns true", Int_Result.Is_Ok (R));
      Run_Test
        ("Ok construction - Is_Error returns false",
         not Int_Result.Is_Error (R));
   end;

   --  ========================================================================
   --  Test: Ok value extraction
   --  ========================================================================

   declare
      R   : constant Int_Result.Result := Int_Result.Ok (123);
      Val : Integer;
   begin
      if Int_Result.Is_Ok (R) then
         Val := Int_Result.Value (R);
         Run_Test ("Ok value extraction - correct value", Val = 123);
      else
         Run_Test ("Ok value extraction - Result should be Ok", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Error construction and then Is_Error query
   --  ========================================================================

   declare
      R : constant Int_Result.Result :=
        Int_Result.Error
          (Kind => Validation_Error, Message => "Test validation error");
   begin
      Run_Test
        ("Error construction - Is_Error returns true",
         Int_Result.Is_Error (R));
      Run_Test
        ("Error construction - Is_Ok returns false", not Int_Result.Is_Ok (R));
   end;

   --  ========================================================================
   --  Test: Error info extraction
   --  ========================================================================

   declare
      R    : constant Int_Result.Result :=
        Int_Result.Error
          (Kind => IO_Error, Message => "Test IO error");
      Info : Error_Type;
   begin
      if Int_Result.Is_Error (R) then
         Info := Int_Result.Error_Info (R);
         Run_Test
           ("Error info - correct kind", Info.Kind = IO_Error);
         Run_Test
           ("Error info - correct message",
            Error_Strings.To_String (Info.Message) = "Test IO error");
      else
         Run_Test ("Error info extraction - Result should be Error", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Result with Boolean type
   --  ========================================================================

   declare
      R : constant Bool_Result.Result := Bool_Result.Ok (True);
   begin
      Run_Test
        ("Boolean Result - Is_Ok returns true", Bool_Result.Is_Ok (R));
      if Bool_Result.Is_Ok (R) then
         Run_Test
           ("Boolean Result - correct value",
            Bool_Result.Value (R) = True);
      end if;
   end;

   --  ========================================================================
   --  Test: Error with empty message
   --  ========================================================================

   declare
      R    : constant Int_Result.Result :=
        Int_Result.Error (Kind => Validation_Error, Message => "");
      Info : Error_Type;
   begin
      Run_Test
        ("Error with empty message - Is_Error", Int_Result.Is_Error (R));
      if Int_Result.Is_Error (R) then
         Info := Int_Result.Error_Info (R);
         Run_Test
           ("Error with empty message - message is empty",
            Error_Strings.Length (Info.Message) = 0);
      end if;
   end;

   --  ========================================================================
   --  Test: Multiple Ok values don't interfere
   --  ========================================================================

   declare
      R1 : constant Int_Result.Result := Int_Result.Ok (100);
      R2 : constant Int_Result.Result := Int_Result.Ok (200);
   begin
      Run_Test
        ("Multiple Ok values - R1 has correct value",
         Int_Result.Is_Ok (R1) and then Int_Result.Value (R1) = 100);
      Run_Test
        ("Multiple Ok values - R2 has correct value",
         Int_Result.Is_Ok (R2) and then Int_Result.Value (R2) = 200);
   end;

   --  ========================================================================
   --  Test: Multiple Error values don't interfere
   --  ========================================================================

   declare
      R1   : constant Int_Result.Result :=
        Int_Result.Error (Kind => Validation_Error, Message => "Error 1");
      R2   : constant Int_Result.Result :=
        Int_Result.Error (Kind => IO_Error, Message => "Error 2");
      Info1 : Error_Type;
      Info2 : Error_Type;
   begin
      if Int_Result.Is_Error (R1) and then Int_Result.Is_Error (R2) then
         Info1 := Int_Result.Error_Info (R1);
         Info2 := Int_Result.Error_Info (R2);
         Run_Test
           ("Multiple errors - R1 has correct kind",
            Info1.Kind = Validation_Error);
         Run_Test
           ("Multiple errors - R1 has correct message",
            Error_Strings.To_String (Info1.Message) = "Error 1");
         Run_Test
           ("Multiple errors - R2 has correct kind",
            Info2.Kind = IO_Error);
         Run_Test
           ("Multiple errors - R2 has correct message",
            Error_Strings.To_String (Info2.Message) = "Error 2");
      else
         Run_Test ("Multiple errors test failed", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Long error message (boundary test)
   --  ========================================================================

   declare
      Long_Message : constant String (1 .. 500) := [others => 'X'];
      R            : constant Int_Result.Result :=
        Int_Result.Error (Kind => Validation_Error, Message => Long_Message);
      Info : Error_Type;
   begin
      Run_Test
        ("Long error message - Is_Error", Int_Result.Is_Error (R));
      if Int_Result.Is_Error (R) then
         Info := Int_Result.Error_Info (R);
         --  Message should be truncated to max length (512)
         Run_Test
           ("Long error message - message stored",
            Error_Strings.Length (Info.Message) > 0);
      end if;
   end;

   --  ========================================================================
   --  Test: From_Error constructor
   --  ========================================================================

   declare
      Err : constant Error_Type :=
        (Kind    => Validation_Error,
         Message => Error_Strings.To_Bounded_String ("validation failed"));
      R   : constant Int_Result.Result := Int_Result.From_Error (Err);
   begin
      Run_Test ("From_Error - Is_Error returns true", Int_Result.Is_Error (R));
      if Int_Result.Is_Error (R) then
         Run_Test
           ("From_Error - preserves error kind",
            Int_Result.Error_Info (R).Kind = Validation_Error);
      end if;
   end;

   --  ========================================================================
   --  Test: Map with Ok transforms value
   --  ========================================================================

   declare
      function Double (X : Integer) return Integer is (X * 2);
      function Map_Double is new Int_Result.Map (F => Double);

      R      : constant Int_Result.Result := Int_Result.Ok (21);
      Mapped : constant Int_Result.Result := Map_Double (R);
   begin
      Run_Test
        ("Map with Ok transforms value",
         Int_Result.Is_Ok (Mapped) and then Int_Result.Value (Mapped) = 42);
   end;

   --  ========================================================================
   --  Test: Bimap with Ok transforms value
   --  ========================================================================

   declare
      function Double (X : Integer) return Integer is (X * 2);

      function Change_Kind (E : Error_Type) return Error_Type is
        ((Kind => Internal_Error, Message => E.Message));

      function Transform is new Int_Result.Bimap
        (Map_Ok  => Double,
         Map_Err => Change_Kind);

      R      : constant Int_Result.Result := Int_Result.Ok (21);
      Mapped : constant Int_Result.Result := Transform (R);
   begin
      Run_Test
        ("Bimap with Ok transforms value",
         Int_Result.Is_Ok (Mapped) and then Int_Result.Value (Mapped) = 42);
   end;

   --  ========================================================================
   --  Test: Bimap with Error transforms error
   --  ========================================================================

   declare
      function Double (X : Integer) return Integer is (X * 2);

      function Change_Kind (E : Error_Type) return Error_Type is
        ((Kind => Internal_Error, Message => E.Message));

      function Transform is new Int_Result.Bimap
        (Map_Ok  => Double,
         Map_Err => Change_Kind);

      R      : constant Int_Result.Result :=
        Int_Result.Error (Kind => Validation_Error, Message => "test");
      Mapped : constant Int_Result.Result := Transform (R);
   begin
      Run_Test
        ("Bimap with Error transforms error kind",
         Int_Result.Is_Error (Mapped) and then
         Int_Result.Error_Info (Mapped).Kind = Internal_Error);
   end;

   --  ========================================================================
   --  Test: Ensure with Ok and predicate passes
   --  ========================================================================

   declare
      function Is_Positive (X : Integer) return Boolean is (X > 0);

      function To_Validation_Error (X : Integer) return Error_Type;
      function To_Validation_Error (X : Integer) return Error_Type is
         pragma Unreferenced (X);
      begin
         return
           (Kind    => Validation_Error,
            Message => Error_Strings.To_Bounded_String ("Not positive"));
      end To_Validation_Error;

      function Validate is new Int_Result.Ensure
        (Pred     => Is_Positive,
         To_Error => To_Validation_Error);

      R      : constant Int_Result.Result := Int_Result.Ok (10);
      Result : constant Int_Result.Result := Validate (R);
   begin
      Run_Test
        ("Ensure keeps Ok if predicate holds",
         Int_Result.Is_Ok (Result) and then Int_Result.Value (Result) = 10);
   end;

   --  ========================================================================
   --  Test: Ensure with Ok and predicate fails
   --  ========================================================================

   declare
      function Is_Positive (X : Integer) return Boolean is (X > 0);

      function To_Validation_Error (X : Integer) return Error_Type;
      function To_Validation_Error (X : Integer) return Error_Type is
         pragma Unreferenced (X);
      begin
         return
           (Kind    => Validation_Error,
            Message => Error_Strings.To_Bounded_String ("Not positive"));
      end To_Validation_Error;

      function Validate is new Int_Result.Ensure
        (Pred     => Is_Positive,
         To_Error => To_Validation_Error);

      R      : constant Int_Result.Result := Int_Result.Ok (-5);
      Result : constant Int_Result.Result := Validate (R);
   begin
      Run_Test
        ("Ensure converts to Error if predicate fails",
         Int_Result.Is_Error (Result));
   end;

   --  ========================================================================
   --  Test: With_Context on Error adds context
   --  ========================================================================

   declare
      function Add_Location (E : Error_Type; Where : String) return Error_Type;
      function Add_Location (E : Error_Type; Where : String) return Error_Type
      is
         use Error_Strings;
         New_Msg : constant String := To_String (E.Message) & " at " & Where;
      begin
         return (Kind => E.Kind, Message => To_Bounded_String (New_Msg));
      end Add_Location;

      function Add_Context is new
        Int_Result.With_Context (Add => Add_Location);

      R      : constant Int_Result.Result :=
        Int_Result.Error (Kind => Parse_Error, Message => "file not found");
      Result : constant Int_Result.Result := Add_Context (R, "read_config");
      Info   : Error_Type;
   begin
      if Int_Result.Is_Error (Result) then
         Info := Int_Result.Error_Info (Result);
         Run_Test
           ("With_Context on Error adds context",
            Error_Strings.To_String (Info.Message) =
               "file not found at read_config");
      else
         Run_Test ("With_Context should return Error", False);
      end if;
   end;

   --  ========================================================================
   --  Test: Fallback with Error primary returns alternative
   --  ========================================================================

   declare
      Primary     : constant Int_Result.Result :=
        Int_Result.Error (Kind => Parse_Error, Message => "Error");
      Alternative : constant Int_Result.Result := Int_Result.Ok (99);
      Result      : constant Int_Result.Result :=
        Int_Result.Fallback (Primary, Alternative);
   begin
      Run_Test
        ("Fallback with Error returns alternative",
         Int_Result.Is_Ok (Result) and then Int_Result.Value (Result) = 99);
   end;

   --  ========================================================================
   --  Test: Recover with Error calls handler
   --  ========================================================================

   declare
      function Handle_Error (E : Error_Type) return Integer;
      function Handle_Error (E : Error_Type) return Integer is
         pragma Unreferenced (E);
      begin
         return -1;
      end Handle_Error;

      function Recover_From_Error is new Int_Result.Recover
        (Handle => Handle_Error);

      R   : constant Int_Result.Result :=
        Int_Result.Error (Kind => Validation_Error, Message => "Error");
      Val : constant Integer := Recover_From_Error (R);
   begin
      Run_Test ("Recover with Error calls handler", Val = -1);
   end;

   --  ========================================================================
   --  Test: Tap with Ok calls On_Ok
   --  ========================================================================

   declare
      On_Ok_Called    : Boolean := False;
      On_Error_Called : Boolean := False;

      procedure On_Ok (V : Integer);
      procedure On_Ok (V : Integer) is
         pragma Unreferenced (V);
      begin
         On_Ok_Called := True;
      end On_Ok;

      procedure On_Error (E : Error_Type);
      procedure On_Error (E : Error_Type) is
         pragma Unreferenced (E);
      begin
         On_Error_Called := True;
      end On_Error;

      function Tap_Side_Effects is new Int_Result.Tap
        (On_Ok    => On_Ok,
         On_Error => On_Error);

      R      : constant Int_Result.Result := Int_Result.Ok (42);
      Result : constant Int_Result.Result := Tap_Side_Effects (R);
   begin
      Run_Test
        ("Tap with Ok calls On_Ok",
         On_Ok_Called and then not On_Error_Called and then
         Int_Result.Is_Ok (Result));
   end;

   --  Print summary
   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: Domain.Error.Result");
   Put_Line ("========================================");
   Put_Line ("Total tests: " & Total_Tests'Image);
   Put_Line ("Passed:      " & Passed_Tests'Image);
   Put_Line ("Failed:      " & Natural'Image (Total_Tests - Passed_Tests));
   New_Line;

   --  Register results with test framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);

end Test_Domain_Error_Result;
