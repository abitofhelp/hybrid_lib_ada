pragma Ada_2022;
--  =========================================================================
--  Domain.Types.Option - Generic Option/Maybe monad
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Option[T] monad for handling nullable/optional values without null
--    pointers. Provides type-safe handling of values that may or may not
--    be present.
--
--  Architecture Notes:
--    - Generic over value type T
--    - Pure domain implementation (no external dependencies)
--    - Enables functional composition of optional values
--    - Alternative to null pointers and access types
--
--  Usage:
--    with Domain.Types.Option;
--
--    package String_Option is new Domain.Types.Option (T => String);
--
--    Opt : String_Option.Option := String_Option.Of_Value ("value");
--    if String_Option.Is_Some (Opt) then
--       Process (String_Option.Value (Opt));
--    end if;
--
--  Design Pattern:
--    - Of_Value (value): Contains a value (named to avoid reserved 'Some')
--    - None: No value present
--    - Forces explicit handling of missing values at compile time
--
--  See Also:
--    Domain.Error.Result - For computations that can fail with error
--  =========================================================================

package Domain.Types.Option
  with Preelaborate
is

   --  ========================================================================
   --  Generic Option Type: Maybe monad for T
   --  ========================================================================

   --  This generic package must be instantiated for each value type T
   --  Example: package String_Option is new Option (T => String);

   generic
      type T is private;  --  The value type
   package Generic_Option is

      --  Opaque option type - internal representation hidden
      type Option is private;

      --  =====================================================================
      --  Constructors
      --  =====================================================================

      function Of_Value (Value : T) return Option
      with
         Inline,
         Post => Is_Some (Of_Value'Result);
      --  Create an Option containing a value (named 'Of_Value' since 'Some'
      --  is a reserved word in Ada 2012+ for quantified expressions)

      function None return Option
      with
         Inline,
         Post => Is_None (None'Result);
      --  Create an empty Option (no value)

      --  =====================================================================
      --  Query functions
      --  =====================================================================

      function Is_Some (Self : Option) return Boolean
      with Inline;
      --  Returns True if Option contains a value

      function Is_None (Self : Option) return Boolean
      with Inline;
      --  Returns True if Option is empty

      --  =====================================================================
      --  Value extraction
      --  =====================================================================

      function Value (Self : Option) return T
      with Pre => Is_Some (Self), Inline;
      --  Get the contained value
      --  Precondition: Option must contain a value

      --  =====================================================================
      --  Unwrap operations (extract value or use default)
      --  =====================================================================

      function Unwrap_Or (Self : Option; Default : T) return T
      with
         Post => (if Is_Some (Self) then Unwrap_Or'Result = Value (Self)
                  else Unwrap_Or'Result = Default);
      --  Get the value if Some, otherwise return Default
      --  Use this to provide a safe fallback value

      generic
         with function F return T;
      function Unwrap_Or_With (Self : Option) return T;
      --  Get the value if Some, otherwise compute default lazily via F
      --  Use when default is expensive to compute

      --  =====================================================================
      --  Functional operations (transform and chain)
      --  =====================================================================

      generic
         with function F (X : T) return T;
      function Map (Self : Option) return Option
      with
         Post => (if Is_Some (Self) then Is_Some (Map'Result)
                  else Is_None (Map'Result));
      --  Transform the contained value if Some, propagate None if None
      --  Example: Double_Option := Int_Option.Map (Double'Access)

      generic
         with function F (X : T) return Option;
      function And_Then (Self : Option) return Option
      with
         Post => (if Is_None (Self) then And_Then'Result = Self);
      --  Chain optional operations (monadic bind)
      --  If Self is None, propagates None without calling F
      --  If Self is Some, calls F with value (F might return None)
      --  Example: Get_User (Id).And_Then (Get_Email'Access)

      generic
         with function Pred (X : T) return Boolean;
      function Filter (Self : Option) return Option;
      --  Keep value only if predicate holds, otherwise None
      --  If None, returns None
      --  If Some and Pred(value) is True, returns Some(value)
      --  If Some and Pred(value) is False, returns None

      --  =====================================================================
      --  Fallback
      --  =====================================================================

      function Or_Else (Primary : Option; Alternative : Option) return Option
      with
         Post => (if Is_Some (Primary) then Or_Else'Result = Primary
                  else Or_Else'Result = Alternative);
      --  Try Primary, if None then use Alternative
      --  Both are eagerly evaluated

      generic
         with function F return Option;
      function Or_Else_With (Self : Option) return Option;
      --  Try Self, if None then compute alternative lazily via F
      --  Use when alternative is expensive to compute

   private

      --  Internal representation: discriminated record (tagged union pattern)
      type Option_Kind is (Some_Kind, None_Kind);

      type Option (Kind : Option_Kind := None_Kind) is record
         case Kind is
            when Some_Kind =>
               Value_Data : T;

            when None_Kind =>
               null;
         end case;
      end record;

   end Generic_Option;

end Domain.Types.Option;
