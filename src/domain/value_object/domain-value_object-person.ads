pragma Ada_2022;
--  =========================================================================
--  Domain.Value_Object.Person - Person value object for the greeter domain
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  Purpose:
--    Defines the Person value object representing a person's name.
--    Value object: immutable, validated at construction, uses bounded strings.
--    Returns Result type for validation (no exceptions).
--    Domain provides data (Get_Name), Application formats output.
--
--  Usage:
--    Result := Create ("Alice");
--    if Person_Result.Is_Ok (Result) then
--       Person := Person_Result.Value (Result);
--       Name := Get_Name (Person);  --  Use in application layer
--    end if;
--
--  Design Notes:
--    - Value object pattern: immutable after creation
--    - Smart constructor (Create) enforces validation
--    - Uses bounded strings (memory safe, no heap allocation)
--    - Returns Result type (no exceptions)
--    - Pure domain logic - ZERO external crate dependencies
--
--  See Also:
--    Domain.Error.Result - Result type for error handling
--  =========================================================================

with Ada.Strings.Bounded;
with Domain.Error.Result;

package Domain.Value_Object.Person
  with Preelaborate
is

   --  Maximum name length (reasonable limit for person names)
   Max_Name_Length : constant := 100;

   --  Bounded string for name storage (no dynamic allocation)
   package Name_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => Max_Name_Length);

   --  ========================================================================
   --  Person Value Object
   --  ========================================================================

   --  ========================================================================
   --  DESIGN DECISION: Public Record for Generic Instantiation
   --  ========================================================================
   --  Person is a public record (not private) to enable generic instantiation:
   --    package Person_Result is new Generic_Result (T => Person);
   --
   --  Ada generics require visibility into the type structure. If Person were
   --  private, external code couldn't instantiate Generic_Result with it.
   --
   --  Trade-off: Direct construction (bypassing Create) is possible but
   --  strongly discouraged. Clients MUST use Create for validation.
   --
   --  Future SPARK: When SPARK is added, Type_Invariant => Is_Valid_Person
   --  will enforce the invariant even for direct construction.
   --  ========================================================================
   type Person is record
      Name_Value : Name_Strings.Bounded_String;
   end record;

   --  Instantiate Result type for Person
   package Person_Result is new
     Domain.Error.Result.Generic_Result (T => Person);

   --  ========================================================================
   --  DESIGN DECISION: Is_Valid_Person Not Called in Create
   --  ========================================================================
   --  Is_Valid_Person is NOT called inside Create because:
   --    1. Create performs inline validation (empty check, length check)
   --    2. Calling Is_Valid_Person in Create would be redundant
   --    3. Is_Valid_Person exists for external use:
   --       - Type invariant documentation
   --       - Runtime assertions/debugging: pragma Assert (Is_Valid_Person (P))
   --       - Future SPARK Type_Invariant annotation
   --  ========================================================================
   function Is_Valid_Person (P : Person) return Boolean;

   --  ========================================================================
   --  Smart Constructor: Validates and creates Person
   --  ========================================================================

   --  This is the RECOMMENDED way to create a Person (validates input)
   --  Validation rules:
   --   1. Name must not be empty
   --   2. Name must not exceed Max_Name_Length
   --   3. Name may contain spaces and Unicode characters; whitespace is
   --      preserved exactly as provided
   --
   --  Returns: Result[Person] - Ok if valid, Error if validation fails
   function Create (Name : String) return Person_Result.Result
   with
     Post =>
       (if Name'Length = 0 or else Name'Length > Max_Name_Length
        then Person_Result.Is_Error (Create'Result)
        else
          (Person_Result.Is_Ok (Create'Result)
           and then Name_Strings.Length
                      (Person_Result.Value (Create'Result).Name_Value)
                    > 0));

   --  ========================================================================
   --  Accessors
   --  ========================================================================

   --  Get the string representation of the person's name
   --  Contract: Result is never empty (enforced by Create validation)
   function Get_Name (Self : Person) return String
   with
     Inline,
     Post =>
       Get_Name'Result'Length > 0
       and then Get_Name'Result'Length <= Max_Name_Length;

end Domain.Value_Object.Person;
