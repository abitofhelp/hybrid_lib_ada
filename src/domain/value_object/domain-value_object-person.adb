pragma Ada_2022;
--  =========================================================================
--  Domain.Value_Object.Person - Implementation of Person value object
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Implements Person value object factory function Create with validation
--    and Is_Valid_Person predicate for type invariant checking.
--  =========================================================================

package body Domain.Value_Object.Person is

   use Person_Strings;
   use Domain.Error;

   ----------------------
   -- Is_Valid_Person --
   ----------------------

   function Is_Valid_Person (P : Person) return Boolean is
   begin
      --  Person is valid if name is not empty
      --  This is the TYPE INVARIANT - must always hold for any Person instance
      return Length (P.Name_Value) > 0;
   end Is_Valid_Person;

   ------------
   -- Create --
   ------------

   function Create (Name : String) return Person_Result.Result is
   begin
      --  Validation 1: Check for empty string
      if Name'Length = 0 then
         return
           Person_Result.Error
             (Kind    => Validation_Error,
              Message => "Person name cannot be empty");
      end if;

      --  Validation 2: Check maximum length
      if Name'Length > Max_Name_Length then
         return
           Person_Result.Error
             (Kind    => Validation_Error,
              Message =>
                "Person name exceeds maximum length of"
                & Max_Name_Length'Image
                & " characters");
      end if;

      --  All validations passed - create the value object
      return Person_Result.Ok ((Name_Value => To_Bounded_String (Name)));
   end Create;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Self : Person) return String is
   begin
      return To_String (Self.Name_Value);
   end Get_Name;

end Domain.Value_Object.Person;
