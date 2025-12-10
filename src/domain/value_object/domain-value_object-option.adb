pragma Ada_2022;
--  =========================================================================
--  Domain.Value_Object.Option - Implementation of Option monad
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Implements Generic_Option constructors (Of_Value, None), accessors
--    (Get, Get_Or_Else), and railway combinators (Map, Flat_Map, Filter).
--  =========================================================================

package body Domain.Value_Object.Option is

   package body Generic_Option is

      --------------
      -- Of_Value --
      --------------

      function Of_Value (Value : T) return Option is
      begin
         return (Kind => Some_Kind, Value_Data => Value);
      end Of_Value;

      ----------
      -- None --
      ----------

      function None return Option is
      begin
         return (Kind => None_Kind);
      end None;

      -------------
      -- Is_Some --
      -------------

      function Is_Some (Self : Option) return Boolean is
      begin
         return Self.Kind = Some_Kind;
      end Is_Some;

      -------------
      -- Is_None --
      -------------

      function Is_None (Self : Option) return Boolean is
      begin
         return Self.Kind = None_Kind;
      end Is_None;

      -----------
      -- Value --
      -----------

      function Value (Self : Option) return T is
      begin
         return Self.Value_Data;
      end Value;

      ---------------
      -- Unwrap_Or --
      ---------------

      function Unwrap_Or (Self : Option; Default : T) return T is
      begin
         if Self.Kind = Some_Kind then
            return Self.Value_Data;
         else
            return Default;
         end if;
      end Unwrap_Or;

      --------------------
      -- Unwrap_Or_With --
      --------------------

      function Unwrap_Or_With (Self : Option) return T is
      begin
         if Self.Kind = Some_Kind then
            return Self.Value_Data;
         else
            return F;
         end if;
      end Unwrap_Or_With;

      ---------
      -- Map --
      ---------

      function Map (Self : Option) return Option is
      begin
         if Self.Kind = Some_Kind then
            return Of_Value (F (Self.Value_Data));
         else
            return Self;  -- Propagate None
         end if;
      end Map;

      --------------
      -- And_Then --
      --------------

      function And_Then (Self : Option) return Option is
      begin
         if Self.Kind = Some_Kind then
            return F (Self.Value_Data);
         else
            return Self;  -- Propagate None
         end if;
      end And_Then;

      ------------
      -- Filter --
      ------------

      function Filter (Self : Option) return Option is
      begin
         if Self.Kind = Some_Kind then
            if Pred (Self.Value_Data) then
               return Self;  -- Keep value
            else
               return None;  -- Predicate failed
            end if;
         else
            return Self;  -- Already None
         end if;
      end Filter;

      -------------
      -- Or_Else --
      -------------

      function Or_Else
        (Primary : Option; Alternative : Option) return Option
      is
      begin
         if Primary.Kind = Some_Kind then
            return Primary;
         else
            return Alternative;
         end if;
      end Or_Else;

      ------------------
      -- Or_Else_With --
      ------------------

      function Or_Else_With (Self : Option) return Option is
      begin
         if Self.Kind = Some_Kind then
            return Self;
         else
            return F;
         end if;
      end Or_Else_With;

   end Generic_Option;

end Domain.Value_Object.Option;
