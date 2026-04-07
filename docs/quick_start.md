# Hybrid_Lib_Ada Quick Start Guide

**Version:** 2.0.0  
**Date:** 2025-12-10  
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>  
**Status:** Released  

---

## Table of Contents

- [Installation](#installation)
- [First Program](#first-program)
- [Working with Person Values](#working-with-person-values)
- [Error Handling](#error-handling)
- [Running Example Programs](#running-example-programs)
- [Running Tests](#running-tests)
- [Build Profiles](#build-profiles)
- [Common Issues](#common-issues)
- [Next Steps](#next-steps)

---

## Installation

### Using Alire (Recommended)

```bash
# Add hybrid_lib_ada to your project
alr with hybrid_lib_ada

# Or get hybrid_lib_ada standalone
alr get hybrid_lib_ada
cd hybrid_lib_ada_*
alr build
```

### Manual Installation

```bash
git clone --recurse-submodules https://github.com/abitofhelp/hybrid_lib_ada.git
cd hybrid_lib_ada
alr build
```

### Prerequisites

- **Alire** 2.0+ (Ada package manager)
- **GNAT** 13+ (via Alire toolchain)
- **Make** (for convenience targets)

#### Installing Alire

```bash
# macOS (Homebrew)
brew install alire

# Linux - download from https://alire.ada.dev/docs/#installation
# Visit: https://github.com/alire-project/alire/releases

# Verify installation
alr --version
```

---

## First Program

Create a simple program to generate a greeting:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Hybrid_Lib_Ada.API;

procedure My_First_Greeting is
   use Hybrid_Lib_Ada.API;

   --  Create a greet command
   Cmd : constant Greet_Command := Create_Greet_Command ("Alice");

   --  Execute the greet operation
   Result : constant Unit_Result.Result := Greet (Cmd);
begin
   if Unit_Result.Is_Ok (Result) then
      Put_Line ("Greeting succeeded!");
   else
      Put_Line ("Greeting failed");
   end if;
end My_First_Greeting;
```

**Build and Run:**

```bash
alr build
./bin/my_first_greeting
```

**Expected Output:**

```
Hello, Alice!
Greeting succeeded!
```

---

## Working with Person Values

Create and validate Person value objects:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Hybrid_Lib_Ada.API;

procedure Person_Example is
   use Hybrid_Lib_Ada.API;

   --  Create a Person (with validation)
   Result : constant Person_Result.Result := Create_Person ("Bob");
begin
   if Person_Result.Is_Ok (Result) then
      declare
         P : constant Person_Type := Person_Result.Value (Result);
      begin
         Put_Line ("Created person: " & Get_Name (P));
      end;
   else
      --  Validation failed (empty name, too long, etc.)
      declare
         Info : constant Error_Type := Person_Result.Error_Info (Result);
      begin
         Put_Line ("Validation error: " &
           Error_Strings.To_String (Info.Message));
      end;
   end if;
end Person_Example;
```

---

## Error Handling

Hybrid_Lib_Ada uses the Result monad pattern - no exceptions are raised.

### Pattern 1: Check Success/Failure

```ada
Result : constant Unit_Result.Result := Greet (Cmd);

if Unit_Result.Is_Ok (Result) then
   --  Success path
   Put_Line ("Operation succeeded");
else
   --  Error path
   Put_Line ("Operation failed");
end if;
```

### Pattern 2: Extract Error Info

```ada
Result : constant Person_Result.Result := Create_Person ("");

if Person_Result.Is_Error (Result) then
   declare
      Info : constant Error_Type := Person_Result.Error_Info (Result);
   begin
      Put_Line ("Error kind: " & Info.Kind'Image);
      Put_Line ("Message: " & Error_Strings.To_String (Info.Message));
   end;
end if;
```

### Error Kinds

| Kind | Description |
|------|-------------|
| `Validation_Error` | Input validation failed (empty name, too long) |
| `Parse_Error` | Malformed data or parsing failure |
| `Not_Found_Error` | Requested resource not found |
| `IO_Error` | Infrastructure I/O operation failed |
| `Internal_Error` | Unexpected internal error (bug) |

**Why No Exceptions?**

- Explicit error paths enforced by compiler
- SPARK compatible for formal verification
- Deterministic timing (no stack unwinding)
- Errors are values that can be passed and transformed

---

## Running Example Programs

Hybrid_Lib_Ada includes runnable example programs in the `examples/` directory:

### Building Examples

```bash
# Build examples
alr exec -- gprbuild -P examples/examples.gpr

# Executables output to examples/bin/
ls examples/bin/
```

### Basic Greeting Example

Demonstrates simple library usage with a valid name:

```bash
./examples/bin/basic_greeting
```

**Expected Output:**

```
=== Basic Greeting Example ===

Hello, World!
Greeting executed successfully!

=== End Example ===
```

### Error Handling Example

Demonstrates Result monad error handling with validation errors:

```bash
./examples/bin/error_handling
```

**Expected Output:**

```
=== Error Handling Example ===

Test 1: Valid name 'Alice'
Hello, Alice!
  Result: OK - Greeting printed

Test 2: Empty name ''
  Result: ERROR - VALIDATION_ERROR: Name cannot be empty

Test 3: Name too long (150 chars)
  Result: ERROR - VALIDATION_ERROR: Name exceeds maximum length

=== End Example ===
```

---

## Running Tests

### All Tests

```bash
make test-all
```

### Specific Test Suites

```bash
# Unit tests only
make test-unit

# Integration tests only
make test-integration
```

### Expected Output

```
========================================
     HYBRID_LIB_ADA UNIT TEST SUITE
========================================

[PASS] Ok construction - Is_Ok returns true
[PASS] Create valid name - Is_Ok
...

########################################
###    UNIT TESTS: SUCCESS            ###
###    All 99 tests passed!           ###
########################################

========================================
  HYBRID_LIB_ADA INTEGRATION TEST SUITE
========================================

[PASS] Greet use case with console writer - success
...

########################################
###  INTEGRATION TESTS: SUCCESS      ###
###    All 10 tests passed!          ###
########################################

########################################
###   ALL TEST SUITES: SUCCESS      ###
###   All tests passed!              ###
########################################
```

---

## Build Profiles

The library supports 6 build profiles for different target environments:

| Profile | Target Platform | RAM | String Limits | Contracts | Debug |
|---------|-----------------|-----|---------------|-----------|-------|
| `standard` | Desktop/Server | 1+ GB | 128/256/512 | Yes | Yes |
| `concurrent` | Multi-threaded Server | 1+ GB | 128/256/512 | Yes | Yes |
| `stm32mp135_linux` | STM32MP135F-DK (Linux MPU) | 512 MB | 128/256/512 | Yes | Yes |
| `embedded` | Ravenscar Embedded | 512KB-1MB | 64/128/256 | Yes | No |
| `stm32h7s78` | STM32H7S78-DK | 620KB+32MB | 64/128/256 | Yes | Yes |
| `baremetal` | Zero Footprint (ZFP) | 128KB-256KB | 32/64/128 | No | No |

**String Limits** are Max_Name_Length / Max_Message_Length / Max_Error_Length.

### Selecting a Profile

```bash
# Standard profile (default)
alr build

# Embedded profile
alr build -- -XHYBRID_LIB_PROFILE=embedded

# STM32H7S78-DK profile
alr build -- -XHYBRID_LIB_PROFILE=stm32h7s78
```

See [Build Profiles](guides/build_profiles.md) for detailed configuration.

---

## Common Issues

### Q: Build fails with "functional.gpr not found"

**A:** The library depends on the `functional` crate. Ensure it's available:

```bash
alr update
```

### Q: Tests not found

**A:** Build tests first:

```bash
make build-tests
```

### Q: Style warnings about line length

**A:** The library uses strict style checking. These warnings are informational:

```
(style) this line is too long: 85 [-gnatyM]
```

### Q: Examples not building

**A:** Build examples with the dedicated GPR:

```bash
alr exec -- gprbuild -P examples/examples.gpr
```

---

## Next Steps

- **[Documentation Index](index.md)** - Complete documentation overview
- **[All About Our API](guides/all_about_our_api.md)** - Three-package API pattern
- **[Error Handling Strategy](guides/error_handling_strategy.md)** - Deep dive into Result monad
- **[Architecture Enforcement](guides/architecture_enforcement.md)** - Layer dependency rules
- **[Software Test Guide](formal/software_test_guide.md)** - Comprehensive testing guide

---

**License:** BSD-3-Clause<br>  
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
