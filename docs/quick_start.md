# Quick Start Guide

**Version:** 1.0.0<br>
**Date:** November 29, 2025<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## Prerequisites

- **Alire** 2.0+ (Ada package manager)
- **GNAT** 14+ (via Alire toolchain)
- **Make** (for convenience targets)

### Installing Alire

```bash
# macOS (Homebrew)
brew install alire

# Linux (download latest from https://alire.ada.dev/docs/#installation)
# Visit releases page: https://github.com/alire-project/alire/releases
# Download the appropriate binary for your platform and add to PATH

# Verify installation
alr --version
```

---

## Building the Library

### Clone and Build

```bash
# Clone the repository
git clone https://github.com/abitofhelp/hybrid_lib_ada.git
cd hybrid_lib_ada

# Build with Alire
alr build

# Or with Make
make build
```

### Build Output

```
lib/libhybrid_lib_ada.a    # Static library
```

---

## Using the Library

### Add as Dependency

In your project's `alire.toml`:

```toml
[[depends-on]]
hybrid_lib_ada = "*"
```

Then:

```bash
alr update
alr build
```

### Basic Usage

```ada
with Hybrid_Lib_Ada.API;

procedure Greet_Example is
   use Hybrid_Lib_Ada.API;

   --  Create a greet command
   Cmd : constant Greet_Command := Create_Greet_Command ("Alice");

   --  Execute the greet operation
   Result : constant Unit_Result.Result := Greet (Cmd);
begin
   if Unit_Result.Is_Ok (Result) then
      --  Success! "Hello, Alice!" was written to console
      null;
   else
      --  Handle error
      declare
         Info : constant Error_Type := Unit_Result.Error_Info (Result);
      begin
         Ada.Text_IO.Put_Line ("Error: " & Error_Strings.To_String (Info.Message));
      end;
   end if;
end Greet_Example;
```

### Working with Person Values

```ada
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
         Ada.Text_IO.Put_Line ("Created person: " & Get_Name (P));
      end;
   else
      --  Validation failed (empty name, too long, etc.)
      declare
         Info : constant Error_Type := Person_Result.Error_Info (Result);
      begin
         Ada.Text_IO.Put_Line ("Validation error: " &
           Error_Strings.To_String (Info.Message));
      end;
   end if;
end Person_Example;
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
./test/bin/unit_runner

# Integration tests only
./test/bin/integration_runner
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
###    All 88 tests passed!           ###
########################################
```

---

## Build Profiles

The library supports multiple build profiles for different target environments:

| Profile | Use Case | RAM | Features |
|---------|----------|-----|----------|
| `standard` | Desktop/server | 1+ GB | Full features |
| `embedded` | Ravenscar embedded | 512KB+ | Tasking safe |
| `baremetal` | Zero footprint | 128KB+ | Minimal runtime |

### Selecting a Profile

```bash
alr build -- -XHYBRID_LIB_ADA_PROFILE=embedded
```

See [Build Profiles](guides/build_profiles.md) for detailed configuration.

---

## Error Handling

The library uses functional error handling via Result monad:

```ada
--  All operations return Result[T] instead of raising exceptions
--  Result is either Ok(value) or Error(error_info)

if Unit_Result.Is_Ok (Result) then
   --  Success path: extract value
   Value := Unit_Result.Value (Result);
else
   --  Error path: extract error info
   Info := Unit_Result.Error_Info (Result);
   --  Info.Kind: Validation_Error, IO_Error, etc.
   --  Info.Message: Human-readable error description
end if;
```

### Error Kinds

| Kind | Description |
|------|-------------|
| `Validation_Error` | Input validation failed (empty name, too long) |
| `IO_Error` | Infrastructure I/O operation failed |
| `Not_Found_Error` | Requested resource not found |
| `Already_Exists_Error` | Resource already exists |
| `Config_Error` | Configuration/setup error |
| `Internal_Error` | Unexpected internal error |

---

## Common Issues

### Build Fails with "functional.gpr not found"

The library depends on the `functional` crate. Ensure it's available:

```bash
alr update
```

### Tests Not Found

Build tests first:

```bash
alr exec -- gprbuild -P test/unit/unit_tests.gpr
alr exec -- gprbuild -P test/integration/integration_tests.gpr
```

### Style Warnings

The library uses strict style checking. Warnings about line length are informational:

```
(style) this line is too long: 85 [-gnatyM]
```

---

## Next Steps

- Read [All About Our API](guides/all_about_our_api.md) for API architecture
- Review [Software Design Specification](formal/software_design_specification.md) for internals
