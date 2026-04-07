# Build Profiles

**Version:** 2.0.0  
**Date:** 2025-12-10  
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>  
**Status:** Released  

This document explains how to build the library for different target platforms.

## Overview

Hybrid Lib Ada supports multiple build profiles for different target platforms. Each profile configures:

- **Bounded string sizes** (Max_Name_Length, Max_Message_Length, Max_Error_Length)
- **Runtime settings** (Enable_Contracts, Enable_Debug)
- **Build mode** (development, validation, release)

## Available Profiles

| Profile | Target | RAM | String Limits | Contracts | Debug |
|---------|--------|-----|---------------|-----------|-------|
| `standard` | Desktop/Server | 1+ GB | 128/256/512 | Yes | Yes |
| `concurrent` | Multi-threaded Server | 1+ GB | 128/256/512 | Yes | Yes |
| `stm32mp135_linux` | STM32MP135F-DK (Linux MPU) | 512 MB | 128/256/512 | Yes | Yes |
| `embedded` | Ravenscar Embedded | 512KB-1MB | 64/128/256 | Yes | No |
| `stm32h7s78` | STM32H7S78-DK | 620KB+32MB | 64/128/256 | Yes | Yes |
| `baremetal` | Zero Footprint (ZFP) | 128KB-256KB | 32/64/128 | No | No |

## Building with Profiles

### Using Alire (Recommended)

```bash
# Standard profile (default)
alr build

# Embedded profile
alr build -- -XHYBRID_LIB_PROFILE=embedded

# Baremetal profile
alr build -- -XHYBRID_LIB_PROFILE=baremetal

# Concurrent profile
alr build -- -XHYBRID_LIB_PROFILE=concurrent

# STM32H7S78-DK profile
alr build -- -XHYBRID_LIB_PROFILE=stm32h7s78

# STM32MP135F-DK Linux profile
alr build -- -XHYBRID_LIB_PROFILE=stm32mp135_linux
```

### Using gprbuild Directly

```bash
gprbuild -P hybrid_lib_ada.gpr -XHYBRID_LIB_PROFILE=embedded
```

### Using Make

```bash
# Test all profiles compile
make build-profiles
```

## How It Works

1. **GPR Variable**: The `HYBRID_LIB_PROFILE` external variable selects the profile
2. **Source_Dirs Switch**: The GPR uses a `case` statement to include the profile-specific config:

```ada
case Profile is
   when "standard" =>
      for Source_Dirs use ("src/**", "config/profiles/standard");
   when "embedded" =>
      for Source_Dirs use ("src/**", "config/profiles/embedded");
   -- ... etc
end case;
```

3. **Config Package**: Each profile has its own `Hybrid_Lib_Ada_Config` package in `config/profiles/<profile>/hybrid_lib_ada_config.ads`

## Profile Configuration

Each profile config provides these constants:

```ada
package Hybrid_Lib_Ada_Config is
   pragma Pure;

   Profile_Name    : constant String := "...";
   Target_Platform : constant String := "...";

   type Build_Profile_Kind is (release, validation, development);
   Build_Profile : constant Build_Profile_Kind := development;

   --  Bounded string configuration
   Max_Name_Length    : constant := ...;
   Max_Message_Length : constant := ...;
   Max_Error_Length   : constant := ...;

   --  Runtime configuration
   Enable_Contracts : constant Boolean := ...;
   Enable_Debug     : constant Boolean := ...;
end Hybrid_Lib_Ada_Config;
```

## Adding a New Profile

1. Create directory: `config/profiles/<profile_name>/`
2. Create config file: `<project>_config.ads` with appropriate values
3. Add the profile to the `Profile_Type` in `<project>.gpr`
4. Add the `case` branch for Source_Dirs
5. Update the PROFILES list in `Makefile` build-profiles target

## Restrictions Files

Some profiles include `restrictions.adc` files with pragma restrictions for safety:

- `config/profiles/embedded/restrictions.adc`
- `config/profiles/baremetal/restrictions.adc`
- `config/profiles/stm32h7s78/restrictions.adc`
- `config/profiles/stm32mp135_linux/restrictions.adc`

To use restrictions, add to your project:
```ada
package Compiler is
   for Local_Configuration_Pragmas use "path/to/restrictions.adc";
end Compiler;
```

Or build with:
```bash
alr exec -- gprbuild -gnatec=path/to/restrictions.adc
```

## Linking with Tasking Dependencies

If your library depends on a crate that uses **protected objects** or other Ada tasking features (e.g., `gnatcoll`, `tzif`), executables linking against your library must include the tasking runtime.

### Symptom

Undefined linker symbols like:
```
___gnat_tasking_runtime_initialize
_system__tasking__protected_objects_E
_system__tasking__protected_objects__entries_E
```

### Solution

Add `-lgnarl` to the linker switches in your executable's GPR file:

```ada
package Linker is
   --  Link against tasking runtime (required by protected objects)
   for Default_Switches ("Ada") use ("-lgnarl");
end Linker;
```

### When This Applies

- Your library depends on `gnatcoll` (uses protected types internally)
- Your library depends on `tzif` (uses protected types for thread-safe caching)
- Your library uses `protected` types or tasks directly

### Note

This is only needed for **executables** (examples, tests, apps). The library itself builds fine - the issue appears at link time when creating the final executable.

## See Also

- [Alire documentation](https://alire.ada.dev/docs/)
- `make help` for all build commands
