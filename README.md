# Hybrid DDD/Clean/Hexagonal library starter for Ada 2022

[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE) [![Ada](https://img.shields.io/badge/Ada-2022-blue.svg)](https://ada-lang.io) [![SPARK](https://img.shields.io/badge/SPARK-Proved-green.svg)](https://www.adacore.com/about-spark)

**Version:** 2.0.0  
**Date:** December 10, 2025  
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Released  

## Overview

**Hybrid_Lib_Ada** is a professional Ada 2022 library demonstrating hybrid DDD/Clean/Hexagonal architecture with functional error handling.

**Key Capabilities:**

- 4-layer hexagonal architecture (Domain, Application, Infrastructure, API)
- Functional error handling via Result monad (no exceptions)
- Three-package API pattern for flexible dependency injection
- Generic I/O plugin pattern for platform portability
- Embedded-safe design (no heap allocation, bounded types)
- SPARK-compatible for formal verification
- Cross-platform: Linux, macOS, Windows, Embedded

## SPARK Formal Verification

<table>
<tr>
<td width="120"><strong>Status</strong></td>
<td><img src="https://img.shields.io/badge/SPARK-Proved-green.svg" alt="SPARK Proved"></td>
</tr>
<tr>
<td><strong>Scope</strong></td>
<td>API.Operations generic package (SPARK_Mode On)</td>
</tr>
<tr>
<td><strong>Mode</strong></td>
<td>gnatprove --mode=prove --level=2</td>
</tr>
<tr>
<td><strong>Results</strong></td>
<td>See <a href="CHANGELOG.md">CHANGELOG</a> for current proof statistics</td>
</tr>
</table>

### Verification Commands

```bash
make spark-check    # Run SPARK legality verification
make spark-prove    # Run full SPARK proof verification
```

## Features

- ✅ 4-layer hexagonal architecture
- ✅ Result monad error handling (no exceptions across boundaries)
- ✅ Static dependency injection via generics (zero runtime overhead)
- ✅ Three-package API pattern (Operations + Desktop + facade)
- ✅ Generic I/O plugin pattern for platform portability
- ✅ Embedded safety restrictions (no heap allocation)
- ✅ SPARK-compatible design (see SPARK section above)
- ✅ Comprehensive documentation with UML diagrams
- ✅ Test framework (see CHANGELOG)
- ✅ Example programs (basic_greeting, error_handling)
- ✅ Windows CI with GitHub Actions
- ✅ 6 build profiles (standard, concurrent, embedded, baremetal, STM32)

## Architecture

### 4-Layer Hexagonal Architecture

```
        Consumer Application
                ↓
        ┌───────────────────────────────────────────────┐
        │              API LAYER (Public Facade)        │
        │  ┌─────────────────┬────────────────────────┐ │
        │  │ API + Operations│    API.Desktop         │ │
        │  │ (facade)        │  (composition root)    │ │
        │  │                 │  Wires Infrastructure  │ │
        │  │ Depends on:     │  Depends on:           │ │
        │  │ App + Domain    │  ALL layers            │ │
        │  └─────────────────┴────────────────────────┘ │
        └───────────────────────────────────────────────┘
                                │
        ┌───────────────────────▼───────────────────────┐
        │              INFRASTRUCTURE LAYER             │
        │  Adapters: Console_Writer                     │
        │  Depends on: Application + Domain             │
        └───────────────────────┬───────────────────────┘
                                │
        ┌───────────────────────▼───────────────────────┐
        │               APPLICATION LAYER               │
        │  Use Cases: Greet | Commands | Ports          │
        │  Depends on: Domain only                      │
        └───────────────────────┬───────────────────────┘
                                │
        ┌───────────────────────▼───────────────────────┐
        │                 DOMAIN LAYER                  │
        │  Value Objects: Person | Error: Result monad  │
        │  Depends on: NOTHING (zero dependencies)      │
        └───────────────────────────────────────────────┘
```

**Design Principles:**

- Dependencies flow inward (toward Domain)
- Domain layer has zero external dependencies
- Infrastructure implements ports defined in Application
- **API facade depends on Application + Domain ONLY** (no Infrastructure)
- **API.Desktop** is a composition root that wires Infrastructure
- Generic I/O plugin pattern enables platform portability
- Static dispatch via generics (zero runtime overhead)

### Project Structure

```
Hybrid_Lib_Ada/
├── src/
│   ├── api/                        # Public Interface
│   │   ├── hybrid_lib_ada-api.ads  # Facade: re-exports + Greet operation
│   │   ├── operations/             # Generic operations (DI pattern)
│   │   └── desktop/                # Desktop composition root
│   │
│   ├── application/                # Use Cases & Ports
│   │   ├── command/                # Input DTOs (Greet_Command)
│   │   ├── port/                   # Port interfaces (in/out)
│   │   └── usecase/                # Use case orchestration (Greet)
│   │
│   ├── infrastructure/             # Adapters
│   │   └── adapter/                # Console_Writer implementation
│   │
│   ├── domain/                     # Pure Business Logic
│   │   ├── error/                  # Error types & Result monad
│   │   └── value_object/           # Person value object
│   │
│   └── hybrid_lib_ada.ads          # Root package
│
├── examples/                       # Example Programs
│   ├── basic_greeting.adb          # Simple greeting example
│   └── error_handling.adb          # Result monad error handling
│
├── test/                           # Test Suite
│   ├── unit/                       # 99 unit tests
│   └── integration/                # 10 integration tests
│
├── docs/                           # Documentation (submodule)
│   ├── formal/                     # SRS, SDS, STG
│   ├── guides/                     # Developer guides
│   └── diagrams/                   # UML diagrams
│
├── config/profiles/                # Build profiles
│   ├── standard/                   # Desktop/server (default)
│   ├── concurrent/                 # Multi-threaded server
│   ├── embedded/                   # Ravenscar embedded
│   ├── baremetal/                  # Zero footprint (ZFP)
│   ├── stm32h7s78/                 # STM32H7S78-DK
│   └── stm32mp135_linux/           # STM32MP135F-DK (Linux MPU)
│
├── hybrid_lib_ada.gpr              # Main library project
├── hybrid_lib_ada_internal.gpr     # Internal project (tests + examples)
├── alire.toml                      # Alire manifest
└── Makefile                        # Build automation
```

### Three-Package API Pattern

```ada
-- 1. API.Operations - Generic operations (SPARK-safe, no Infrastructure)
generic
   with function Writer (Message : String) return Unit_Result.Result;
package Hybrid_Lib_Ada.API.Operations is
   function Greet (Cmd : Greet_Command) return Unit_Result.Result;
end Hybrid_Lib_Ada.API.Operations;

-- 2. API.Desktop - Desktop composition root (wires Console_Writer)
package Hybrid_Lib_Ada.API.Desktop is
   function Greet (Cmd : Greet_Command) return Unit_Result.Result;
end Hybrid_Lib_Ada.API.Desktop;

-- 3. API - Public facade (convenience wrapper)
package Hybrid_Lib_Ada.API is
   function Greet (Cmd : Greet_Command) return Unit_Result.Result;
   -- Delegates to API.Desktop.Greet
end Hybrid_Lib_Ada.API;
```

**Benefits:**

- Library users get simple API facade by default
- Advanced users can inject custom I/O adapters via API.Operations
- Zero runtime overhead (compile-time polymorphism)
- SPARK-compatible (API.Operations has SPARK_Mode On)

## Platform Support

| Platform | Status | Notes |
|----------|--------|-------|
| **Linux** | ✅ Full | Fully supported |
| **macOS** | ✅ Full | Primary development platform |
| **BSD** | ✅ Full | Fully supported |
| **Windows** | ✅ Full | Windows 11+ (CI tested) |
| **Embedded** | 🔧 Stub | Custom adapter required |

## Quick Start

### Using Alire (Recommended)

```bash
# Add to your project
alr with hybrid_lib_ada

# Or get standalone
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

## Usage

### Basic Greeting

```ada
with Ada.Text_IO;
with Hybrid_Lib_Ada.API;

procedure My_First_Greeting is
   use Ada.Text_IO;
   use Hybrid_Lib_Ada.API;

   Cmd    : constant Greet_Command := Create_Greet_Command ("Alice");
   Result : constant Unit_Result.Result := Greet (Cmd);
begin
   if Unit_Result.Is_Ok (Result) then
      Put_Line ("Greeting succeeded!");
   else
      declare
         Err : constant Error_Type := Unit_Result.Error_Info (Result);
      begin
         Put_Line ("Error: " & Error_Strings.To_String (Err.Message));
      end;
   end if;
end My_First_Greeting;
```

**Output:**

```
Hello, Alice!
Greeting succeeded!
```

### Error Handling with Result Monad

```ada
with Hybrid_Lib_Ada.API;

procedure Error_Example is
   use Hybrid_Lib_Ada.API;

   -- Empty name triggers validation error
   Result : constant Unit_Result.Result :=
     Greet (Create_Greet_Command (""));
begin
   if Unit_Result.Is_Error (Result) then
      declare
         Err : constant Error_Type := Unit_Result.Error_Info (Result);
      begin
         -- Err.Kind = Validation_Error
         -- Err.Message = "Name cannot be empty"
      end;
   end if;
end Error_Example;
```

### Custom I/O Adapter (Advanced)

```ada
with Hybrid_Lib_Ada.API.Operations;
with My_Custom_Writer;

procedure Custom_Output is
   package My_Ops is new Hybrid_Lib_Ada.API.Operations
     (Writer => My_Custom_Writer.Write);

   use Hybrid_Lib_Ada.API;
   Cmd    : constant Greet_Command := Create_Greet_Command ("Bob");
   Result : Unit_Result.Result;
begin
   Result := My_Ops.Greet (Cmd);
end Custom_Output;
```

## Testing

| Test Type | Count | Location | Purpose |
|-----------|-------|----------|---------|
| Unit | 99 | `test/unit/` | Domain & Application logic |
| Integration | 10 | `test/integration/` | Cross-layer interactions |
| **Total** | **109** | | **100% passing** |

```bash
# Run all tests
make test-all

# Run specific test level
make test-unit
make test-integration

# Code quality
make check-arch          # Validate architecture boundaries
```

## Examples

```bash
# Build examples
alr exec -- gprbuild -P examples/examples.gpr

# Run basic greeting
./examples/bin/basic_greeting

# Run error handling demonstration
./examples/bin/error_handling
```

## Build Profiles

| Profile | Target Platform | RAM | String Limits | Contracts | Debug |
|---------|-----------------|-----|---------------|-----------|-------|
| `standard` | Desktop/Server | 1+ GB | 128/256/512 | Yes | Yes |
| `concurrent` | Multi-threaded Server | 1+ GB | 128/256/512 | Yes | Yes |
| `stm32mp135_linux` | STM32MP135F-DK (Linux MPU) | 512 MB | 128/256/512 | Yes | Yes |
| `embedded` | Ravenscar Embedded | 512KB-1MB | 64/128/256 | Yes | No |
| `stm32h7s78` | STM32H7S78-DK | 620KB+32MB | 64/128/256 | Yes | Yes |
| `baremetal` | Zero Footprint (ZFP) | 128KB-256KB | 32/64/128 | No | No |

```bash
# Build with specific profile
alr build -- -XHYBRID_LIB_PROFILE=embedded
```

## Documentation

- 📚 **[Documentation Index](docs/index.md)** - Complete documentation overview
- 🚀 **[Quick Start Guide](docs/quick_start.md)** - Get started in minutes
- 📖 **[Software Requirements Specification](docs/formal/software_requirements_specification.md)**
- 🏗️ **[Software Design Specification](docs/formal/software_design_specification.md)**
- 🧪 **[Software Test Guide](docs/formal/software_test_guide.md)**
- 📝 **[CHANGELOG](CHANGELOG.md)** - Release history

### Diagrams

- `docs/diagrams/library_architecture.svg` - 4-layer architecture overview
- `docs/diagrams/ada/api_reexport_pattern_ada.svg` - Three-package API pattern
- `docs/diagrams/ada/package_structure_ada.svg` - Package structure
- `docs/diagrams/ada/error_handling_flow_ada.svg` - Error propagation
- `docs/diagrams/ada/static_dispatch_ada.svg` - Static DI with generics
- `docs/diagrams/ada/three_package_api_ada.svg` - API composition pattern

## Dependencies

| Dependency | Version | Purpose |
|------------|---------|---------|
| functional | ^3.0.0 | Result monad and Option types |

## Submodule Management

This project uses git submodules for shared tooling:

- `docs` - Shared documentation templates and guides
- `scripts/python` - Build, release, and architecture scripts
- `test/python` - Shared test fixtures and configuration

### Commands

```bash
# After fresh clone
make submodule-init

# Pull latest from submodule repos
make submodule-update

# Check current submodule commits
make submodule-status
```

## Contributing

This project is not open to external contributions at this time.

## AI Assistance & Authorship

This project — including its source code, tests, documentation, and other deliverables — is designed, implemented, and maintained by human developers, with Michael Gardner as the Principal Software Engineer and project lead.

We use AI coding assistants (such as OpenAI GPT models and Anthropic Claude Code) as part of the development workflow to help with:

- drafting and refactoring code and tests,
- exploring design and implementation alternatives,
- generating or refining documentation and examples,
- and performing tedious and error-prone chores.

AI systems are treated as tools, not authors. All changes are reviewed, adapted, and integrated by the human maintainers, who remain fully responsible for the architecture, correctness, and licensing of this project.

## License

Copyright © 2025 Michael Gardner, A Bit of Help, Inc.

Licensed under the BSD-3-Clause License. See [LICENSE](LICENSE) for details.

## Author

Michael Gardner
A Bit of Help, Inc.
https://github.com/abitofhelp
