# Hybrid_Lib_Ada - Greeter Library with Hexagonal Architecture

[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE) [![Ada](https://img.shields.io/badge/Ada-2022-blue.svg)](https://ada-lang.io) [![SPARK](https://img.shields.io/badge/SPARK-Proved-green.svg)](https://www.adacore.com/about-spark)

**Version:** 2.0.0<br>
**Date:** December 09, 2025<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

> **Note**: This library is currently a release candidate awaiting publication of the `functional` crate to Alire.

## Overview

A professional Ada 2022 library starter demonstrating a **hybrid DDD/Clean/Hexagonal architecture** with **functional programming** principles using the `functional` crate for Result monads.

This is a **library template** showcasing:
- **4-Layer Hexagonal Architecture** (Domain, Application, Infrastructure, API)
- **Static Dispatch Dependency Injection** via generics (zero runtime overhead)
- **Railway-Oriented Programming** with Result monads (no exceptions across boundaries)
- **Three-Package API Pattern** (Operations generic, platform-specific composition roots, facade)
- **Embedded Safety** (No_Implicit_Heap_Allocations, bounded types, static allocation)
- **Single-Project Structure** (easy Alire deployment)

## Features

- ✅ Single-project structure (easy Alire deployment)
- ✅ Result monad error handling (Domain.Error.Result)
- ✅ Static dependency injection via generics
- ✅ Three-package API pattern (Operations + Desktop + facade)
- ✅ Generic I/O plugin pattern for platform portability
- ✅ Embedded safety restrictions (no heap allocation)
- ✅ SPARK-compatible design for formal verification
- ✅ Comprehensive documentation with UML diagrams
- ✅ Test framework (99 unit + 10 integration = 109 tests)
- ✅ Example programs (basic_greeting, error_handling)
- ✅ Windows CI with GitHub Actions
- ✅ Aspect syntax (not pragmas)
- ✅ Makefile automation

## Architecture

### Layer Structure

```
Hybrid_Lib_Ada/
├── src/
│   ├── api/                        # Public Interface (ZERO dependencies)
│   │   ├── hybrid_lib_ada-api.ads  # Facade: re-exports + Greet operation
│   │   ├── operations/             # Generic operations (DI pattern)
│   │   └── desktop/                # Desktop composition root
│   │
│   ├── application/                # Use Cases & Ports (Depends on: Domain)
│   │   ├── command/                # Input DTOs (Greet_Command)
│   │   ├── error/                  # Re-exports Domain.Error
│   │   ├── port/                   # Port interfaces (in/out)
│   │   └── usecase/                # Use case orchestration (Greet)
│   │
│   ├── infrastructure/             # Adapters (Depends on: Application + Domain)
│   │   └── adapter/                # Console_Writer implementation
│   │
│   ├── domain/                     # Pure Business Logic (ZERO dependencies)
│   │   ├── error/                  # Error types & Result monad
│   │   └── value_object/           # Person value object
│   │
│   └── hybrid_lib_ada.ads          # Root package with embedded restrictions
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
├── hybrid_lib_ada.gpr              # Main library project
├── hybrid_lib_ada_internal.gpr     # Internal project (tests + examples)
├── alire.toml                      # Alire manifest
└── Makefile                        # Build automation
```

### 4-Layer Architecture

**Dependency Rule: All dependencies point INWARD**

```
+-----------------------------------------------------------------+
|                          API Layer                               |
|  Hybrid_Lib_Ada.API (facade) + API.Desktop + API.Operations     |
+----------------------------------+------------------------------+
                                   |
+----------------------------------v------------------------------+
|                      Application Layer                           |
|  Use Cases (Greet) + Inbound/Outbound Ports + Commands          |
+----------------------------------+------------------------------+
                                   |
+----------------------------------v------------------------------+
|                    Infrastructure Layer                          |
|  Adapters (Console_Writer) + Platform Implementations           |
+----------------------------------+------------------------------+
                                   |
+----------------------------------v------------------------------+
|                       Domain Layer                               |
|  Entities + Value Objects (Person) + Result Monad + Errors      |
+-----------------------------------------------------------------+
```

**Design Principles:**

- Dependencies flow inward (toward Domain)
- Domain layer has zero external dependencies
- Infrastructure implements ports defined in Application
- API provides stable public interface via facade pattern
- Generic I/O plugin pattern enables platform portability

### Three-Package API Pattern

```ada
-- 1. API.Operations - Generic operations (define the interface)
generic
   with function Writer (Message : String) return Unit_Result.Result;
package Hybrid_Lib_Ada.API.Operations is
   function Greet (Cmd : Greet_Command) return Unit_Result.Result;
end Hybrid_Lib_Ada.API.Operations;

-- 2. API.Desktop - Desktop composition root (wire dependencies)
package Hybrid_Lib_Ada.API.Desktop is
   function Greet (Cmd : Greet_Command) return Unit_Result.Result;
   -- Internally instantiates API.Operations with Console_Writer
end Hybrid_Lib_Ada.API.Desktop;

-- 3. API - Public facade (convenience wrapper)
package Hybrid_Lib_Ada.API is
   -- Re-exports domain/application types
   function Greet (Cmd : Greet_Command) return Unit_Result.Result;
   -- Delegates to API.Desktop.Greet
end Hybrid_Lib_Ada.API;
```

**Benefits:**
- Library users get simple API facade by default
- Advanced users can inject custom I/O adapters via API.Operations
- Zero runtime overhead (compile-time polymorphism)

## Platform Support

| Platform | Status | Notes |
|----------|--------|-------|
| **Linux** | ✅ Full | Primary development platform, console I/O |
| **macOS** | ✅ Full | CI tested, console I/O |
| **Windows** | ✅ Full | CI tested (v2.0.0+), console I/O |
| **Embedded** | 🔧 Stub | Architecture supports it, custom adapter required |

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
- **GNAT** 14+ (via Alire toolchain)
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
with My_Custom_Writer; -- Your I/O adapter

procedure Custom_Output is
   package My_Ops is new Hybrid_Lib_Ada.API.Operations
     (Writer => My_Custom_Writer.Write);

   use Hybrid_Lib_Ada.API;
   Cmd : constant Greet_Command := Create_Greet_Command ("Bob");
begin
   -- Uses your custom writer instead of console
   Result := My_Ops.Greet (Cmd);
end Custom_Output;
```

## Testing

Tests use a custom lightweight test framework (no AUnit dependency):

| Test Type     | Count | Location              | Purpose                              |
|---------------|-------|-----------------------|--------------------------------------|
| Unit          | 99    | `test/unit/`          | Domain & Application logic           |
| Integration   | 10    | `test/integration/`   | Cross-layer interactions             |
| **Total**     | **109**|                      | **100% passing**                     |

```bash
# Run all tests
make test-all

# Run specific test level
make test-unit
make test-integration

# Code quality
make check-arch          # Validate architecture boundaries
make diagrams            # Regenerate UML diagrams
make stats               # Code statistics
```

**Expected Output:**

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
```

## Examples

The library includes runnable examples:

```bash
# Build examples
alr build

# Run basic greeting
./bin/basic_greeting

# Run error handling demonstration
./bin/error_handling
```

See `examples/` directory for source code.

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
- `docs/diagrams/ada/package_structure_ada.svg` - Actual packages
- `docs/diagrams/ada/error_handling_flow_ada.svg` - Error propagation
- `docs/diagrams/ada/static_dispatch_ada.svg` - Static DI with generics
- `docs/diagrams/ada/three_package_api_ada.svg` - API composition pattern

## Code Standards

This project follows:
- **Ada Agent** (`~/.claude/agents/ada.md`)
- **Architecture Agent** (`~/.claude/agents/architecture.md`)
- **Functional Agent** (`~/.claude/agents/functional.md`)
- **Testing Agent** (`~/.claude/agents/testing.md`)

### Key Standards Applied

1. **Aspects over Pragmas:** `with Pure` not `pragma Pure`
2. **Contracts:** Pre/Post conditions on all public operations
3. **No Heap:** Domain uses bounded strings, embedded restrictions enforced
4. **Immutability:** Value objects immutable after creation
5. **Pure Functions:** Domain logic has no side effects
6. **Result Monads:** No exceptions across boundaries
7. **Static Dispatch:** Generics for dependency injection

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

## Project Status

**Status**: Release Candidate (v2.0.0)

- ✅ Single-project structure (easy Alire deployment)
- ✅ Result monad error handling (Domain.Error.Result)
- ✅ Static dependency injection via generics
- ✅ Three-package API pattern (Operations + Desktop + facade)
- ✅ Generic I/O plugin pattern
- ✅ Embedded safety restrictions
- ✅ SPARK-compatible design
- ✅ Comprehensive documentation with UML diagrams
- ✅ Test framework (99 unit + 10 integration = 109 tests)
- ✅ Example programs (basic_greeting, error_handling)
- ✅ Windows CI with GitHub Actions
- ⏳ Awaiting functional crate publication to Alire
