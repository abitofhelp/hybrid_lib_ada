# Software Design Specification

**Version:** 1.0.0<br>
**Date:** 2025-11-29<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## 1. Introduction

### 1.1 Purpose

This Software Design Specification (SDS) describes the internal architecture, package structure, and design decisions for **Hybrid_Lib_Ada**.

### 1.2 Scope

This document covers:
- 4-layer hexagonal architecture
- Package hierarchy and dependencies
- Type definitions and contracts
- Static dependency injection via generics
- SPARK verification boundaries

### 1.3 References

- Software Requirements Specification (SRS)
- [All About Our API](../guides/all_about_our_api.md) - Detailed API architecture guide
- Ada 2022 Reference Manual
- SPARK 2014 Reference Manual

---

## 2. Architectural Overview

### 2.1 Layer Architecture

Hybrid_Lib_Ada uses a **4-layer library architecture** (Domain, Application, Infrastructure, API):

```
┌─────────────────────────────────────────────────────────────┐
│                        API Layer                             │
│  Public facade + composition roots + SPARK operations        │
│  src/api/                                                    │
└─────────────────────────────┬───────────────────────────────┘
                              │ depends on
┌─────────────────────────────▼───────────────────────────────┐
│                   Infrastructure Layer                       │
│  Concrete adapters implementing ports                        │
│  src/infrastructure/                                         │
└─────────────────────────────┬───────────────────────────────┘
                              │ implements
┌─────────────────────────────▼───────────────────────────────┐
│                    Application Layer                         │
│  Use cases, commands, ports                                  │
│  src/application/                                            │
└─────────────────────────────┬───────────────────────────────┘
                              │ depends on
┌─────────────────────────────▼───────────────────────────────┐
│                      Domain Layer                            │
│  Pure business logic, value objects, errors                  │
│  src/domain/                                                 │
└─────────────────────────────────────────────────────────────┘
```

### 2.2 Dependency Rules

| Layer | May Depend On |
|-------|---------------|
| Domain | Nothing (zero dependencies) |
| Application | Domain only |
| Infrastructure | Application, Domain |
| API | All layers (composition root) |

### 2.3 Hexagonal Pattern

```
           ┌──────────────────────────────────────┐
           │          Application Core            │
           │                                      │
    ┌──────┤  Domain ← Application               │
    │      │                                      │
    │      └──────────────────────────────────────┘
    │                       │
    │                       │ Ports
    ▼                       ▼
┌────────┐           ┌────────────┐
│ Writer │◄──────────│ Greet Use  │
│  Port  │           │   Case     │
└────────┘           └────────────┘
    ▲
    │ Implements
┌────────────────┐
│ Console_Writer │ (Infrastructure)
└────────────────┘
```

---

## 3. Package Structure

### 3.1 Directory Layout

```
src/
├── hybrid_lib_ada.ads              # Root package
│
├── domain/
│   ├── domain.ads                  # Domain layer root
│   ├── error/
│   │   ├── domain-error.ads        # Error type definition
│   │   └── result/
│   │       └── domain-error-result.ads  # Generic Result monad
│   ├── unit/
│   │   └── domain-unit.ads         # Unit type (void equivalent)
│   └── value_object/
│       └── person/
│           └── domain-value_object-person.ads
│
├── application/
│   ├── application.ads             # Application layer root
│   ├── command/
│   │   └── greet/
│   │       └── application-command-greet.ads
│   ├── port/
│   │   └── outbound/
│   │       └── writer/
│   │           └── application-port-outbound-writer.ads
│   └── usecase/
│       └── greet/
│           └── application-usecase-greet.ads
│
├── infrastructure/
│   ├── infrastructure.ads          # Infrastructure layer root
│   └── adapter/
│       └── console_writer/
│           └── infrastructure-adapter-console_writer.ads
│
└── api/
    ├── hybrid_lib_ada-api.ads      # Public facade
    ├── hybrid_lib_ada-api.adb
    ├── operations/
    │   └── hybrid_lib_ada-api-operations.ads  # SPARK-safe
    └── desktop/
        └── hybrid_lib_ada-api-desktop.ads     # Composition root
```

### 3.2 Package Descriptions

#### 3.2.1 Domain Layer

| Package | Purpose | SPARK |
|---------|---------|-------|
| `Domain` | Layer root | On |
| `Domain.Error` | Error type with Kind + Message | On |
| `Domain.Error.Result` | Generic Result[T] monad | On |
| `Domain.Unit` | Unit type for void operations | On |
| `Domain.Value_Object.Person` | Person value object | On |

#### 3.2.2 Application Layer

| Package | Purpose | SPARK |
|---------|---------|-------|
| `Application` | Layer root | On |
| `Application.Command.Greet` | Greet command DTO | On |
| `Application.Port.Outbound.Writer` | Writer port definition | On |
| `Application.Usecase.Greet` | Greet use case | On |

#### 3.2.3 Infrastructure Layer

| Package | Purpose | SPARK |
|---------|---------|-------|
| `Infrastructure` | Layer root | Off |
| `Infrastructure.Adapter.Console_Writer` | Console output adapter | Off |

#### 3.2.4 API Layer

| Package | Purpose | SPARK |
|---------|---------|-------|
| `Hybrid_Lib_Ada` | Library root | Off |
| `Hybrid_Lib_Ada.API` | Public facade | Off |
| `Hybrid_Lib_Ada.API.Operations` | SPARK-safe operations | On |
| `Hybrid_Lib_Ada.API.Desktop` | Desktop composition root | Off |

---

## 4. Type Definitions

### 4.1 Domain Types

#### 4.1.1 Error_Kind

```ada
type Error_Kind is
  (Validation_Error,    -- Input validation failed
   IO_Error,            -- I/O operation failed
   Not_Found_Error,     -- Resource not found
   Already_Exists_Error,-- Resource exists
   Config_Error,        -- Configuration error
   Internal_Error);     -- Unexpected internal error
```

#### 4.1.2 Error_Type

```ada
type Error_Type is record
   Kind    : Error_Kind;
   Message : Error_String;  -- Bounded string
end record;
```

#### 4.1.3 Result (Generic)

```ada
generic
   type T is private;
package Domain.Error.Result.Generic_Result is
   type Result is private;

   function Ok (Value : T) return Result;
   function Error (Kind : Error_Kind; Message : String) return Result;

   function Is_Ok (R : Result) return Boolean;
   function Is_Error (R : Result) return Boolean;
   function Value (R : Result) return T;
   function Error_Info (R : Result) return Error_Type;
end Generic_Result;
```

#### 4.1.4 Person

```ada
type Person is private;

function Create (Name : String) return Person_Result.Result;
function Get_Name (P : Person) return String;
function Is_Valid_Person (P : Person) return Boolean;
```

### 4.2 Application Types

#### 4.2.1 Greet_Command

```ada
type Greet_Command is private;

function Create (Name : String) return Greet_Command;
function Get_Name (Cmd : Greet_Command) return String;
```

#### 4.2.2 Writer Port

```ada
generic
   with function Write (Message : String) return Unit_Result.Result;
package Generic_Writer is
   function Write_Message (Message : String) return Unit_Result.Result
   renames Write;
end Generic_Writer;
```

### 4.3 API Types

All public types are re-exported from `Hybrid_Lib_Ada.API`:

```ada
subtype Person_Type is Domain.Value_Object.Person.Person;
subtype Greet_Command is Application.Command.Greet.Greet_Command;
subtype Error_Type is Domain.Error.Error_Type;
```

---

## 5. Static Dependency Injection

### 5.1 Overview

Hybrid_Lib_Ada uses Ada generics for static (compile-time) dependency injection:

```ada
--  1. Port defines generic signature
generic
   with function Write (Message : String) return Unit_Result.Result;
package Generic_Writer is ...

--  2. Use case is generic, parameterized by Writer
generic
   with function Writer (Message : String) return Unit_Result.Result;
package Application.Usecase.Greet is ...

--  3. Composition root instantiates with concrete adapter
package Console_Ops is new Hybrid_Lib_Ada.API.Operations
  (Writer => Infrastructure.Adapter.Console_Writer.Write);
```

### 5.2 Benefits

| Benefit | Description |
|---------|-------------|
| Zero runtime overhead | Monomorphization at compile time |
| SPARK compatible | No runtime dispatching |
| Type safe | Compiler verifies contracts |
| Testable | Mock writers for unit tests |

---

## 6. Three-Package API Pattern

### 6.1 Problem Statement

How to provide:
- SPARK-verifiable operations
- Platform-specific wiring
- Clean public facade

### 6.2 Solution

```
┌─────────────────────────────────────────────────────────────┐
│                      User Code                               │
│   with Hybrid_Lib_Ada.API;                                  │
│   Result := API.Greet (Cmd);                                │
└────────────────────────────┬────────────────────────────────┘
                             │
┌────────────────────────────▼────────────────────────────────┐
│                  Hybrid_Lib_Ada.API                          │
│                  (Thin Facade)                               │
│  - Re-exports types                                          │
│  - Delegates Greet to Desktop                               │
│  - SPARK_Mode: Off                                          │
└────────────────────────────┬────────────────────────────────┘
                             │ delegates
┌────────────────────────────▼────────────────────────────────┐
│              Hybrid_Lib_Ada.API.Desktop                      │
│              (Composition Root)                              │
│  - Instantiates Operations with Console_Writer               │
│  - SPARK_Mode: Off (I/O wiring)                             │
│  - Located in api/desktop/ (arch_guard exception)           │
└────────────────────────────┬────────────────────────────────┘
                             │ instantiates
┌────────────────────────────▼────────────────────────────────┐
│            Hybrid_Lib_Ada.API.Operations                     │
│            (SPARK-Safe Operations)                           │
│  - Generic, parameterized by Writer                          │
│  - SPARK_Mode: On (formally verifiable)                     │
│  - Depends ONLY on Application/Domain                        │
└─────────────────────────────────────────────────────────────┘
```

### 6.3 SPARK Verification Boundary

| Package | SPARK_Mode | Reason |
|---------|------------|--------|
| API.Operations | On | Pure logic, verifiable |
| API.Desktop | Off | I/O wiring |
| API (facade) | Off | Delegates to Desktop |
| Infrastructure.* | Off | I/O operations |
| Application.* | On | Business logic |
| Domain.* | On | Core domain |

### 6.4 Platform-Specific Composition Roots

`API.Desktop` is the default composition root for desktop/server environments. For other platforms (embedded, web, testing), create additional composition roots:

| Platform | Composition Root | Writer Adapter |
|----------|------------------|----------------|
| Desktop | `API.Desktop` | Console_Writer |
| Embedded | `API.Embedded` | UART_Writer, LCD_Writer |
| Web | `API.Web` | DOM_Writer |
| Testing | `API.Test` | Mock_Writer |

Each composition root:
1. Implements the same interface as `API.Desktop`
2. Instantiates `API.Operations` with a platform-specific adapter
3. Lives under `src/api/<platform>/`
4. Is recognized by arch_guard as a composition root

**For detailed instructions on creating platform-specific composition roots, including step-by-step examples for embedded systems (STM32F769I), GPR configuration, and troubleshooting, see [All About Our API](../guides/all_about_our_api.md#creating-platform-specific-composition-roots).**

---

## 7. Error Handling Strategy

### 7.1 Result Monad Pattern

All fallible operations return `Result[T]`:

```ada
function Create (Name : String) return Person_Result.Result;
--  Returns Ok(Person) or Error(Validation_Error, "message")

function Greet (Cmd : Greet_Command) return Unit_Result.Result;
--  Returns Ok(Unit) or Error(IO_Error, "message")
```

### 7.2 Error Propagation

Errors flow through use case orchestration:

```ada
function Execute (Cmd : Greet_Command) return Unit_Result.Result is
   Person_Res : constant Person_Result.Result :=
     Person.Create (Get_Name (Cmd));
begin
   if Person_Result.Is_Error (Person_Res) then
      --  Propagate domain validation error
      return Unit_Result.Error (...);
   end if;

   --  Write greeting via port
   return Writer (Format_Greeting (Person_Result.Value (Person_Res)));
end Execute;
```

### 7.3 No Exceptions Policy

| Situation | Handling |
|-----------|----------|
| Validation failure | Return Error result |
| I/O failure | Return Error result |
| Unexpected error | Return Internal_Error result |
| Programmer error | Assert/raise (debug only) |

---

## 8. Build Configuration

### 8.1 GPR Projects

| Project | Purpose |
|---------|---------|
| `hybrid_lib_ada.gpr` | Public library (restricted interfaces) |
| `hybrid_lib_ada_internal.gpr` | Internal (unrestricted, for tests) |

### 8.2 Build Profiles

| Profile | Target | Features |
|---------|--------|----------|
| `standard` | Desktop/server | Full features |
| `embedded` | Ravenscar embedded | Tasking safe |
| `baremetal` | Zero footprint | Minimal runtime |

---

## 9. Design Decisions

### 9.1 API.Operations as Child vs Sibling

**Decision:** `API.Operations` (child) instead of `API_Operations` (sibling)

**Rationale:**
- Clean hierarchy is more idiomatic Ada
- SPARK works either way
- Preelaborate adds minimal value for consumers

### 9.2 No Heap Allocation

**Decision:** All types use bounded strings and stack allocation

**Rationale:**
- Embedded system compatibility
- SPARK compatibility
- Deterministic behavior

### 9.3 Static vs Dynamic Polymorphism

**Decision:** Static polymorphism via generics

**Rationale:**
- SPARK compatible
- Zero runtime overhead
- Compile-time type safety

---

## 10. Appendices

### A. Package Dependency Graph

```
Hybrid_Lib_Ada.API
    ├── Hybrid_Lib_Ada.API.Desktop
    │       └── Infrastructure.Adapter.Console_Writer
    │               └── Application.Port.Outbound.Writer
    │                       └── Domain.Error.Result
    │                               └── Domain.Error
    │                                       └── Domain
    └── Domain.Value_Object.Person
            └── Domain.Error.Result
                    └── Domain.Error
```

### B. Change History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2025-11-29 | Michael Gardner | Initial release |
