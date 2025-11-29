# Software Requirements Specification

**Version:** 1.0.0<br>
**Date:** 2025-11-29<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## 1. Introduction

### 1.1 Purpose

This Software Requirements Specification (SRS) defines the functional and non-functional requirements for **Hybrid_Lib_Ada**, a canonical Ada 2022 library demonstrating hexagonal architecture patterns with functional error handling.

### 1.2 Scope

Hybrid_Lib_Ada provides:
- A reusable library for greeting operations
- Demonstration of DDD/Clean/Hexagonal architecture in Ada
- Functional error handling via Result monad
- SPARK-compatible design for formal verification
- Embedded-safe patterns (no heap allocation)

### 1.3 Definitions

| Term | Definition |
|------|------------|
| **DDD** | Domain-Driven Design - strategic and tactical patterns for complex software |
| **Hexagonal Architecture** | Ports & Adapters pattern isolating business logic from infrastructure |
| **Result Monad** | Functional pattern for error handling without exceptions |
| **SPARK** | Ada subset for formal verification |
| **Value Object** | Immutable domain object defined by its attributes |

### 1.4 References

- Ada 2022 Reference Manual (ISO/IEC 8652:2023)
- SPARK 2014 Reference Manual
- Domain-Driven Design (Eric Evans, 2003)
- Clean Architecture (Robert C. Martin, 2017)

---

## 2. Overall Description

### 2.1 Product Perspective

Hybrid_Lib_Ada is a standalone library designed to be imported by Ada applications. It provides:

```
┌─────────────────────────────────────────────────────────┐
│                   Client Application                     │
│                                                          │
│   with Hybrid_Lib_Ada.API;                              │
│   Result := API.Greet (API.Create_Greet_Command ("X")); │
└────────────────────────┬────────────────────────────────┘
                         │
┌────────────────────────▼────────────────────────────────┐
│                   Hybrid_Lib_Ada                         │
│                                                          │
│  API Layer → Application Layer → Domain Layer            │
│       ↓                                                  │
│  Infrastructure Layer (adapters)                         │
└─────────────────────────────────────────────────────────┘
```

### 2.2 Product Functions

| Function | Description |
|----------|-------------|
| **Greet** | Generate and output a personalized greeting |
| **Create_Person** | Create validated Person value object |
| **Create_Greet_Command** | Create command for greet operation |

### 2.3 User Characteristics

| User Type | Characteristics |
|-----------|-----------------|
| **Library Consumer** | Ada developer integrating greeting functionality |
| **Architecture Student** | Learning hexagonal architecture in Ada |
| **Embedded Developer** | Requiring heap-free, SPARK-compatible patterns |

### 2.4 Constraints

| Constraint | Rationale |
|------------|-----------|
| Ada 2022 | Required for modern language features |
| GNAT 14+ | Required compiler version |
| No Heap Allocation | Embedded system compatibility |
| SPARK Subset | Formal verification capability |

### 2.5 Assumptions and Dependencies

- Alire 2.0+ available for dependency management
- `functional` crate available (Result monad implementation)
- GNAT toolchain installed via Alire

---

## 3. Functional Requirements

### 3.1 Domain Layer Requirements

#### REQ-DOM-001: Person Value Object

**Description:** The system SHALL provide a Person value object representing a named individual.

**Acceptance Criteria:**
- Person is immutable after creation
- Person name is validated on creation
- Name length is bounded (1 to 100 characters)
- Empty names are rejected with Validation_Error

#### REQ-DOM-002: Error Types

**Description:** The system SHALL define structured error types for all failure modes.

**Acceptance Criteria:**
- Error includes Kind enumeration
- Error includes human-readable Message
- Error Kinds: Validation_Error, IO_Error, Not_Found_Error, Already_Exists_Error, Config_Error, Internal_Error

#### REQ-DOM-003: Result Monad

**Description:** The system SHALL use Result[T] for all fallible operations.

**Acceptance Criteria:**
- Result is either Ok(value) or Error(error_info)
- No exceptions raised for expected errors
- Type-safe value extraction

### 3.2 Application Layer Requirements

#### REQ-APP-001: Greet Command

**Description:** The system SHALL provide a Greet_Command data transfer object.

**Acceptance Criteria:**
- Encapsulates name for greeting
- Immutable after creation
- No validation (thin DTO)

#### REQ-APP-002: Greet Use Case

**Description:** The system SHALL provide a Greet use case orchestrating the greeting workflow.

**Acceptance Criteria:**
- Accepts Greet_Command
- Creates Person from command name
- Generates greeting message
- Writes message via Writer port
- Returns Result[Unit]

#### REQ-APP-003: Writer Port

**Description:** The system SHALL define an output port for write operations.

**Acceptance Criteria:**
- Generic function signature: Write(Message) -> Result[Unit]
- Port is Application-owned, Infrastructure-implemented
- Static polymorphism via generics

### 3.3 Infrastructure Layer Requirements

#### REQ-INF-001: Console Writer Adapter

**Description:** The system SHALL provide a Console_Writer adapter.

**Acceptance Criteria:**
- Implements Writer port contract
- Writes to standard output
- Returns Ok on success
- Returns IO_Error on failure

### 3.4 API Layer Requirements

#### REQ-API-001: Public Facade

**Description:** The system SHALL provide a thin public facade for library consumers.

**Acceptance Criteria:**
- Single `Hybrid_Lib_Ada.API` package for imports
- Re-exports Domain types (Person, Error, Unit)
- Re-exports Application types (Greet_Command, Unit_Result)
- Provides Greet function

#### REQ-API-002: SPARK-Safe Operations

**Description:** The system SHALL provide SPARK-verifiable operations.

**Acceptance Criteria:**
- API.Operations package with SPARK_Mode(On)
- Generic, parameterized by Writer port
- No Infrastructure dependencies
- Formally verifiable business logic

#### REQ-API-003: Composition Root

**Description:** The system SHALL provide platform-specific composition roots.

**Acceptance Criteria:**
- API.Desktop wires Console_Writer
- SPARK_Mode(Off) for I/O wiring
- Instantiates API.Operations with concrete adapter

---

## 4. Non-Functional Requirements

### 4.1 Performance

| Requirement | Target |
|-------------|--------|
| Greet operation latency | < 1ms (excluding I/O) |
| Memory allocation | Zero heap allocation |
| Stack usage | < 4KB per call |

### 4.2 Reliability

| Requirement | Description |
|-------------|-------------|
| Error handling | All errors returned via Result, no exceptions |
| Input validation | All inputs validated at domain boundary |
| Crash safety | No uncaught exceptions possible |

### 4.3 Portability

| Requirement | Description |
|-------------|-------------|
| Compiler | GNAT 14+ |
| Platforms | Linux, macOS, Windows |
| Embedded | Ravenscar-compatible design |
| Bare-metal | Zero-footprint runtime option |

### 4.4 Maintainability

| Requirement | Description |
|-------------|-------------|
| Architecture | 4-layer hexagonal (Domain/Application/Infrastructure/API) |
| Coupling | Inward dependencies only |
| Testing | Unit tests for each layer |
| Documentation | Full API and architecture documentation |

### 4.5 Security

| Requirement | Description |
|-------------|-------------|
| Input validation | Bounded strings prevent buffer overflow |
| No dynamic memory | Prevents use-after-free, double-free |
| SPARK compatible | Enables formal verification |

---

## 5. Interface Requirements

### 5.1 User Interfaces

None - this is a library, not an application.

### 5.2 Software Interfaces

#### 5.2.1 Alire Integration

```toml
[[depends-on]]
hybrid_lib_ada = "*"
```

#### 5.2.2 Ada API

```ada
with Hybrid_Lib_Ada.API;
use Hybrid_Lib_Ada.API;

Cmd    : constant Greet_Command := Create_Greet_Command ("Name");
Result : constant Unit_Result.Result := Greet (Cmd);
```

### 5.3 Hardware Interfaces

None specified - library is hardware-agnostic.

---

## 6. Traceability Matrix

| Requirement | Design | Test |
|-------------|--------|------|
| REQ-DOM-001 | Domain.Value_Object.Person | test_domain_person.adb |
| REQ-DOM-002 | Domain.Error | test_domain_error_result.adb |
| REQ-DOM-003 | Domain.Error.Result | test_domain_error_result.adb |
| REQ-APP-001 | Application.Command.Greet | test_application_command_greet.adb |
| REQ-APP-002 | Application.Usecase.Greet | test_application_usecase_greet.adb |
| REQ-APP-003 | Application.Port.Outbound.Writer | test_application_usecase_greet.adb |
| REQ-INF-001 | Infrastructure.Adapter.Console_Writer | test_api_greet.adb |
| REQ-API-001 | Hybrid_Lib_Ada.API | test_api_greet.adb |
| REQ-API-002 | Hybrid_Lib_Ada.API.Operations | test_api_operations.adb |
| REQ-API-003 | Hybrid_Lib_Ada.API.Desktop | test_api_greet.adb |

---

## 7. Appendices

### A. Glossary

See Section 1.3 Definitions.

### B. Change History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2025-11-29 | Michael Gardner | Initial release |
