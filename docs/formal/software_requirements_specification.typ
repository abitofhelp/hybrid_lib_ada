// ============================================================================
// File: srs_lib.typ
// Purpose: Software Requirements Specification for the Hybrid_Lib_Ada project.
// Scope: Project-specific SRS content plus invocation of shared formal-document
//   functionality from core.typ.
// Usage: This is an authoritative Typst source document. The generated PDF is
//   the distribution artifact.
// Modification Policy:
//   - Edit this file for project-specific SRS content.
//   - Keep shared presentation logic in core.typ.
// Table Ordering:
//   Sort any table whose rows a reader might scan to locate a specific
//   entry — definitions, acronyms, constraints, packages, interfaces,
//   and similar reference tables.  Sort alphabetically by the first
//   column.  Tables with an inherent sequence (requirement IDs within
//   a section, change history, workflow steps) retain their logical order.
// SPDX-License-Identifier: BSD-3-Clause
// ============================================================================

#import "core.typ": change_history_table, formal_doc

#let doc = (
  authors: ("Michael Gardner",),
  copyright: "© 2025 Michael Gardner, A Bit of Help, Inc.",
  license_file: "See the LICENSE file in the project root",
  project_name: "HYBRID_LIB_ADA",
  spdx_license: "BSD-3-Clause",
  status: "Released", // valid: "Draft" | "Review" | "Released" | "Archived"
  status_date: "2025-12-14",
  title: "Software Requirements Specification",
  version: "2.1.0",
)

#let profile = (
  app_role: none, // valid: "cli" | "service" | "embedded"
  assurance: "spark-targeted", // valid: "mandatory-spark" | "spark-targeted" | "non-spark"
  deployment: "native", // valid: "native" | "containerized" | "hybrid"
  execution: "sequential", // valid: "sequential" | "concurrent"
  execution_environment: ("linux", "macos", "windows", "ada-runtime"),
  // valid items: "linux" | "linux-rt" | "macos" | "windows" | "ios" | "ada-runtime"
  library_role: "enterprise", // valid: "enterprise" | "utility"
  parallelism: "none", // valid: "none" | "optional" | "required" | "bounded"
  platform: ("desktop", "server", "embedded"), // valid items: "cloud" | "server" | "desktop" | "embedded"
  processor_architecture: ("amd64", "arm64", "arm32"), // valid items: "amd64" | "arm64" | "arm32" | "other"
  variant: "library", // valid: "library" | "application"
)

#let change_history = (
  (
    version: "2.1.0",
    date: "2025-12-14",
    author: "Michael Gardner",
    changes: "Remove project statistics section; metrics belong in CHANGELOG. Add Section 6.2 Architectural Constraints and explicit architecture-enforcement language.",
  ),
  (
    version: "2.0.0",
    date: "2025-12-09",
    author: "Michael Gardner",
    changes: "Complete regeneration for v2.0.0; corrected Error_Kind values; added NFR-08 Testability.",
  ),
  (
    version: "1.0.0",
    date: "2025-12-08",
    author: "Michael Gardner",
    changes: "Initial release.",
  ),
)

#show: formal_doc.with(doc, profile, change_history)

= Introduction

== Purpose

This Software Requirements Specification (SRS) defines the functional and non-functional requirements for *Hybrid_Lib_Ada*, a canonical Ada 2022 library demonstrating hybrid DDD/Clean/Hexagonal architecture with functional error handling.

== Scope

*Hybrid_Lib_Ada* provides:

- A reusable library for greeting operations.
- Demonstration of DDD/Clean/Hexagonal architecture in Ada.
- Functional error handling via a Result monad.
- SPARK-compatible design for formal verification.
- Embedded-safe patterns that avoid heap allocation.

== Definitions and Acronyms

// Sorted alphabetically by Term.
#table(
  columns: (auto, 1fr),
  table.header([*Term*], [*Definition*]),
  [DDD],
  [Domain-Driven Design — strategic and tactical patterns for complex software.],

  [Hexagonal Architecture],
  [Ports & Adapters pattern isolating business logic from infrastructure.],

  [Result Monad], [Functional pattern for error handling without exceptions.],
  [SPARK], [Ada subset for formal verification.],
  [Value Object], [Immutable domain object defined by its attributes.],
)

== References

- Ada 2022 Reference Manual (ISO/IEC 8652:2023).
- SPARK 2014 Reference Manual.
- *Domain-Driven Design* (Eric Evans, 2003).
- *Clean Architecture* (Robert C. Martin, 2017).

= Overall Description

== Product Perspective

*Hybrid_Lib_Ada* is a standalone library designed to be imported by Ada applications implementing hexagonal (ports and adapters) architecture with clean separation between domain logic, application use cases, and infrastructure adapters.

This library profile uses the same hybrid Domain/Application/Infrastructure core as *Hybrid_App_Ada*, but expresses the outer boundary through `API`, `API.Desktop`, and `API.Operations` rather than `Presentation` and `Bootstrap`.

#table(
  columns: (1fr,),
  align: center + horizon,
  inset: 10pt,
  stroke: 0.8pt,
  [
    *Client Application* \
    `with Hybrid_Lib_Ada.API;` \
    `Result := API.Greet (API.Create_Greet_Command("X"));`
  ],
  [*↓*],
  [
    *Hybrid_Lib_Ada* \
    API Layer → Application Layer → Domain Layer \
    Infrastructure Layer (adapters)
  ],
)

Supporting architecture guidance is maintained in `docs/guides/`. Supporting UML diagrams are maintained in `docs/diagrams/`.

#table(
  columns: (auto, 1fr),
  table.header([*Layer*], [*Purpose*]),
  [Domain], [Pure business logic, value objects, error types.],
  [Application], [Use cases, commands, ports (interfaces).],
  [Infrastructure], [Adapters for I/O operations.],
  [API], [Public facade with stable interface.],
)

== Product Features

1. *Greeting Operations*: Generate personalized greetings via clean architecture.
2. *Domain Modeling*: Provide a Person value object with validation.
3. *Error Handling*: Use railway-oriented programming with Result monads.
4. *Platform Abstraction*: Provide a generic I/O plugin pattern for portability.
5. *Formal Verification*: Maintain a SPARK-compatible domain layer.

== User Classes

// Sorted alphabetically by User Class.
#table(
  columns: (auto, 1fr),
  table.header([*User Class*], [*Description*]),
  [Architecture Students], [Developers learning hexagonal architecture in Ada.],
  [Embedded Developers],
  [Developers requiring heap-free, SPARK-compatible patterns.],

  [Library Consumers], [Ada developers integrating library functionality.],
)

== Operating Environment

#table(
  columns: (auto, 1fr),
  table.header([*Requirement*], [*Specification*]),
  [Platforms], [POSIX (Linux, macOS, BSD), Windows 11, Embedded.],
  [Ada Compiler], [GNAT FSF 13+ or GNAT Pro.],
  [Ada Version], [Ada 2022.],
  [Dependencies], [`functional ^3.0.0`],
)

== Constraints

// Sorted alphabetically by Constraint.
#table(
  columns: (auto, 1fr),
  table.header([*Constraint*], [*Rationale*]),
  [Ada 2022], [Required for modern language features.],
  [GNAT 13+], [Required compiler version.],
  [No Heap Allocation], [Embedded system compatibility.],
  [SPARK Subset], [Formal verification capability.],
)

= Interface Requirements

== User Interfaces

None. This is a library, not an application.

== Software Interfaces

=== Alire Integration

```toml
[[depends-on]]
hybrid_lib_ada = "*"
```

=== Ada API

```ada
with Hybrid_Lib_Ada.API;
use Hybrid_Lib_Ada.API;

Cmd    : constant Greet_Command := Create_Greet_Command ("Name");
Result : constant Unit_Result.Result := Greet (Cmd);
```

== Hardware Interfaces

None. The library is hardware-agnostic.

= Functional Requirements

== Domain Layer (FR-01)

*Priority:* High
*Description:* Provide core domain entities and value objects.

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [FR-01.1], [Person value object with bounded name (1–100 characters).],
  [FR-01.2], [Person is immutable after creation.],
  [FR-01.3], [Empty names rejected with `Validation_Error`.],
  [FR-01.4], [Structured error types with `Kind` enumeration and `Message`.],
  [FR-01.5], [Result monad for all fallible operations.],
  [FR-01.6],
  [Error kinds: `Validation_Error`, `Parse_Error`, `Not_Found_Error`, `IO_Error`, `Internal_Error`.],
)

*Acceptance Criteria (FR-01):*

- Person value object validates name on creation.
- Result is either `Ok(value)` or `Error(error_info)`.
- No exceptions are raised for expected errors.
- Value extraction remains type-safe.

== Application Layer (FR-02)

*Priority:* High
*Description:* Provide use cases and port definitions.

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [FR-02.1],
  [Provide `Greet_Command` data transfer object for greeting requests.],

  [FR-02.2], [Provide Greet use case orchestrating the greeting workflow.],
  [FR-02.3], [Define `Writer` outbound port for output operations.],
  [FR-02.4], [Use static polymorphism via generic function signatures.],
  [FR-02.5], [Return `Result[Unit]` for all operations.],
)

*Acceptance Criteria (FR-02):*

- `Greet_Command` encapsulates the name and remains immutable after creation.
- The Greet use case accepts a command, creates a `Person`, generates the greeting, and writes through the port.
- The `Writer` port provides a generic signature equivalent to `Write(Message) -> Result[Unit]`.
- The port is Application-owned and Infrastructure-implemented.

== Infrastructure Layer (FR-03)

*Priority:* High
*Description:* Provide platform-specific adapters implementing ports.

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [FR-03.1], [Provide `Console_Writer` adapter for standard output.],
  [FR-03.2], [Implement the `Writer` port contract.],
  [FR-03.3], [Return `Ok` on success and `IO_Error` on failure.],
)

*Acceptance Criteria (FR-03):*

- `Console_Writer` writes to standard output.
- All I/O errors are captured and returned as Result values.

== API Layer (FR-04)

*Priority:* High
*Description:* Provide stable public interface.

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [FR-04.1], [Expose a single `Hybrid_Lib_Ada.API` package for imports.],
  [FR-04.2], [Re-export domain types such as `Person`, `Error`, and `Unit`.],
  [FR-04.3],
  [Re-export application types such as `Greet_Command` and `Unit_Result`.],

  [FR-04.4],
  [Provide `API.Operations` with `SPARK_Mode(On)` for verifiable logic.],

  [FR-04.5], [Provide `API.Desktop` composition root wiring `Console_Writer`.],
)

*Acceptance Criteria (FR-04):*

- `API.Operations` is generic and parameterized by the `Writer` port.
- `Operations` introduces no Infrastructure dependencies.
- `API.Desktop` wires `Console_Writer` and uses `SPARK_Mode(Off)` for I/O wiring.

== Error Handling (FR-05)

*Priority:* High
*Description:* Railway-oriented error handling without exceptions.

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [FR-05.1], [Use Result monad for all fallible operations.],
  [FR-05.2], [Provide descriptive error messages.],
  [FR-05.3], [Provide error codes for all failure modes.],
  [FR-05.4], [Do not use exceptions in library code for expected failures.],
)

*Error Handling Policy Note:* Expected failures shall be represented as `Result` errors rather than propagated exceptions. Unexpected runtime failures (for example `Program_Error` or `Storage_Error`) remain outside this policy and shall be contained at architectural boundaries using `Functional.Try` or equivalent boundary handling.

= Quality and Cross-Cutting Requirements

== Performance (NFR-01)

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [NFR-01.1], [Operation latency < 1 ms (excluding I/O).],
  [NFR-01.2], [Zero heap allocation.],
  [NFR-01.3], [Stack usage < 4 KB per call.],
)

== Reliability (NFR-02)

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [NFR-02.1],
  [Return all errors via Result monad with no exceptions for expected failures.],

  [NFR-02.2], [Validate all inputs at the domain boundary.],
  [NFR-02.3], [Permit no uncaught exceptions for expected error paths.],
)

== Portability (NFR-03)

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [NFR-03.1], [Support POSIX platforms (Linux, macOS, BSD).],
  [NFR-03.2], [Support Windows platforms.],
  [NFR-03.3], [Support embedded platforms via custom adapters.],
  [NFR-03.4], [Keep platform-specific code out of the domain layer.],
  [NFR-03.5], [Expose no infrastructure types in application-layer ports.],
  [NFR-03.6], [Select platform adapters via GPR configuration.],
)

== Maintainability (NFR-04)

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [NFR-04.1], [Use hexagonal architecture with clear boundaries.],
  [NFR-04.2], [Provide comprehensive documentation and docstrings.],
  [NFR-04.3], [Maintain > 90% test coverage.],
  [NFR-04.4], [Maintain zero compiler warnings.],
)

== Usability (NFR-05)

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [NFR-05.1], [Provide a clear, intuitive API.],
  [NFR-05.2], [Provide working examples for all use cases.],
  [NFR-05.3], [Provide comprehensive error messages.],
)

== Platform Abstraction (NFR-06)

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [NFR-06.1],
  [Application layer defines abstract outbound ports using pure function signatures.],

  [NFR-06.2],
  [Infrastructure layer provides platform-specific adapters implementing ports.],

  [NFR-06.3],
  [Composition roots (`API.Desktop`, `API.Windows`, `API.Embedded`) wire adapters to ports.],

  [NFR-06.4], [Use domain types in port signatures, not infrastructure types.],
  [NFR-06.5], [Add new platforms without modifying the application layer.],
  [NFR-06.6], [Keep all platform adapters testable via mock implementations.],
)

== SPARK Formal Verification (NFR-07)

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [NFR-07.1], [Domain layer shall pass SPARK legality checking.],
  [NFR-07.2], [All domain packages shall use `SPARK_Mode => On`.],
  [NFR-07.3],
  [Permit no provable runtime errors in the domain layer (overflow, range, division).],

  [NFR-07.4], [Require all domain variables to be initialized before use.],
  [NFR-07.5],
  [Require preconditions and postconditions on domain operations to be provably correct.],

  [NFR-07.6], [Run SPARK legality verification via `make spark-check`.],
  [NFR-07.7], [Run SPARK proof verification via `make spark-prove`.],
  [NFR-07.8],
  [Infrastructure and API layers may use `SPARK_Mode => Off` for I/O operations.],
)

#table(
  columns: (auto, auto, 1fr),
  table.header([*Layer*], [*SPARK_Mode*], [*Rationale*]),
  [Domain], [On], [Pure business logic, provable.],
  [Application], [On], [Operations, inbound ports, outbound ports.],
  [Infrastructure], [Off], [I/O operations.],
  [API], [Off], [Facade over infrastructure.],
)

== Testability (NFR-08)

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [NFR-08.1], [Provide unit tests for all domain packages.],
  [NFR-08.2], [Provide integration tests for cross-layer interactions.],
  [NFR-08.3], [Provide mock adapters for testing without I/O.],
  [NFR-08.4], [Provide test runners with pass/fail reporting.],
)

= Design and Implementation Constraints

== System Requirements

=== Hardware Requirements

// Sorted alphabetically by Category.
#table(
  columns: (auto, 1fr),
  table.header([*Category*], [*Requirement*]),
  [CPU], [Any modern processor.],
  [Disk], [10 MB minimum.],
  [RAM], [64 MB minimum.],
)

=== Software Requirements

// Sorted alphabetically by Category.
#table(
  columns: (auto, 1fr),
  table.header([*Category*], [*Requirement*]),
  [Build System], [Alire 2.0+.],
  [Compiler], [GNAT FSF 13+ or GNAT Pro.],
  [Operating System], [Linux, macOS, BSD, Windows 11.],
)

== Architectural Constraints

- Maintain domain isolation from infrastructure.
- Keep application-owned ports free from infrastructure types.
- Preserve composition-root responsibility for adapter wiring.
- Document any project-specific exceptions explicitly.

Architecture conformance shall be enforced through a combination of project/build configuration constraints and automated validation using `scripts/arch_guard.py`. The architecture guard shall be run during normal developer build workflow to detect direct dependency violations early and in CI/CD to prevent non-conforming changes from being accepted.

= Verification and Traceability

== Verification Methods

// Sorted alphabetically by Method.
#table(
  columns: (auto, 1fr),
  table.header([*Method*], [*Description*]),
  [Architecture Validation],
  [`arch_guard.py` validates direct dependency rules during developer builds and CI/CD; project/build interface restrictions provide additional enforcement where configured.],

  [Code Review], [All code reviewed before merge.],
  [Coverage Analysis], [> 90% line coverage.],
  [Dynamic Testing], [All tests must pass.],
  [Static Analysis], [Zero compiler warnings.],
)

== Traceability Matrix

#table(
  columns: (auto, 1fr, 1fr),
  table.header([*Requirement*], [*Design*], [*Test*]),
  [FR-01.1], [Domain.Value_Object.Person], [test_domain_person.adb],
  [FR-01.4], [Domain.Error], [test_domain_error_result.adb],
  [FR-01.5], [Domain.Error.Result], [test_domain_error_result.adb],
  [FR-02.1], [Application.Command.Greet], [test_application_command_greet.adb],
  [FR-02.2], [Application.Usecase.Greet], [test_application_usecase_greet.adb],
  [FR-02.3],
  [Application.Port.Outbound.Writer],
  [test_application_usecase_greet.adb],

  [FR-03.1], [Infrastructure.Adapter.Console_Writer], [test_api_greet.adb],
  [FR-04.1], [Hybrid_Lib_Ada.API], [test_api_greet.adb],
  [FR-04.4], [Hybrid_Lib_Ada.API.Operations], [test_api_operations.adb],
  [FR-04.5], [Hybrid_Lib_Ada.API.Desktop], [test_api_greet.adb],
)

= Appendices

== Glossary

See Section 1.3 Definitions and Acronyms.

== Change History

#change_history_table(change_history)
