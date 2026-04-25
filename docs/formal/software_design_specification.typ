// ============================================================================
// File: software_design_specification.typ
// Purpose: Software Design Specification for HYBRID_LIB_ADA.
// Scope: Real project SDS content using the shared formal Typst core.
// Usage: Compile to PDF as the distribution artifact. The .typ source is the
//   authoritative document source.
// Modification Policy:
//   - Keep shared presentation logic in core.typ.
//   - Keep project-specific design content here.
// Table Ordering:
//   Sort any table whose rows a reader might scan to locate a specific
//   entry — definitions, acronyms, constraints, packages, interfaces,
//   and similar reference tables.  Sort alphabetically by the first
//   column.  Tables with an inherent sequence (requirement IDs within
//   a section, change history, workflow steps) retain their logical order.
// SPDX-License-Identifier: BSD-3-Clause
// ============================================================================

#import "core.typ": formal_doc

#let doc = (
  authors: ("Michael Gardner",),
  copyright: "© 2025 Michael Gardner, A Bit of Help, Inc.",
  license_file: "See the LICENSE file in the project root",
  project_name: "HYBRID_LIB_ADA",
  spdx_license: "BSD-3-Clause",
  status: "Released",
  status_date: "2026-04-24",
  title: "Software Design Specification",
  version: "2.0.2",
)

#let profile = (
  app_role: none,
  assurance: "spark-targeted",
  deployment: "native",
  execution: "sequential",
  execution_environment: ("linux", "macos", "windows", "ada-runtime"),
  library_role: "enterprise",
  parallelism: "none",
  platform: ("desktop", "server", "embedded"),
  processor_architecture: ("amd64", "arm64", "arm32"),
  variant: "library",
)

#let change_history = (
  (
    version: "2.0.2",
    date: "2026-04-24",
    author: "Michael Gardner",
    changes: "Patch — added Library_Standalone Design Decision under Build Configuration. Documents the rationale for Library_Standalone = \"standard\" (composability across Ada toolchain ecosystems, avoidance of duplicate-runtime link failures from \"encapsulated\" stand-alone libraries) and credits Library_Interface as the public-API enforcement mechanism. Cites the arch_guard enforcement boundary (ROOT_GPR_ENCAPSULATED_ERROR).",
  ),
  (
    version: "2.0.1",
    date: "2025-12-11",
    author: "Michael Gardner",
    changes: "Added Section 6.3 Exception Boundary Specification with architecture diagram, layer rules, and required/prohibited patterns.",
  ),
  (
    version: "2.0.0",
    date: "2025-12-09",
    author: "Michael Gardner",
    changes: "Complete regeneration for v2.0.0; corrected Error_Kind (5 values); updated package structure.",
  ),
  (
    version: "1.0.0",
    date: "2025-11-29",
    author: "Michael Gardner",
    changes: "Initial release.",
  ),
)

// Extra appendix subsections rendered before the auto-appended Change History.
#let extra_appendices = [
  == Package Dependency Notes

  The package dependency structure reflects the same hybrid core as the application profile, with the API pattern providing the library-specific outer boundary. Supporting UML diagrams are maintained in `docs/diagrams/`.
]

#show: formal_doc.with(doc, profile, change_history, extra_appendix_body: extra_appendices)

= Introduction

== Purpose

This Software Design Specification (SDS) describes the internal architecture, package structure, key type definitions, build strategy, and design decisions for *HYBRID_LIB_ADA*.

== Scope

This document covers:
- the 4-layer hybrid library architecture,
- the three-package API pattern,
- package hierarchy and dependency rules,
- core type definitions and contracts,
- static dependency injection via Ada generics,
- SPARK verification boundaries, and
- the layered exception-boundary enforcement strategy.

== References

- Software Requirements Specification (SRS).
- `docs/guides/all_about_our_api.md`.
- `docs/guides/architecture_enforcement.md`.
- Supporting UML diagrams maintained in `docs/diagrams/`.
- Ada 2022 Reference Manual.
- SPARK Reference Manual.

= Architectural Overview

== Layer Architecture

*HYBRID_LIB_ADA* uses the shared hybrid architecture family and the same inner core as the application variant: *Domain → Application → Infrastructure*. The library profile differs only at the outer boundary, where it uses the API pattern instead of Presentation + Bootstrap.

#table(
  columns: (auto, 1fr, auto),
  table.header([*Layer / Boundary*], [*Purpose*], [*Depends On*]),
  [API Facade (`Hybrid_Lib_Ada.API`)],
  [Thin public facade; re-exports public types and delegates operations to the default composition root.],
  [Application + Domain],

  [API Composition Root (`API.Desktop`)],
  [Default desktop/server composition root; wires infrastructure adapters into the generic operations surface.],
  [All layers],

  [API Operations (`API.Operations`)],
  [SPARK-safe operations surface parameterized by port implementations.],
  [Application + Domain],

  [Infrastructure],
  [Adapters for I/O and external integration; implements application-owned ports.],
  [Application + Domain],

  [Application], [Use cases, commands, and port definitions.], [Domain],
  [Domain],
  [Pure business concepts, value objects, and error types.],
  [Nothing],
)

Detailed architecture guidance is maintained in `docs/guides/`. Supporting UML diagrams are maintained in `docs/diagrams/`.

== Dependency Rules

The API layer has *two distinct areas* with different dependency rules.

#table(
  columns: (auto, 1fr),
  table.header([*Component*], [*May Depend On*]),
  [Domain], [Nothing (zero dependencies).],
  [Application], [Domain only.],
  [Infrastructure], [Application and Domain.],
  [API facade (`api/`)], [Application + Domain only.],
  [API composition roots (`api/desktop/`)],
  [All layers, including Infrastructure.],
)

The API facade (`Hybrid_Lib_Ada.API`, `API.Operations`) provides the public interface and does not import Infrastructure. The composition root (`API.Desktop`) is the intentional exception that wires concrete adapters to the generic operations surface.

Architecture conformance is enforced through layered controls:
- project/build configuration constraints where applicable,
- active validation using `scripts/arch_guard.py`,
- execution of `scripts/arch_guard.py` during normal developer build workflow, and
- execution of `scripts/arch_guard.py` in CI/CD before non-conforming changes are accepted.

== Hexagonal Pattern

The Application layer defines ports and use cases. Infrastructure adapters implement outbound ports. Domain and Application remain independent of concrete I/O concerns.

// Sort rows alphabetically by the first column.
#table(
  columns: (auto, 1fr),
  table.header([*Element*], [*Role*]),
  [Application.Port.Outbound.Writer],
  [Application-owned outbound port contract.],

  [Application.Usecase.Greet],
  [Orchestrates the greeting use case using domain validation and an outbound Writer port.],

  [Infrastructure.Adapter.Console_Writer],
  [Concrete infrastructure adapter implementing the Writer port.],
)

= Package Structure

== Directory Layout

```text
src/
├── hybrid_lib_ada.ads              # Root package
├── version/
│   └── hybrid_lib_ada-version.ads  # Version information
│
├── domain/
│   ├── domain.ads                  # Domain layer root
│   ├── domain-unit.ads             # Unit type
│   ├── error/
│   │   ├── domain-error.ads        # Error type definition
│   │   └── domain-error-result.ads # Generic Result monad
│   └── value_object/
│       ├── domain-value_object.ads
│       ├── domain-value_object-option.ads
│       └── domain-value_object-person.ads
│
├── application/
│   ├── application.ads
│   ├── error/
│   │   └── application-error.ads   # Re-exports Domain.Error
│   ├── command/
│   │   ├── application-command.ads
│   │   └── application-command-greet.ads
│   ├── port/
│   │   ├── application-port.ads
│   │   ├── inbound/
│   │   │   └── application-port-inbound.ads
│   │   └── outbound/
│   │       ├── application-port-outbound.ads
│   │       └── application-port-outbound-writer.ads
│   └── usecase/
│       ├── application-usecase.ads
│       └── application-usecase-greet.ads
│
├── infrastructure/
│   ├── infrastructure.ads
│   └── adapter/
│       ├── infrastructure-adapter.ads
│       └── infrastructure-adapter-console_writer.ads
│
└── api/
    ├── hybrid_lib_ada-api.ads
    ├── hybrid_lib_ada-api.adb
    ├── operations/
    │   ├── hybrid_lib_ada-api-operations.ads
    │   └── hybrid_lib_ada-api-operations.adb
    └── desktop/
        ├── hybrid_lib_ada-api-desktop.ads
        └── hybrid_lib_ada-api-desktop.adb
```

== Package Descriptions

=== Domain Layer

// Sort rows alphabetically by the first column.
#table(
  columns: (auto, 1fr, auto),
  table.header([*Package*], [*Purpose*], [*SPARK*]),
  [`Domain`], [Layer root.], [On],
  [`Domain.Error`], [Error type with kind and bounded message.], [On],
  [`Domain.Error.Result`], [Generic Result[T] monad.], [On],
  [`Domain.Unit`], [Unit type for void-like operations.], [On],
  [`Domain.Value_Object`], [Value object root.], [On],
  [`Domain.Value_Object.Option`], [Option[T] monad.], [On],
  [`Domain.Value_Object.Person`], [Person value object.], [On],
)

=== Application Layer

// Sort rows alphabetically by the first column.
#table(
  columns: (auto, 1fr, auto),
  table.header([*Package*], [*Purpose*], [*SPARK*]),
  [`Application`], [Layer root.], [On],
  [`Application.Command`], [Command root.], [On],
  [`Application.Command.Greet`], [Greeting command DTO.], [On],
  [`Application.Error`], [Re-exports Domain.Error.], [On],
  [`Application.Port`], [Port root.], [On],
  [`Application.Port.Outbound.Writer`], [Writer port definition.], [On],
  [`Application.Usecase.Greet`], [Greeting use case.], [On],
)

=== Infrastructure Layer

// Sort rows alphabetically by the first column.
#table(
  columns: (auto, 1fr, auto),
  table.header([*Package*], [*Purpose*], [*SPARK*]),
  [`Infrastructure`], [Layer root.], [Off],
  [`Infrastructure.Adapter`], [Adapter root.], [Off],
  [`Infrastructure.Adapter.Console_Writer`], [Console output adapter.], [Off],
)

=== API Layer

// Sort rows alphabetically by the first column.
#table(
  columns: (auto, 1fr, auto),
  table.header([*Package*], [*Purpose*], [*SPARK*]),
  [`Hybrid_Lib_Ada`], [Library root.], [Off],
  [`Hybrid_Lib_Ada.API`], [Public facade.], [Off],
  [`Hybrid_Lib_Ada.API.Desktop`], [Desktop composition root.], [Off],
  [`Hybrid_Lib_Ada.API.Operations`], [SPARK-safe operations surface.], [On],
  [`Hybrid_Lib_Ada.Version`], [Version information.], [Off],
)

= Type Definitions

== Domain Types

=== Error_Kind

```ada
type Error_Kind is
  (Validation_Error,
   Parse_Error,
   Not_Found_Error,
   IO_Error,
   Internal_Error);
```

=== Error_Type

```ada
type Error_Type is record
   Kind    : Error_Kind;
   Message : Error_Strings.Bounded_String;  -- Max 512 chars
end record;
```

=== Result (Generic)

```ada
generic
   type T is private;
package Domain.Error.Result.Generic_Result is
   type Result is private;

   function Ok (Value : T) return Result;
   function Error (Kind : Error_Kind; Message : String) return Result;

   function Is_Ok (R : Result) return Boolean;
   function Is_Error (R : Result) return Boolean;
   function Value (R : Result) return T with Pre => Is_Ok (R);
   function Error_Info (R : Result) return Error_Type with Pre => Is_Error (R);
end Generic_Result;
```

=== Person

```ada
type Person is private;

function Create (Name : String) return Person_Result.Result;
function Get_Name (P : Person) return String;
function Is_Valid_Person (P : Person) return Boolean;
```

== Application Types

=== Greet_Command

```ada
type Greet_Command is private;

function Create (Name : String) return Greet_Command;
function Get_Name (Cmd : Greet_Command) return String;
```

=== Writer Port

```ada
generic
   with function Write (Message : String) return Unit_Result.Result;
package Generic_Writer is
   function Write_Message (Message : String) return Unit_Result.Result
   renames Write;
end Generic_Writer;
```

== API Types

All public types are re-exported from `Hybrid_Lib_Ada.API`.

```ada
subtype Person_Type is Domain.Value_Object.Person.Person;
subtype Greet_Command is Application.Command.Greet.Greet_Command;
subtype Error_Type is Domain.Error.Error_Type;
subtype Error_Kind is Domain.Error.Error_Kind;
```

= Design Patterns

== Static Dependency Injection

*HYBRID_LIB_ADA* uses Ada generics for static (compile-time) dependency injection.

```ada
-- 1. Port defines generic signature
package Generic_Writer is ...

-- 2. Use case or operations package is generic, parameterized by Writer
package Application.Usecase.Greet is ...

-- 3. Composition root instantiates with concrete adapter
package Console_Ops is new Hybrid_Lib_Ada.API.Operations
  (Writer => Infrastructure.Adapter.Console_Writer.Write);
```

// Sort rows alphabetically by the first column.
#table(
  columns: (auto, 1fr),
  table.header([*Benefit*], [*Description*]),
  [SPARK compatible], [No runtime dispatching is required.],
  [Testable],
  [Mock writers and alternate composition roots are easy to provide.],

  [Type safe], [The compiler verifies contracts and instantiations.],
  [Zero runtime overhead], [Monomorphization occurs at compile time.],
)

== Three-Package API Pattern

The library profile uses a three-package API pattern:
- `Hybrid_Lib_Ada.API` as the public facade,
- `Hybrid_Lib_Ada.API.Desktop` as the default composition root,
- `Hybrid_Lib_Ada.API.Operations` as the SPARK-safe operations surface.

// Sort rows alphabetically by the first column.
#table(
  columns: (auto, 1fr, auto),
  table.header([*Package*], [*Design Role*], [*SPARK*]),
  [`Hybrid_Lib_Ada.API`],
  [Thin public facade; re-exports public types and delegates to the default composition root.],
  [Off],

  [`Hybrid_Lib_Ada.API.Desktop`],
  [Composition root for desktop/server environments; wires adapters into operations.],
  [Off],

  [`Hybrid_Lib_Ada.API.Operations`],
  [Generic operations surface parameterized by outbound ports; safe verification boundary.],
  [On],
)

== Result Monad Pattern

All fallible operations return `Result[T]` values rather than propagating expected failures as exceptions.

```ada
function Create (Name : String) return Person_Result.Result;
-- Returns Ok(Person) or Error(Validation_Error, "message")

function Greet (Cmd : Greet_Command) return Unit_Result.Result;
-- Returns Ok(Unit) or Error(IO_Error, "message")
```

= Error Handling Strategy

== Error Propagation

Errors flow through use-case orchestration as Result values.

```ada
function Execute (Cmd : Greet_Command) return Unit_Result.Result is
   Person_Res : constant Person_Result.Result := Person.Create (Get_Name (Cmd));
begin
   if Person_Result.Is_Error (Person_Res) then
      return Unit_Result.Error (...);
   end if;

   return Writer (Format_Greeting (Person_Result.Value (Person_Res)));
end Execute;
```

== No Exceptions Policy

// Sort rows alphabetically by the first column.
#table(
  columns: (auto, 1fr),
  table.header([*Situation*], [*Handling*]),
  [I/O failure], [Return Error result.],
  [Programmer error in core layers],
  [Fail loudly; treat as a bug to fix rather than a normal runtime condition.],

  [Unexpected error at infrastructure boundary],
  [Convert to `Internal_Error` result at the boundary.],

  [Validation failure], [Return Error result.],
)

== Exception Boundary Specification

This project uses a layered exception-boundary policy.

// Sort rows alphabetically by the first column.
#table(
  columns: (auto, auto, auto, 1fr),
  table.header(
    [*Layer*], [*Functional.Try*], [*Manual `exception`*], [*Rationale*]
  ),
  [API],
  [N/A],
  [Forbidden],
  [Public facade; returns Result values to consumers.],

  [Application],
  [N/A],
  [Forbidden],
  [Use-case orchestration; works with Result values only.],

  [Domain],
  [N/A],
  [Forbidden],
  [Pure core logic; works with Result values only.],

  [Infrastructure],
  [Required at external boundaries],
  [Forbidden],
  [Must convert external exceptions to Result values consistently.],

  [Test],
  [Optional],
  [Allowed],
  [Tests may deliberately verify exception scenarios.],
)

=== Required Pattern: Functional.Try at Boundaries

All Infrastructure adapters that perform I/O or call external APIs shall use `Functional.Try.Map_To_Result` or `Functional.Try.Map_To_Result_With_Param`.

```ada
with Functional.Try.Map_To_Result_With_Param;

package body Infrastructure.Adapter.Console_Writer is
   -- Inner action may raise exceptions
   function Write_Action (Message : String) return Unit_Result.Result is
   begin
      Ada.Text_IO.Put_Line (Message);
      return Unit_Result.Ok (Unit_Value);
   end Write_Action;

   -- Error factory converts exception mapping into domain error
   function Make_Error (Kind : Error_Kind; Message : String)
     return Unit_Result.Result is
   begin
      return Unit_Result.Error (Kind, Message);
   end Make_Error;

   package Try_Write is new Functional.Try.Map_To_Result_With_Param (...);
   Mappings : constant Try_Write.Mapping_Array := [...];

   function Write (Message : String) return Unit_Result.Result is
   begin
      return Try_Write.Run (Message, Mappings);
   end Write;
end Infrastructure.Adapter.Console_Writer;
```

=== Prohibited Pattern: Manual Exception Handlers

Manual `exception` handlers in infrastructure adapters are forbidden because they create inconsistent error mapping, weaken auditability, and separate exception conversion from the shared policy.

=== Validation Enforcement

Release preparation and developer workflow validate these rules:
1. Infrastructure code shall use `Functional.Try` where boundary I/O exists.
2. Infrastructure code shall not contain manual `exception` handlers.
3. Domain/Application/API code shall not use the `exception` keyword.
4. Test code is exempt.

= Build Configuration

== GPR Projects

// Sort rows alphabetically by the first column.
#table(
  columns: (auto, 1fr),
  table.header([*Project*], [*Purpose*]),
  [`hybrid_lib_ada.gpr`],
  [Public library with `Library_Interface` restrictions.],

  [`hybrid_lib_ada_internal.gpr`],
  [Internal unrestricted project used for tests and examples.],
)

== Build Profiles

// Sort rows alphabetically by the first column.
#table(
  columns: (auto, auto, 1fr),
  table.header([*Profile*], [*Target*], [*Features*]),
  [baremetal], [Minimal runtime], [Zero-footprint style profile.],
  [embedded], [Ravenscar embedded], [Tasking-safe embedded profile.],
  [standard], [Desktop/server], [Full feature set.],
)

== Library_Standalone Design Decision

hybrid_lib_ada uses:

```gpr
Library_Standalone => "standard";
```

*Rationale:*

- hybrid_lib_ada is intended for composition within Ada toolchain
  ecosystems where the consumer supplies the Ada runtime at link time.
- Using `"encapsulated"` would bundle the full Ada runtime (RTS) into
  `libhybrid_lib_ada.a`. When two encapsulated stand-alone libraries
  (ESALs) are linked into the same executable, the linker emits
  thousands of `multiple definition` errors for the duplicated runtime
  symbols. This is a fundamental composition limitation of
  `"encapsulated"`, not a fixable defect.
- Public-API enforcement is achieved via `Library_Interface`, not via
  `Library_Standalone`. `Library_Interface` lists the packages clients
  may `with`; GPRbuild rejects any `with` clause naming an unlisted
  package. `Library_Standalone` is purely a packaging directive.

*Therefore:*

- `"standard"` is required for composability with other Ada libraries
  that may themselves be stand-alone.
- `"encapsulated"` is explicitly prohibited for this project.
- The architecture rule is enforced by
  `scripts/python/shared/arch_guard/adapters/ada.py`
  (`ROOT_GPR_ENCAPSULATED_ERROR`) on every `make check-arch`.

*Note:* `"encapsulated"` may be appropriate for standalone distribution
to non-Ada environments (e.g., a C-callable library where the consumer
has no Ada runtime), but this is not a requirement for hybrid_lib_ada.

= Design Decisions

== API.Operations as Child vs Sibling

*Decision:* use `API.Operations` as a child package instead of a sibling package.

*Rationale:* the child-package hierarchy is more idiomatic Ada, preserves a clean public API structure, and supports the design without losing SPARK value.

== No Heap Allocation

*Decision:* use bounded strings and stack-oriented allocation strategies.

*Rationale:* this supports embedded compatibility, deterministic behavior, and SPARK-friendly design constraints.

== Static vs Dynamic Polymorphism

*Decision:* use static polymorphism via generics.

*Rationale:* this supports SPARK compatibility, zero runtime overhead, and compile-time type safety.
