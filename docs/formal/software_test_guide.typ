// ============================================================================
// File: software_test_guide.typ
// Purpose: Software Test Guide for HYBRID_LIB_ADA.
// Scope: Real project test strategy, organization, execution, traceability,
//   examples, and maintenance guidance for the library profile.
// Usage: This is an authoritative Typst source document. The generated PDF is
//   the distribution artifact.
// Modification Policy:
//   - Edit this file for project-specific STG content.
//   - Keep shared presentation logic in core.typ.
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
  title: "Software Test Guide",
  project_name: "HYBRID_LIB_ADA",
  authors: ("Michael Gardner",),
  version: "2.1.0",
  applies_to: "^2.0",
  status: "Released",
  status_date: "2026-04-26",
  spdx_license: "BSD-3-Clause",
  license_file: "See the LICENSE file in the project root",
  copyright: "© 2026 Michael Gardner, A Bit of Help, Inc.",
)

#let profile = (
  variant: "library",
  library_role: "enterprise",
  app_role: none,
  assurance: "spark-targeted",
  execution: "sequential",
  parallelism: "none",
  platform: ("desktop", "server", "embedded"),
  execution_environment: ("linux", "macos", "windows", "ada-runtime"),
  processor_architecture: ("amd64", "arm64", "arm32"),
  deployment: "native",
)

#let change_history = (
  (
    version: "2.1.0",
    date: "2025-12-14",
    author: "Michael Gardner",
    changes: "Remove hardcoded metrics per documentation standards; metrics now in CHANGELOG.",
  ),
  (
    version: "2.0.0",
    date: "2025-12-09",
    author: "Michael Gardner",
    changes: "Complete regeneration for v2.0.0; added Example Programs section.",
  ),
  (
    version: "1.0.0",
    date: "2025-11-29",
    author: "Michael Gardner",
    changes: "Initial release.",
  ),
)

#show: formal_doc.with(doc, profile, change_history)

#set heading(numbering: "1.1.")

// Supporting architecture guidance is maintained in docs/guides/.
// Supporting UML diagrams are maintained in docs/diagrams/.

= Introduction

== Purpose

This Software Test Guide (STG) describes the testing strategy, test structure, execution procedures, and maintenance guidance for *HYBRID_LIB_ADA*.

== Scope

This document covers:
- test architecture and organization
- unit and integration test suites
- test execution procedures
- shared test framework usage
- adding new tests
- runnable example programs

== References

- Software Requirements Specification (SRS)
- Software Design Specification (SDS)
- `docs/guides/all_about_our_api.md`

= Test Strategy

== Test Categories

// Sort rows alphabetically by the first column.
#table(
  columns: (auto, auto, 1fr),
  table.header([Category], [Location], [Purpose]),
  [Example Programs], [`examples/`], [Runnable demonstrations of library usage.],
  [Integration Tests], [`test/integration/`], [Test cross-layer interactions.],
  [Unit Tests], [`test/unit/`], [Test individual packages in isolation.],
)

See CHANGELOG for current test counts per release.

== Testing Philosophy

- *Result Monad Testing:* all error paths are tested via Result inspection.
- *Mock-Based Isolation:* infrastructure is mocked for unit tests where appropriate.
- *No Exceptions for Expected Failures:* tests verify Result-based expected failure handling.
- *Deterministic:* same inputs produce the same observable results.
- *Public API Coverage:* all public APIs should be covered by tests.

= Test Organization

== Directory Structure

```text
test/
├── bin/                          # Compiled test executables
│   ├── unit_runner
│   ├── integration_runner
│   └── test_*.adb executables
│
├── common/                       # Shared test infrastructure
│   ├── test_framework.ads
│   └── test_framework.adb
│
├── unit/                         # Unit test sources
│   ├── unit_tests.gpr
│   ├── unit_runner.adb
│   ├── test_domain_error_result.adb
│   ├── test_domain_person.adb
│   ├── test_application_command_greet.adb
│   ├── test_application_usecase_greet.adb
│   └── test_api_operations.adb
│
├── integration/                  # Integration test sources
│   ├── integration_tests.gpr
│   ├── integration_runner.adb
│   └── test_api_greet.adb
│
└── python/                       # Python-based tests
    └── test_arch_guard_ada.py
```

== Naming Conventions

// Sort rows alphabetically by the first column.
#table(
  columns: (auto, auto, 1fr),
  table.header([Element], [Convention], [Example]),
  [Mock prefix], [`Mock_`], [`Mock_Writer_Success`],
  [Runner], [`<category>_runner.adb`], [`unit_runner.adb`],
  [Test file], [`test_<layer>_<package>.adb`], [`test_domain_person.adb`],
  [Test name], [Descriptive, action-result], [`"Create valid name - Is_Ok"`],
)

== GPR Projects

Tests use `hybrid_lib_ada_internal.gpr`, which provides unrestricted access to project packages for testing without `Library_Interface` restrictions.

= Test Framework

== Framework Overview

The shared test framework in `test/common/test_framework.ads` provides:
- test result tracking
- category summaries
- grand total reporting
- color-coded output

== Framework API

```ada
package Test_Framework is

   procedure Register_Results (Total : Natural; Passed : Natural);

   function Grand_Total_Tests return Natural;
   function Grand_Total_Passed return Natural;

   procedure Reset;

   function Print_Category_Summary
     (Category_Name : String;
      Total         : Natural;
      Passed        : Natural) return Integer;

end Test_Framework;
```

== Usage Pattern

```ada
procedure Test_My_Package is
   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;

   procedure Run_Test (Name : String; Passed : Boolean) is
   begin
      Total_Tests := Total_Tests + 1;
      if Passed then
         Passed_Tests := Passed_Tests + 1;
      end if;
   end Run_Test;

begin
   --  Test cases here
   Run_Test ("Feature works", Condition);

   Test_Framework.Register_Results (Total_Tests, Passed_Tests);
end Test_My_Package;
```

= Test Execution

== Running All Tests

```bash
make test-all
```

== Running Specific Suites

```bash
make test-unit
make test-integration
```

== Expected Output

```text
========================================
     HYBRID_LIB_ADA UNIT TEST SUITE
========================================

[PASS] ...
...
```

*Note:* current exact counts are maintained in CHANGELOG.

= Test Details

== Unit Tests

=== Domain Layer Tests

- `test_domain_error_result.adb` — tests `Domain.Error.Result`
- `test_domain_person.adb` — tests `Domain.Value_Object.Person`
- `test_domain_option.adb` — tests `Domain.Value_Object.Option`

=== Application Layer Tests

- `test_application_command_greet.adb` — tests `Application.Command.Greet`
- `test_application_usecase_greet.adb` — tests `Application.Usecase.Greet` with mock writer isolation

=== API Layer Tests

- `test_api_operations.adb` — tests `Hybrid_Lib_Ada.API.Operations`
- verifies SPARK-safe generic instantiation

== Integration Tests

- `test_api_greet.adb` — tests full stack through `Hybrid_Lib_Ada.API`
- uses the real console writer adapter
- verifies complete workflow end-to-end

== Architecture Validation Tests

`test/python/test_arch_guard_ada.py` validates direct dependency rules for the library architecture profile. This validation is run during normal developer workflow and again in CI/CD.

= Writing New Tests

== Test Template

```ada
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Test_Framework;
with My.Package;

procedure Test_My_Package is
   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;

   procedure Run_Test (Name : String; Passed : Boolean) is
   begin
      Total_Tests := Total_Tests + 1;
      if Passed then
         Passed_Tests := Passed_Tests + 1;
      end if;
   end Run_Test;

begin
   Run_Test ("Feature works", My.Package.Feature = Expected);
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);
end Test_My_Package;
```

== Adding to GPR

Update the relevant test project file to include the new test executable in `for Main use (...)`.

== Mock Patterns

Use simple, resettable mock state for outbound-port behavior:
- captured message
- call count
- configured success/failure mode

= Traceability

== Requirements to Tests

#table(
  columns: (auto, 1fr),
  table.header([Requirement], [Test File]),
  [FR-01.1 (Person)], [`test_domain_person.adb`],
  [FR-01.4 (Error)], [`test_domain_error_result.adb`],
  [FR-02.2 (Use Case)], [`test_application_usecase_greet.adb`],
  [FR-04.4 (Operations)], [`test_api_operations.adb`],
  [FR-04.1 (Facade)], [`test_api_greet.adb`],
)

== Layer Coverage

// Sort rows alphabetically by the first column.
#table(
  columns: (auto, 1fr),
  table.header([Layer], [Test Files]),
  [API], [`test_api_*.adb`],
  [Application], [`test_application_*.adb`],
  [Domain], [`test_domain_*.adb`],
)

See CHANGELOG for current counts per layer and total.

= Example Programs

== Basic Greeting

- file: `examples/basic_greeting.adb`
- purpose: demonstrate the simplest library usage path
- build: `alr exec -- gprbuild -P examples/examples.gpr`
- run: `./examples/bin/basic_greeting`

== Error Handling

- file: `examples/error_handling.adb`
- purpose: demonstrate Result-based validation and error handling
- run: `./examples/bin/error_handling`

== Building Examples

Examples are built via `examples/examples.gpr`.

= Test Maintenance

== When to Update

- new package added → add corresponding test file
- API changed → update affected tests
- bug fixed → add regression test
- error handling changed → update error-path tests

== Quality Guidelines

- test names must be descriptive
- one assertion per test case is preferred
- mock state is reset between tests
- tests shall not depend on one another

== Continuous Integration

Tests run automatically on:
- push to main branch
- pull request creation
- manual workflow dispatch

== Exit Codes

#table(
  columns: (auto, 1fr),
  table.header([Code], [Meaning]),
  [0], [All tests passed],
  [1], [One or more tests failed],
)
