# Software Test Guide

**Version:** 1.0.0<br>
**Date:** 2025-11-29<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## 1. Introduction

### 1.1 Purpose

This Software Test Guide (STG) describes the testing strategy, test structure, and execution procedures for **Hybrid_Lib_Ada**.

### 1.2 Scope

This document covers:
- Test architecture and organization
- Unit and integration test suites
- Test execution procedures
- Test framework usage
- Adding new tests

### 1.3 References

- Software Requirements Specification (SRS)
- Software Design Specification (SDS)
- [All About Our API](../guides/all_about_our_api.md)

---

## 2. Test Architecture

### 2.1 Test Categories

| Category | Location | Purpose | Count |
|----------|----------|---------|-------|
| Unit Tests | `test/unit/` | Test individual packages in isolation | 88 |
| Integration Tests | `test/integration/` | Test cross-layer interactions | 10 |
| **Total** | | | **98** |

### 2.2 Directory Structure

```
test/
├── bin/                          # Compiled test executables
│   ├── unit_runner
│   ├── integration_runner
│   └── test_*.adb executables
│
├── common/                       # Shared test infrastructure
│   ├── test_framework.ads        # Result tracking, summaries
│   └── test_framework.adb
│
├── unit/                         # Unit test sources
│   ├── unit_tests.gpr            # GPR project
│   ├── unit_runner.adb           # Main test runner
│   ├── test_domain_error_result.adb
│   ├── test_domain_person.adb
│   ├── test_application_command_greet.adb
│   ├── test_application_usecase_greet.adb
│   └── test_api_operations.adb
│
├── integration/                  # Integration test sources
│   ├── integration_tests.gpr     # GPR project
│   ├── integration_runner.adb    # Main test runner
│   └── test_api_greet.adb
│
└── python/                       # Python-based tests (arch_guard)
    └── test_arch_guard_ada.py
```

### 2.3 Test GPR Projects

Tests use `hybrid_lib_ada_internal.gpr` which provides unrestricted access to all packages (no Library_Interface restrictions):

```ada
--  test/unit/unit_tests.gpr

with "../../hybrid_lib_ada_internal.gpr";

project Unit_Tests is
   for Source_Dirs use (".", "../common");
   for Object_Dir use "../../obj/test/unit";
   for Exec_Dir use "../bin";

   for Main use
     ("unit_runner.adb",
      "test_domain_error_result.adb",
      ...);

   package Compiler is
      for Default_Switches ("Ada") use
         Hybrid_Lib_Ada_Internal.Compiler'Default_Switches("Ada");
   end Compiler;
end Unit_Tests;
```

---

## 3. Unit Tests

### 3.1 Domain Layer Tests

#### 3.1.1 test_domain_error_result.adb

**Package Under Test:** `Domain.Error.Result`

| Test | Description |
|------|-------------|
| Ok construction - Is_Ok returns true | Verify Ok result returns true for Is_Ok |
| Ok construction - Is_Error returns false | Verify Ok result returns false for Is_Error |
| Ok value extraction - correct value | Verify Value returns the wrapped value |
| Error construction - Is_Error returns true | Verify Error result returns true for Is_Error |
| Error info - correct kind | Verify Error_Info returns correct Error_Kind |
| Error info - correct message | Verify Error_Info returns correct message |
| Boolean Result - correct value | Verify Result[Boolean] works correctly |
| Multiple Ok values | Verify multiple Result instances are independent |
| Multiple errors | Verify multiple Error results are independent |
| Long error message | Verify long messages are stored correctly |

**Total:** 19 tests

#### 3.1.2 test_domain_person.adb

**Package Under Test:** `Domain.Value_Object.Person`

| Test | Description |
|------|-------------|
| Create valid name - Is_Ok | Valid name creates Ok result |
| Create valid name - Get_Name correct | Name round-trips correctly |
| Create empty name - Is_Error | Empty name returns Error |
| Create empty name - Validation_Error | Error kind is Validation_Error |
| Create name too long - Is_Error | Overlong name returns Error |
| Create name at max length - Is_Ok | Max length name succeeds |
| Create single char name - Is_Ok | Single character name succeeds |
| Create name with spaces - Is_Ok | Names with spaces succeed |
| Create name with special chars | Special characters preserved |
| Create with Unicode | Unicode characters preserved |
| Is_Valid_Person | Validation function works |
| Multiple instances | Instances are independent |

**Total:** 22 tests

### 3.2 Application Layer Tests

#### 3.2.1 test_application_command_greet.adb

**Package Under Test:** `Application.Command.Greet`

| Test | Description |
|------|-------------|
| Create simple name | Name stored correctly |
| Create name with spaces | Spaces preserved |
| Create single char | Single character works |
| Create max length name | Max length handled |
| Create with special chars | Special characters preserved |
| Create with Unicode | Unicode preserved |
| Multiple commands | Commands are independent |
| Round-trip test | Create → Get_Name preserves value |

**Total:** 13 tests

#### 3.2.2 test_application_usecase_greet.adb

**Package Under Test:** `Application.Usecase.Greet`

Uses **mock writer** for isolation:
- `Mock_Writer_Success` - Always succeeds, captures message
- `Mock_Writer_Failure` - Always returns IO_Error

| Test | Description |
|------|-------------|
| Execute valid name - Is_Ok | Valid name succeeds |
| Execute valid name - message written | Correct message output |
| Execute different names | Multiple names work |
| Execute with writer failure | Writer error propagates |
| Execute with special chars | Special characters preserved |
| Execute with Unicode | Unicode preserved |
| Execute with max length | Max length handled |
| Multiple executions | State resets between calls |

**Total:** 20 tests

### 3.3 API Layer Tests

#### 3.3.1 test_api_operations.adb

**Package Under Test:** `Hybrid_Lib_Ada.API.Operations`

Uses **mock writer** for isolation, testing the SPARK-safe generic:

| Test | Description |
|------|-------------|
| Greet 'Alice' - Is_Ok | Valid name succeeds |
| Greet 'Alice' - Writer called once | Writer invoked exactly once |
| Greet 'Alice' - Message correct | Output is "Hello, Alice!" |
| Greet 'Bob' - correct message | Different names work |
| Greet 'Jane Doe' - spaces | Names with spaces work |
| Greet special chars | Special characters preserved |
| Greet with failing writer | Writer error propagates |
| Greet with failing writer - IO_Error | Error kind correct |
| Multiple calls | Call count tracked |
| Multiple calls - last message | Latest message captured |

**Total:** 14 tests

---

## 4. Integration Tests

### 4.1 test_api_greet.adb

**Packages Under Test:** Full stack through `Hybrid_Lib_Ada.API`

Tests the complete flow: API facade → API.Desktop → API.Operations → Application.Usecase → Domain, with real Console_Writer adapter.

| Test | Description |
|------|-------------|
| API.Greet valid name - Is_Ok | Full stack succeeds |
| API.Greet 'Bob' - Is_Ok | Different name works |
| API.Greet 'Jane Doe' - Is_Ok | Spaces preserved |
| API.Greet special chars - Is_Ok | Special characters work |
| API.Greet Unicode - Is_Ok | Unicode preserved |
| Create_Greet_Command round-trip | Command creation works |
| Create_Person valid - Is_Ok | Person creation works |
| Create_Person round-trip | Name round-trips |
| Create_Person empty - Is_Error | Validation fails for empty |
| Create_Person empty - Validation_Error | Correct error kind |

**Total:** 10 tests

---

## 5. Test Framework

### 5.1 Test_Framework Package

The shared test framework provides:

```ada
package Test_Framework is

   --  Track grand totals across all test suites
   procedure Register_Results (Total : Natural; Passed : Natural);

   --  Get cumulative results
   function Grand_Total_Tests return Natural;
   function Grand_Total_Passed return Natural;

   --  Reset counters (for test runner)
   procedure Reset;

   --  Print color-coded category summary
   function Print_Category_Summary
     (Category_Name : String;
      Total         : Natural;
      Passed        : Natural) return Integer;  --  0=success, 1=failure

end Test_Framework;
```

### 5.2 Test Pattern

Each test file follows this pattern:

```ada
procedure Test_My_Package is
   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;

   procedure Run_Test (Name : String; Passed : Boolean) is
   begin
      Total_Tests := Total_Tests + 1;
      if Passed then
         Passed_Tests := Passed_Tests + 1;
         Put_Line ("[PASS] " & Name);
      else
         Put_Line ("[FAIL] " & Name);
      end if;
   end Run_Test;

begin
   Put_Line ("========================================");
   Put_Line ("Testing: My.Package");
   Put_Line ("========================================");

   --  Test cases
   Run_Test ("Test name", Condition);
   ...

   --  Register with framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);
end Test_My_Package;
```

### 5.3 Mock Writer Pattern

For testing with mock infrastructure:

```ada
--  State for mock
Captured_Message : Unbounded_String;
Write_Call_Count : Natural := 0;
Mock_Should_Fail : Boolean := False;

function Mock_Writer (Message : String) return Unit_Result.Result is
begin
   Write_Call_Count := Write_Call_Count + 1;

   if Mock_Should_Fail then
      return Unit_Result.Error
        (Kind    => IO_Error,
         Message => "Mock failure");
   end if;

   Captured_Message := To_Unbounded_String (Message);
   return Unit_Result.Ok (Unit_Value);
end Mock_Writer;

procedure Reset_Mock is
begin
   Captured_Message := Null_Unbounded_String;
   Write_Call_Count := 0;
   Mock_Should_Fail := False;
end Reset_Mock;

--  Instantiate with mock
package Test_Ops is new Application.Usecase.Greet
  (Writer => Mock_Writer);
```

---

## 6. Test Execution

### 6.1 Build Tests

```bash
# Build all tests
make build-tests

# Or manually:
alr exec -- gprbuild -P test/unit/unit_tests.gpr
alr exec -- gprbuild -P test/integration/integration_tests.gpr
```

### 6.2 Run All Tests

```bash
# Run all tests via Make
make test-all

# Or run individually:
./test/bin/unit_runner
./test/bin/integration_runner
```

### 6.3 Run Individual Test Files

```bash
# Run specific test
./test/bin/test_domain_person
./test/bin/test_api_operations
```

### 6.4 Expected Output

```
========================================
     HYBRID_LIB_ADA UNIT TEST SUITE
========================================

========================================
Testing: Domain.Error.Result
========================================

[PASS] Ok construction - Is_Ok returns true
[PASS] Ok construction - Is_Error returns false
...

========================================
        GRAND TOTAL - ALL UNIT TESTS
========================================
Total tests:   88
Passed:        88
Failed:        0

########################################
###                                  ###
###    UNIT TESTS: SUCCESS           ###
###    All  88 tests passed!         ###
###                                  ###
########################################
```

### 6.5 Exit Codes

| Code | Meaning |
|------|---------|
| 0 | All tests passed |
| 1 | One or more tests failed |

---

## 7. Adding New Tests

### 7.1 Adding a Unit Test

1. **Create test file:**

```bash
touch test/unit/test_my_package.adb
```

2. **Add to GPR project:**

```ada
--  test/unit/unit_tests.gpr
for Main use
  ("unit_runner.adb",
   ...
   "test_my_package.adb");  -- Add here
```

3. **Add to runner:**

```ada
--  test/unit/unit_runner.adb

with Test_My_Package;  -- Add import

...

Test_My_Package;  -- Add call
```

4. **Implement test:**

```ada
procedure Test_My_Package is
   ...
begin
   Run_Test ("My test", Expected = Actual);
   Test_Framework.Register_Results (Total, Passed);
end Test_My_Package;
```

5. **Build and run:**

```bash
alr exec -- gprbuild -P test/unit/unit_tests.gpr
./test/bin/unit_runner
```

### 7.2 Adding an Integration Test

Same process but use:
- `test/integration/` directory
- `integration_tests.gpr` project
- `integration_runner.adb` runner

### 7.3 Testing with Custom Writers

See [All About Our API - Testing with Mock Composition Root](../guides/all_about_our_api.md#testing-with-mock-composition-root) for detailed examples.

---

## 8. Traceability

### 8.1 Requirements to Tests

| Requirement | Test File | Tests |
|-------------|-----------|-------|
| REQ-DOM-001 (Person) | test_domain_person.adb | 22 |
| REQ-DOM-002 (Error) | test_domain_error_result.adb | 19 |
| REQ-DOM-003 (Result) | test_domain_error_result.adb | 19 |
| REQ-APP-001 (Command) | test_application_command_greet.adb | 13 |
| REQ-APP-002 (Use Case) | test_application_usecase_greet.adb | 20 |
| REQ-APP-003 (Port) | test_application_usecase_greet.adb | 20 |
| REQ-INF-001 (Adapter) | test_api_greet.adb | 10 |
| REQ-API-001 (Facade) | test_api_greet.adb | 10 |
| REQ-API-002 (Operations) | test_api_operations.adb | 14 |
| REQ-API-003 (Composition) | test_api_greet.adb | 10 |

### 8.2 Layer Coverage

| Layer | Test Files | Tests |
|-------|-----------|-------|
| Domain | test_domain_*.adb | 41 |
| Application | test_application_*.adb | 33 |
| API | test_api_*.adb | 24 |
| **Total** | | **98** |

---

## 9. Appendices

### A. Test Naming Conventions

| Element | Convention | Example |
|---------|------------|---------|
| Test file | `test_<package>.adb` | `test_domain_person.adb` |
| Test name | Descriptive, action-result | "Create valid name - Is_Ok" |
| Mock prefix | `Mock_` | `Mock_Writer_Success` |
| Runner | `<category>_runner.adb` | `unit_runner.adb` |

### B. Change History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2025-11-29 | Michael Gardner | Initial release |
