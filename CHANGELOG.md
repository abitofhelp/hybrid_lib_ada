# Changelog

**Version:** 2.0.0  
**Date:** December 10, 2025  
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>  
**Status:** Released  

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed

- **`Library_Standalone` flipped from `"encapsulated"` to `"standard"`**
  in `hybrid_lib_ada.gpr`. `"encapsulated"` bundled the Ada runtime into
  `libhybrid_lib_ada.a`, causing duplicate-runtime `multiple definition`
  link errors when composed with any other encapsulated SAL.
  `"standard"` uses the system Ada runtime at link time, which is the
  correct model for Ada-toolchain ecosystems. Public-API enforcement
  is unchanged — `Library_Interface` (the actual enforcement
  mechanism) is untouched.
- `hybrid_lib_ada.gpr` comment block rewritten to credit
  `Library_Interface` for public-API enforcement (previous text
  misleadingly attributed it to `Library_Standalone`).
- SDS bumped v2.0.1 → v2.0.2 (patch) with new "Build Configuration /
  Library_Standalone Design Decision" section documenting the
  rationale and the arch_guard enforcement boundary.

### Fixed

- Resolved an ESAL composition limitation that prevented hybrid_lib_ada
  from being linked alongside any other encapsulated stand-alone Ada
  library. The template-regeneration vector that re-introduced
  `"encapsulated"` on library roots was closed upstream in
  `hybrid_scripts_python` PR #6 (arch_guard now enforces
  `Library_Standalone = "standard"` on root library GPRs;
  `namespace_layers.py` no longer auto-upgrades to `"encapsulated"`).

### Migration

- No API changes.
- Downstream consumers: nothing to do if already using `"standard"`.
  If consuming hybrid_lib_ada as a submodule, `git submodule update
  --remote`; if via Alire, `alr update`; then rebuild.

## [2.0.0] - 2025-12-10

**Test Coverage:** 99 unit + 10 integration + 0 examples = 109 total

### Breaking Changes
- **Error_Kind enum refactored** for better error categorization:
  - Added `Parse_Error` for malformed data and parsing failures
  - Added `Not_Found_Error` for missing resources (files, records, etc.)
  - Replaced `System_Error` and `Unknown_Error` with `Internal_Error` for bugs and invariant violations
- **functional dependency upgraded** from ^2.2.1 to ^3.0.0
- **Removed `Expect` function** from Domain.Error.Result - violated architecture rules by raising Program_Error

### Added
- **Example programs** in `examples/` directory:
  - `basic_greeting` - Simple library usage demonstration
  - `error_handling` - Result monad error handling patterns
- **Windows CI workflow** (`windows-release.yml`) for automated Windows testing
- **6 build profiles** documented: standard, concurrent, stm32mp135_linux, embedded, stm32h7s78, baremetal

### Changed
- Domain.Error and Application.Error updated with new Error_Kind values
- Build profile switched to development with debug settings
- Compiler switches updated for development: -Og, -g, -gnatwa, -gnatVa
- Documentation completely regenerated with correct architecture diagrams
- Fixed layer terminology in comments ("API" instead of "Presentation" for library)

### Fixed
- Architecture documentation corrected: API facade depends on Application + Domain ONLY (not Infrastructure)
- API.Desktop correctly documented as composition root (can depend on all layers)

## [1.0.0] - 2025-12-02

### Added
- Initial library structure based on hybrid_app_ada
- 4-layer hexagonal architecture (Domain, Application, Infrastructure, API)
- Public API facade at `Hybrid_Lib_Ada.API`
- Three-package API pattern (Operations, Desktop, facade)
- Generic I/O plugin pattern via `Hybrid_Lib_Ada.API.Operations`
- Desktop platform instantiation via `Hybrid_Lib_Ada.API.Desktop`
- Embedded safety restrictions in root package
- Result monad error handling via `functional` crate
- Person value object with bounded string validation
- Greet use case with dependency injection via generics
- Console writer adapter for desktop platforms
- Version package for runtime version queries
- Comprehensive Makefile for build automation
- 98 tests (88 unit + 10 integration)
- Comprehensive documentation (SRS, SDS, STG)
- 6 UML diagrams with SVG output
- Build profile support (standard, embedded, baremetal, STM32)

### Architecture
- Domain layer: Pure business logic with zero dependencies
- Application layer: Use cases, ports, commands
- Infrastructure layer: Adapters (Console_Writer)
- API layer: Public facade with platform-specific instantiations
- Library_Standalone mode with explicit Library_Interface
- Static dispatch via generics for zero-overhead DI
- SPARK_Mode boundaries defined for future formal verification
- Submodule `export-ignore` for clean Alire deployment

### Dependencies
- functional ^2.2.1 (Result/Option/Try monads)
