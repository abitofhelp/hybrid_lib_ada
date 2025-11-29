# Changelog

**Version:** 1.0.0-dev   
**Date:** 2025-11-29    
**SPDX-License-Identifier:** BSD-3-Clause  
**License File:** See the LICENSE file in the project root  
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Released  

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Initial library structure based on hybrid_app_ada
- 4-layer hexagonal architecture (Domain, Application, Infrastructure, API)
- Public API facade at `Hybrid_Lib_Ada.API`
- Generic I/O plugin pattern via `Hybrid_Lib_Ada.API.Operations`
- Desktop platform instantiation via `Hybrid_Lib_Ada.API.Desktop`
- Embedded safety restrictions in root package
- Result monad error handling via `functional` crate
- Person value object with bounded string validation
- Greet use case with dependency injection via generics
- Console writer adapter for desktop platforms
- Version package for runtime version queries
- Comprehensive Makefile for build automation

### Architecture
- Domain layer: Pure business logic with zero dependencies
- Application layer: Use cases, ports, commands
- Infrastructure layer: Adapters (Console_Writer)
- API layer: Public facade with platform-specific instantiations
- Library_Standalone mode with explicit Library_Interface
- Static dispatch via generics for zero-overhead DI

### Dependencies
- functional ^2.1.0 (Result/Option/Try monads)

## [1.0.0-dev] - 2025-11-28

### Added
- Initial development release
- Project scaffolding from hybrid_app_ada
- Removed presentation and bootstrap layers (app-only)
- Added API layer for library public interface
