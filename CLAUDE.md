# CLAUDE.md - Project-Specific Instructions for hybrid_lib_ada

**Project**: hybrid_lib_ada - Ada 2022 Library Template (Hybrid DDD/Clean/Hex)
**Owner**: Michael Gardner, A Bit of Help, Inc.
**Last Updated**: 2026-02-14

---

## FIRST: Read the Reorientation Guide

**After every compaction or session start**, read `docs/REORIENTATION.md` (if it exists) for current project context, active work items, and key file locations.

---

## Template Project

This is a **template project** used to scaffold new Ada 2022 libraries. Projects cloned from this template are customized using `scripts/python/shared/brand_project.py`.

Changes made here affect all future projects cloned from this template. Consider impact carefully.

---

## Architecture Rules

This project follows hexagonal (DDD/Clean/Hex) architecture:
- **API** -> **Application** -> **Infrastructure** -> **Domain** layering
- Domain has ZERO external dependencies
- `Functional.Try` usage at ALL infrastructure boundaries (see global CLAUDE.md)
- API layer is the public facade (CAN access Domain directly)
- Input Ports (Application) called by API
- Output Ports (Application) implemented by Infrastructure

---

## Debugging

**Prefer GDB over println-style debugging.**

When investigating issues, use GDB as your first choice rather than adding `Put_Line` statements with repetitive build cycles. GDB requires minimal setup time and typically resolves issues faster than divide-and-conquer println strategies.

**GDB advantages:**
- Set breakpoints without modifying source code
- Inspect variables at runtime (values, types, memory)
- Step through exact execution paths
- No recompilation needed to adjust debug points

**Basic GDB workflow:**
```bash
# Build with debug symbols (alr builds with -g by default)
alr build

# Start GDB with a test runner
gdb ./test/bin/unit_runner

# Set breakpoint and run
(gdb) break Domain.Value_Object.Person.Create
(gdb) run

# Inspect variables
(gdb) print Result
```

---

## Testing

```bash
# Run all tests
make test

# Run unit tests only
cd test/unit && alr build && ../bin/unit_runner

# Run integration tests
cd test/integration && alr build && ../bin/integration_runner
```

Note: Libraries do not have E2E tests. E2E testing is the responsibility of consuming application projects.

---

**Copyright**: (c) 2026 Michael Gardner, A Bit of Help, Inc.
**License**: BSD-3-Clause
