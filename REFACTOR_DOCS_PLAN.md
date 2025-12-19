# Documentation Refactoring Plan

**Created:** 2025-12-18
**Status:** Pending (paused for adafmt work)
**Author:** Michael Gardner

---

## Overview

This plan consolidates documentation standards across all Ada projects. The REF projects (hybrid_lib_ada, hybrid_app_ada) serve as authoritative templates that other projects clone from.

---

## Key Principles

1. **All fields REQUIRED** - If not applicable, content indicates N/A/Unused
2. **README as index** - All fields present, linking to primary sources
3. **Zero redundancy** - Each piece of information lives in exactly ONE place
4. **Metrics in CHANGELOG** - Test counts, SPARK stats, dependencies tied to releases
5. **REF = maximal template** - Both REF projects have identical structure

---

## README.md Structure (22 Required Fields)

| # | Section | Content Type | Primary Location |
|---|---------|--------------|------------------|
| 1 | Title + Badges | Content | README |
| 2 | Metadata Header | Content | README |
| 3 | Special Notices | Content (N/A if none) | README |
| 4 | Overview | Content | README |
| 5 | Features | Content (checklist) | README |
| 6 | SPARK Formal Verification | Badge + scope (stats → CHANGELOG) | README |
| 7 | Architecture | Link | SDS |
| 8 | Platform Support | Table | README |
| 9 | Quick Start | Link | quick_start.md |
| 10 | Usage | Link | user_guide.md |
| 11 | Testing | Link (counts → CHANGELOG) | STG |
| 12 | Examples | Link | examples/ |
| 13 | Build Profiles | Link | quick_start.md |
| 14 | Documentation | Links to all docs | README |
| 15 | Dependencies | Link (versions → CHANGELOG) | CHANGELOG |
| 16 | Submodule Management | Content | README |
| 17 | Code Standards | Content (agent refs) | README |
| 18 | Contributing | Content | README |
| 19 | AI Assistance & Authorship | Content (required legal) | README |
| 20 | License | Content | README |
| 21 | Author | Content | README |
| 22 | Project Status | Checklist (no metrics) | README |

---

## Documentation Directory Structure

```
docs/
├── quick_start.md              # Get running in 10 minutes
├── guides/
│   ├── user_guide.md           # Deep dive usage
│   └── cheatsheet.md           # API ref (libs) / CLI ref (apps)
├── formal/
│   ├── srs.md                  # Requirements (renamed from software_requirements_specification.md)
│   ├── sds.md                  # Design (renamed from software_design_specification.md)
│   └── stg.md                  # Testing (renamed from software_test_guide.md)
├── diagrams/                   # (submodule - do not modify)
├── common/                     # (submodule - do not modify)
└── index.md                    # Navigation hub
```

---

## Document Scope Definitions

### quick_start.md
- Installation and prerequisites
- Build commands and profiles
- Submodule initial setup
- First "hello world" example
- Common issues

### user_guide.md
- All API/usage patterns
- Best practices
- Error handling patterns
- Integration patterns
- Design philosophy

### cheatsheet.md
| Project Type | Content |
|--------------|---------|
| Library | API operations mini-reference |
| Backend API | Endpoints, request/response |
| CLI App | Command line options, flags |

### srs.md (Software Requirements Specification)
- Functional requirements (FR)
- Non-functional requirements (NFR)
- Constraints

### sds.md (Software Design Specification)
- Architecture details
- Package structure
- Design patterns
- Design decisions
- Dependencies rationale

### stg.md (Software Test Guide)
- Test strategy
- Test organization
- How to run tests
- How to write tests
- (Counts → CHANGELOG)

### CHANGELOG.md
- Release-specific metrics:
  - Test counts (unit/integration/e2e)
  - SPARK proof statistics
  - Coverage percentages
  - Dependency versions
- Added/Changed/Fixed/Removed per release

---

## README Documentation Links Section

```markdown
## Documentation

- **[Quick Start](docs/quick_start.md)** - Get started in minutes
- **[User Guide](docs/guides/user_guide.md)** - Complete usage guide
- **[Cheatsheet](docs/guides/cheatsheet.md)** - API/CLI reference
- **[SRS](docs/formal/srs.md)** - Requirements specification
- **[SDS](docs/formal/sds.md)** - Design specification
- **[STG](docs/formal/stg.md)** - Test guide
- **[CHANGELOG](CHANGELOG.md)** - Release history & metrics
```

---

## Implementation Workflow

### Phase 1: REF LIB (hybrid_lib_ada)
1. Update documentation agent with simplified README rules
2. Polish README.md with all 22 fields in correct order
3. Rename formal docs: srs.md, sds.md, stg.md
4. Update /docs structure
5. Review and consolidate each document
6. Push content to correct locations (no redundancy)

### Phase 2: REF APP (hybrid_app_ada)
1. Save current docs as backup
2. Clone new docs structure from REF LIB
3. Move app-specific content from backup into new structure
4. Adapt for app context (5-layer vs 4-layer, CLI cheatsheet vs API)

### Phase 3: Other Projects
1. Update functional README from REF template
2. Update tzif README from REF template
3. Update zoneinfo README from REF template
4. Each project adapts content but maintains structure

---

## Decisions Log

| Decision | Rationale |
|----------|-----------|
| All fields required | REF = maximal template; clones delete unused |
| Metrics in CHANGELOG | Tied to releases, prevents drift |
| Rename to srs/sds/stg | Shorter, all devs know acronyms |
| Two docs (quick_start + user_guide) | Different audiences and depths |
| Cheatsheet varies by type | Libs need API ref, apps need CLI ref |
| README as index | Single navigation point, links to primaries |
| Identical REF structure | Consistency across lib/app projects |

---

## Files to Modify

### Documentation Agent
- Simplify README rules to reference REF projects
- Update file naming (srs.md, sds.md, stg.md)
- Add document scope definitions

### hybrid_lib_ada (REF LIB)
- README.md (restructure)
- docs/formal/ (rename files)
- docs/guides/ (ensure cheatsheet exists)
- docs/quick_start.md (consolidate content)
- docs/index.md (update links)

### hybrid_app_ada (REF APP)
- Clone structure from REF LIB
- Adapt content for app context

### Other Projects (after REF complete)
- functional/README.md
- tzif/README.md
- zoneinfo/README.md

---

## Release Script Adjustments

The release script (`scripts/python/release.py`) will need updates to support new documentation structure:

- Update CHANGELOG parsing to extract metrics from new location
- Update doc file references (srs.md, sds.md, stg.md vs long names)
- Validate new required sections in README
- Handle metrics insertion into CHANGELOG during release

---

## Status

- [x] Planning complete
- [ ] Phase 1: REF LIB
- [ ] Phase 2: REF APP
- [ ] Phase 3: Other projects
- [ ] Documentation agent updated
- [ ] Release script updated

**Note:** Work paused for adafmt tests. Resume after adafmt complete.
