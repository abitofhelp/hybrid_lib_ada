# PR Review Ask — PR (hybrid_lib_ada Library_Standalone → "standard")

**Branch**: `library-standalone-standard-fix`
**Base**: `main` @ `f02720d`
**Kind**: Build-model correction + SDS design-decision documentation + submodule bump.
**Position**: PR 2a of 3-repo rollout. Sibling to PR 2b (astengine #11, MERGED as `45daaed`). Independent of PR 3 (astfmt `alr update`).

## Discipline note

No pre-phase ask / GPT direction-lock for this PR (per locked plan). PR 2a is the **structural mirror** of PR 2b (astengine), which itself was the result of four GPT review rounds archived in `astfmt/backup/sessions/`. GPT's PR 2b approval message explicitly listed the divergences for PR 2a:

- no `Closes #10` (no astengine issue language)
- project name should be `hybrid_lib_ada`
- SDS version should be `2.0.1 → 2.0.2`

This file is the PR-level code-review ask only.

Upstream rule that this PR satisfies:
- `hybrid_scripts_python` PR #6 (merged as `da1e5be`) — arch_guard now enforces `Library_Standalone = "standard"` on root library GPRs; `namespace_layers.py` no longer auto-upgrades to `"encapsulated"`.

Sibling PR:
- astengine PR #11 (MERGED as `45daaed`) — same diff shape on astengine.gpr + SDS + CHANGELOG + scripts/python/shared submodule bump.

## Matches-locked-scope check

| Locked item | Implemented |
|---|---|
| Bump `scripts/python/shared` submodule to PR 1's merged commit (da1e5be) | ✓ |
| Flip `hybrid_lib_ada.gpr` root GPR `Library_Standalone` "encapsulated" → "standard" | ✓ |
| Rewrite misleading GPR comment (credit `Library_Interface` for public-API enforcement) | ✓ |
| Keep `Library_Interface` unchanged | ✓ (not a single line touched) |
| SDS patch bump v2.0.1 → v2.0.2 (per GPT direction) | ✓ |
| SDS new "Library_Standalone Design Decision" subsection | ✓ (under existing `= Build Configuration` section, after `== Build Profiles`) |
| SDS `change_history` entry | ✓ |
| CHANGELOG `[Unreleased]` Changed + Fixed + Migration | ✓ |
| CHANGELOG cites submodule-vs-Alire migration split | ✓ |
| No "Closes #X" / no astengine issue language (per GPT direction) | ✓ |
| `make check-arch` — FAIL pre-flip, PASS post-flip | ✓ both sides captured |
| `make build` — clean | ✓ (`libhybrid_lib_ada.a`, 0.79s) |
| `make test` — ALL TEST SUITES: SUCCESS (99 unit + 10 integration = 109/109) | ✓ |
| `make docs-formal` — 3/3 PDFs | ✓ |
| No src/** change | ✓ (verified via `git diff --stat`) |
| No SRS / STG change | ✓ |

## "Fail before flip, pass after flip" — proof

Immediately after bumping the submodule with the GPR still at `"encapsulated"`:

```
❌ Root GPR hybrid_lib_ada.gpr: Library_Standalone = "encapsulated"

Required:
  Library_Standalone use "standard";

Why:
  "encapsulated" bundles the Ada runtime (RTS), which can cause
  duplicate-runtime link failures when multiple encapsulated
  libraries are combined.

  Ada toolchain ecosystems require:
    - Library_Standalone = "standard"
    - Library_Interface for public API enforcement

See:
  project SDS § Library_Standalone design decision
...
❌ GPR Configuration (Transitive Dependency Prevention): INVALID
✅ Source File Dependencies: VALID

❌ Architecture validation FAILED
```

After the one-line flip:

```
✅ GPR Configuration (Transitive Dependency Prevention): VALID
✅ Source File Dependencies: VALID

✅ Architecture validation PASSED - All rules satisfied!
```

Same round-trip proof shape as PR 2b — the upstream rule fires on the same input it does for astengine.

## Actual shapes

### `hybrid_lib_ada.gpr` — the flip + rewritten comment

Before (the "PUBLIC API ENFORCEMENT" block misleadingly framed Library_Standalone as the enforcement mechanism):

```gpr
--  Stand-Alone Library Configuration - PUBLIC API ENFORCEMENT
...
--  ENFORCED BY:
--    - Library_Standalone: Enables stand-alone library mode
--    - Library_Interface: Explicitly lists ONLY public packages
--    - GPRbuild: Prevents clients from accessing non-listed packages
...
for Library_Standalone use "encapsulated";
```

After (corrected: `Library_Interface` credited, `"standard"` chosen, rationale captured inline):

```gpr
--  Stand-Alone Library Configuration - PUBLIC API SURFACE
...
--  Public-API enforcement is the role of Library_Interface, NOT of
--  Library_Standalone. Library_Standalone controls how the library
--  is packaged for linking; Library_Interface explicitly lists the
--  packages that clients are allowed to `with`.
...
--  ENFORCED BY:
--    - Library_Interface: explicitly lists the packages clients may
--      `with`. GPRbuild rejects any `with` clause naming an
--      unlisted package.
--    - Library_Standalone: "standard" — stand-alone library that
--      picks up the system Ada runtime at link time. Do NOT use
--      "encapsulated" here: it bundles the Ada RTS and causes
--      duplicate-runtime link failures when multiple encapsulated
--      libraries are composed. See SDS § "Library_Standalone
--      design decision" for the full rationale. Enforced by
--      arch_guard's Ada adapter (ROOT_GPR_ENCAPSULATED_ERROR).
--  =======================================================================

for Library_Standalone use "standard";
```

### SDS new subsection placement (under existing `= Build Configuration`)

PR 2a differs from PR 2b in section placement: hybrid_lib_ada SDS already had a `= Build Configuration` top-level section (with `== GPR Projects` and `== Build Profiles` subsections). The new content was added as a new subsection `== Library_Standalone Design Decision` after `== Build Profiles`, before the next top-level `= Design Decisions` section. The astengine SDS had to add the entire `= Build Configuration` parent itself (because astengine had no such section).

```
= Build Configuration

== GPR Projects                              [unchanged]

== Build Profiles                            [unchanged]

== Library_Standalone Design Decision        [NEW]

hybrid_lib_ada uses:

    Library_Standalone => "standard";

Rationale:
- hybrid_lib_ada is intended for composition within Ada toolchain
  ecosystems ...
- Using "encapsulated" would bundle the full Ada runtime (RTS) ...
- Public-API enforcement is achieved via Library_Interface, not
  via Library_Standalone ...

Therefore:
- "standard" is required for composability.
- "encapsulated" is explicitly prohibited for this project.
- The architecture rule is enforced by
  scripts/python/shared/arch_guard/adapters/ada.py
  (ROOT_GPR_ENCAPSULATED_ERROR) on every make check-arch.

Note:
"encapsulated" may be appropriate for standalone distribution to
non-Ada environments ..., but this is not a requirement for
hybrid_lib_ada.
```

Same body wording as PR 2b modulo the project name swap.

### CHANGELOG (`[Unreleased]`)

- `### Changed` — the flip + comment rewrite + SDS bump (v2.0.1→v2.0.2)
- `### Fixed` — describes the ESAL composition limitation that this PR resolves and cites PR 1 as the upstream fix that closed the template vector. **No** `Closes #X` (per GPT direction).
- `### Migration` — submodule-vs-Alire split per PR 1's migration note

## Verification chain (all green)

```
$ make check-arch   → PASS (previously FAILED pre-flip — captured above)
$ make build        → Success in 0.79s → lib/libhybrid_lib_ada.a
$ make test         → ALL TEST SUITES: SUCCESS (99 unit + 10 integration = 109/109)
$ make docs-formal  → 3/3 PDFs compile
```

## Review asks for GPT

1. **GPR comment rewrite** — same shape as PR 2b's astengine.gpr rewrite, modulo project-name substitutions. Reads cleanly?

2. **SDS subsection placement** — placed under existing `= Build Configuration` (after `== Build Profiles`, before `= Design Decisions`). PR 2b created the parent section because astengine didn't have one; PR 2a slots in as a new subsection. Right call, or should it move into `= Design Decisions` instead?

3. **SDS text precision** — same explicit-prohibition wording as PR 2b. Anything that should differ for hybrid_lib_ada specifically?

4. **CHANGELOG** — no `Closes #X`, no astengine issue language, project-name substitutions applied, version path 2.0.1 → 2.0.2 (per your direction in the PR 2b approval). Anything missed?

5. **Anything missed** before merge? Same narrow shape as PR 2b; this is the second of three repos in the rollout.

## Scope discipline (NOT done)

- **No** change to `src/**` (source code untouched — verified via `git diff --stat`)
- **No** change to `Library_Interface` (public-API enforcement preserved)
- **No** change to SRS / STG / Plan
- **No** API change
- **No** test changes
- **No** change to any non-Ada-library setting elsewhere in the GPR (Library_Kind, Library_Dir, etc.)

## My merge disposition leaning

**Approve.** Same shape as PR 2b (already merged). Verification round-trips against the new arch_guard rule, all 99 tests green, and docs compile. If GPT flags wording, auto-apply; otherwise merge.

## References

- `hybrid_scripts_python` PR #6 (merged as `da1e5be`)
- astengine PR #11 (MERGED as `45daaed`) — sibling, identical structural shape
- 4-round Claude ↔ GPT review archived in `astfmt/backup/sessions/`
- PR 3 (astfmt `alr update`) — to be opened after this PR merges

(no Closes — no associated GitHub issue on hybrid_lib_ada)
