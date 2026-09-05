# Phase 5: Ecosystem Integration & End-to-End Verification (`mem-ecosystem-integration`)

**Phase**: `mem-ecosystem-integration`  
**Tier**: Tier 1 (Standard)  
**Owns**: `mem/package.json`, `agent-core/package.json`, `eddie/package.json`  
**Prerequisites**: `mem-host-fs-driver` (Done), `mem-flat-slab-simd` (Done)

---

## 1. Work Items

### Item 1: Package Exports & Test Integration in `mem`
- **Files**: `mem/package.json`
- **Specification**:
  - Update `main` and `exports` to expose `bridges/ts/index.js` and `asl.json`.
  - Include `node tests/driver_test.js` in `npm test`.

### Item 2: Wire `@genseam/asl-mem` into Agent Runtimes
- **Files**: `agent-core/package.json`, `eddie/package.json`
- **Specification**:
  - Declare `@genseam/asl-mem` v0.3.0 as dependency for episodic storage & streaming event substrates in `agent-core` and `eddie`.

### Item 3: End-to-End Workspace Verification Gate
- **Specification**:
  - Run all gates across workspace:
    1. `npm --prefix mem test` (storage benchmarks, flat-slab vector scales, driver tests).
    2. Python test runner / resolver checks.
    3. Multi-repo workspace integrity check: `python3 tools/sync_workspace.py --check`.
  - All 12 repositories must be 100% clean and green.
