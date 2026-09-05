# ASL-Mem Execution Phases & Roadmap (Steps Protocol)

**Orchestrator**: Antigravity (`steps`)  
**Separation of Duties**: Implementer writes code; reviewer evaluates clean context; orchestrator commits.  
**Execution Strategy**: Sequential DAG waves with strict pre-commit verification gates.

---

## 1. Phase Dependency Graph (DAG)

```mermaid
graph TD
    P1[Phase 1: mem-contract-integrity] --> P2[Phase 2: mem-wal-replay]
    P2 --> P3[Phase 3: mem-host-fs-driver]
    P1 --> P4[Phase 4: mem-flat-slab-simd]
    P3 --> P5[Phase 5: mem-ecosystem-integration]
    P4 --> P5
```

---

## 2. Phase Matrix

| Phase ID | Description | Priority | Depends On | Owns | Gate Command | Status |
|---|---|---|---|---|---|---|
| `mem-contract-integrity` | Graph node removal with edge cascading, vector dimension validation, zero-vector bounds | `P0` | `[]` | `src/graph.asl`, `src/store.asl`, `tests/mem_test.asl` | `asl/.venv/bin/python -c "import sys; from pathlib import Path; sys.path.insert(0, 'asl/checker'); from resolve import check_file; roots = [Path('mem/src'), Path('asl/grammar/corpus/modules')]; diags = [check_file(p, roots) for p in [Path('mem/src/graph.asl'), Path('mem/src/store.asl'), Path('mem/tests/mem_test.asl')]]; sys.exit(sum(len(d) for d in diags))"` | `done` |
| `mem-wal-replay` | Stream replay state rehydration, checkpoint compaction, corrupted frame tolerance | `P0` | `[mem-contract-integrity]` | `src/wal.asl`, `src/engine.asl`, `tests/engine_test.asl` | `asl/.venv/bin/python -c "import sys; from pathlib import Path; sys.path.insert(0, 'asl/checker'); from resolve import check_file; roots = [Path('mem/src'), Path('asl/grammar/corpus/modules')]; diags = [check_file(p, roots) for p in [Path('mem/src/wal.asl'), Path('mem/src/engine.asl'), Path('mem/tests/engine_test.asl')]]; sys.exit(sum(len(d) for d in diags))"` | `pending` |
| `mem-host-fs-driver` | TypeScript/Node host filesystem driver (streaming append, atomic snapshot rename, fsync) | `P1` | `[mem-wal-replay]` | `bridges/ts/driver.ts`, `bridges/ts/index.ts`, `tests/driver_test.js` | `node mem/tests/driver_test.js` | `pending` |
| `mem-flat-slab-simd` | Contiguous Float32Array slab, SIMD-128 dot product kernels, sub-1ms search for 25k vectors | `P1` | `[mem-contract-integrity]` | `benchmark/run.js`, `src/store.asl` | `node mem/benchmark/run.js --check --scale` | `pending` |
| `mem-ecosystem-integration` | Wire MemoryEngine into `agent-core` and `eddie` as default episodic/session substrate | `P2` | `[mem-host-fs-driver, mem-flat-slab-simd]` | `../agent-core/`, `../eddie/`, `package.json` | `python3 tools/sync_workspace.py --check` | `pending` |

---

## 3. Wave Execution Schedule

- **Wave 0 (Contract & Data Integrity)**:
  - `mem-contract-integrity`
- **Wave 1 (Persistence & Vector Acceleration)**:
  - `mem-wal-replay` (Track A: Durability)
  - `mem-flat-slab-simd` (Track B: Performance)
- **Wave 2 (Host System I/O)**:
  - `mem-host-fs-driver`
- **Wave 3 (Ecosystem Integration & Final Verification)**:
  - `mem-ecosystem-integration`
