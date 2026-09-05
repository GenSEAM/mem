# Phase 2: WAL Replay & Crash Recovery (`mem-wal-replay`)

**Phase**: `mem-wal-replay`  
**Tier**: Tier 1 (Standard)  
**Owns**: `mem/src/wal.asl`, `mem/src/engine.asl`, `mem/tests/engine_test.asl`  
**Prerequisites**: `mem-contract-integrity` (Done)

---

## 1. Work Items

### Item 1: WAL Stream Parser & Batch Deserializer
- **Files**: `mem/src/wal.asl`
- **Specification**:
  - Implement `parse-wal-stream [(raw-log Str)] -> (List WalEntry)`:
    Splits by newline, filters empty lines, and maps `parse-wal-frame`, discarding corrupted/malformed frames gracefully.
  - Export `parse-wal-stream` in `:x [...]`.
- **Failing Gate**: Calling `w/parse-wal-stream` fails until declared.

### Item 2: Engine Recovery State Hydrator
- **Files**: `mem/src/engine.asl`
- **Specification**:
  - Implement `engine-apply-entry [(eng MemoryEngine) (entry w/WalEntry)] -> MemoryEngine`:
    Dispatches on `(.-op-type entry)`:
    - `op-put-node`: parses and applies `engine-put-node`
    - `op-put-edge`: parses and applies `engine-put-edge`
    - `op-del-node`: applies `g/remove-node` to `(.-graph eng)`
    - `op-checkpoint`: updates `(.-last-checkpoint-epoch eng)`
  - Implement `engine-recover [(eng MemoryEngine) (raw-log Str)] -> MemoryEngine`:
    Parses stream and folds `engine-apply-entry` over entries.
  - Export `engine-recover` and `engine-apply-entry` in `:x [...]`.
- **Failing Gate**: Calling `eng/engine-recover` fails until declared.

### Item 3: Unit Tests for Crash Recovery
- **Files**: `mem/tests/engine_test.asl`
- **Specification**:
  - Add `test-wal-crash-recovery`: writes 3 entries (node, vector, del-node) to string format, runs `engine-recover`, verifies resulting graph and sequence state.
- **Verification Gate**:
  ```bash
  asl/.venv/bin/python -c "import sys; from pathlib import Path; sys.path.insert(0, 'asl/checker'); from resolve import check_file; roots = [Path('mem/src'), Path('asl/grammar/corpus/modules')]; diags = [check_file(p, roots) for p in [Path('mem/src/wal.asl'), Path('mem/src/engine.asl'), Path('mem/tests/engine_test.asl')]]; sys.exit(sum(len(d) for d in diags))"
  ```
  Must exit 0 with 0 diagnostics.
