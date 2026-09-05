# Phase 4: Flat Slab SIMD Vector Substrate (`mem-flat-slab-simd`)

**Phase**: `mem-flat-slab-simd`  
**Tier**: Tier 1 (Standard)  
**Owns**: `mem/src/store.asl`, `mem/benchmark/run.js`, `mem/tests/mem_test.asl`  
**Prerequisites**: `mem-contract-integrity` (Done)

---

## 1. Work Items

### Item 1: Contiguous VectorSlab Data Structure in ASL
- **Files**: `mem/src/store.asl`
- **Specification**:
  - Define `VectorSlab`:
    - `dimensions`: `I64`
    - `capacity`: `I64`
    - `count`: `I64`
    - `ids`: `(List Str)`
    - `data`: `(List F64)` (linear continuous array of floats, length = count * dimensions)
  - Implement constructors and operators:
    - `make-vector-slab [(dim I64) (cap I64)] -> VectorSlab`
    - `slab-slot-offset [(slot I64) (dim I64)] -> I64`
    - `slab-insert [(slab VectorSlab) (id Str) (vec (List F64))] -> (Option VectorSlab)`
  - Export `VectorSlab`, `make-vector-slab`, `slab-slot-offset`, `slab-insert`.
- **Failing Gate**: Calling `s/make-vector-slab` fails until declared.

### Item 2: Flat Slab & SIMD-friendly Benchmark Kernels
- **Files**: `mem/benchmark/run.js`
- **Specification**:
  - Implement contiguous `Float32Array` slab storage and 8-way unrolled SIMD-friendly dot product kernel.
  - Maintain both standard object store and high-density flat slab telemetry in `--scale` output.
  - Validate that cold start < 0.1ms, accuracy = 1.000000, and query P50 meets performance target.

### Item 3: Unit Tests for VectorSlab
- **Files**: `mem/tests/mem_test.asl`
- **Specification**:
  - Add `test-vector-slab-operations`: verifies `make-vector-slab`, slot offset calculation, slab insertion, and dimension mismatch rejection.
- **Verification Gate**:
  ```bash
  asl/.venv/bin/python -c "import sys; from pathlib import Path; sys.path.insert(0, 'asl/checker'); from resolve import check_file; roots = [Path('mem/src'), Path('asl/grammar/corpus/modules')]; diags = [check_file(p, roots) for p in [Path('mem/src/store.asl'), Path('mem/tests/mem_test.asl')]]; sys.exit(sum(len(d) for d in diags))"
  node mem/benchmark/run.js --check --scale
  ```
