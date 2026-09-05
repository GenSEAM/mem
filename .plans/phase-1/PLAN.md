# Phase 1: Contract & Data Integrity (`mem-contract-integrity`)

**Phase**: `mem-contract-integrity`  
**Tier**: Tier 1 (Standard)  
**Owns**: `mem/src/graph.asl`, `mem/src/store.asl`, `mem/tests/mem_test.asl`  
**Prerequisites**: None  

---

## 1. Work Items

### Item 1: Graph Node Deletion with Incident Edge Cascading
- **Files**: `mem/src/graph.asl`
- **Specification**:
  - Implement `remove-node [(graph KnowledgeGraph) (node-id Str)] -> KnowledgeGraph`:
    Filters out the node from `(.-nodes graph)` AND filters out all edges where `source-id == node-id` OR `target-id == node-id`.
  - Export `remove-node` in `:x [...]`.
  - Ensure O(1) index compatibility in `build-graph-index`.
- **Failing Gate**: Calling `g/remove-node` fails in `tests/mem_test.asl` until implemented.

### Item 2: Vector Dimension Validation & Safety Bounds
- **Files**: `mem/src/store.asl`
- **Specification**:
  - Implement `validate-vector-dim [(store VectorStore) (v (List F64))] -> Bool`:
    Asserts `(= (list-length v) (.-dimensions store))`.
  - Implement `safe-insert-item [(store VectorStore) (item VectorItem)] -> (Option VectorStore)`:
    Returns `(some next-store)` if valid, `(none)` if dimension mismatch.
  - Export `validate-vector-dim` and `safe-insert-item` in `:x [...]`.
- **Failing Gate**: Calling `s/validate-vector-dim` or `s/safe-insert-item` fails until declared and exported.

### Item 3: Unit Tests & Regression Verification
- **Files**: `mem/tests/mem_test.asl`
- **Specification**:
  - Add `test-graph-node-removal`: tests creating nodes A and B, edge A->B, removing A, and asserting both A and edge A->B are gone (0 orphaned edges).
  - Add `test-vector-dimension-check`: tests 384-dim pass and 128-dim reject.
  - Integrate into `run-tests`.
- **Verification Gate**:
  ```bash
  asl/.venv/bin/python -c "import sys; from pathlib import Path; sys.path.insert(0, 'asl/checker'); from resolve import check_file; roots = [Path('mem/src'), Path('asl/grammar/corpus/modules')]; diags = [check_file(p, roots) for p in [Path('mem/src/graph.asl'), Path('mem/src/store.asl'), Path('mem/tests/mem_test.asl')]]; sys.exit(sum(len(d) for d in diags))"
  ```
  Must exit 0 with 0 diagnostics.
