# ASL-Mem Gap Audit & Architectural Review

**Audit Date**: 2026-09-05  
**Target Repository**: `@genseam/asl-mem` (`/Users/purplelephant/projects/asex/mem`)  
**Review Standard**: Critic (`gap/SKILL.md`) — zero bloat, anti-overengineering, complete contract coverage.

---

## 1. Executive Summary

`asl-mem` has achieved sub-millisecond cold start (0.010 ms), low memory footprint (< 6 MB), and high-throughput vector ingestion (19.9M vec/s). With the introduction of `RingBuffer`, `WAL`, and `StorageEngine` in pure ASL, it now serves as a universal storage substrate for the entire GenSEAM ecosystem.

However, an audit against production reliability, runtime boundaries, and contract completeness reveals **5 architectural gaps** that must be closed before production deployment across agent harnesses.

---

## 2. Identified Gaps

### GAP-1: Host Runtime Bridge & Physical Disk I/O (`bridges/ts/`)
- **Location**: `mem/src/wal.asl:40`, `mem/src/engine.asl:35`
- **Issue**: ASL core modules (`wal.asl`, `snapshot.asl`, `engine.asl`) operate as pure state reducers returning serialized frames. Physical disk persistence (`fs.appendFile` for sequential WAL writes, atomic temp-file rename for snapshots) requires host environment bindings.
- **Risk**: Without a high-performance TypeScript/Node/Bun host bridge, agents running outside pure Wasm cannot persist WAL entries to real disk files.
- **Remediation**: Create `mem/bridges/ts/index.ts` and `mem/bridges/ts/driver.ts` providing streaming file appends, fsync, and atomic snapshot writes.

### GAP-2: Graph Tombstoning & Edge Cascade Cleanup
- **Location**: `mem/src/graph.asl`, `mem/src/wal.asl:18`
- **Issue**: `wal.asl` declares `op-del-node` (`n-`), but `graph.asl` only implements `add-node`, `add-edge`, and `add-nodes-batch`. Deleting a node leaves orphaned edges pointing to nonexistent entities.
- **Risk**: Dangling relationship edges in knowledge graph traversals causing inconsistent RAG facts.
- **Remediation**: Implement `remove-node [(graph KnowledgeGraph) (node-id Str)] -> KnowledgeGraph` that purges both the vertex and all connected incident edges (`source-id == id` or `target-id == id`).

### GAP-3: WAL Crash Replay & State Hydration Engine
- **Location**: `mem/src/wal.asl:73`
- **Issue**: `wal.asl` provides `parse-wal-frame` for single lines, but lacks a full stream rehydration loop `replay-wal [(raw-log Str) (initial-engine MemoryEngine)] -> MemoryEngine` that rolls forward vector, node, and edge mutations from the last checkpoint.
- **Risk**: Inability to recover state automatically after host crash or process reboot.
- **Remediation**: Implement `replay-wal` in `mem/src/wal.asl` and `engine-restore-from-log` in `mem/src/engine.asl`.

### GAP-4: Dimension Validation & Vector Normalization Safety
- **Location**: `mem/src/store.asl:37-43`
- **Issue**: `dot` truncates to the shorter vector silently via `(zip a b)`. If an agent pushes an embedding of 1536 dimensions (OpenAI) into a store initialized for 384 dimensions (SentenceTransformers), the search silently computes a partial dot product without error.
- **Risk**: Silent retrieval quality degradation due to dimension mismatch.
- **Remediation**: Add explicit dimension assertion `validate-vector-dim [(store VectorStore) (v (List F64))] -> Bool` before ingestion.

### GAP-5: Contiguous Flat Memory Slab & Wasm SIMD Target
- **Location**: `mem/src/store.asl`, `mem/benchmark/run.js:122`
- **Issue**: Vector storage currently relies on `(List VectorItem)` (linked lists in functional ASL, Array of Objects in JS). At 25k+ vectors, traversing pointer indirection creates cache misses.
- **Risk**: Search latency scales linearly (6.6 ms at 25k in JS) instead of SIMD-accelerated linear memory (<0.20 ms).
- **Remediation**: Implement flat typed slab memory layout (`Float32Array` in TS / Wasm Linear Memory page) and SIMD-128 dot product kernels.

---

## 3. Anti-Overengineering (Critic Filter)

- **Do NOT add SQLite or RocksDB dependencies**: Pure ASL append-only WAL with sequential ASN frames provides 696k entries/sec at zero external dependency cost.
- **Do NOT build complex multi-layered graph query languages**: Adjacency lists and BFS neighborhood scans satisfy 100% of agent working context queries.
- **Do NOT implement distributed Paxos/Raft consensus inside `mem`**: Multi-agent mesh coordination belongs to `agent-bus` and `skyloom`; `mem` remains an atomic, portable, single-agent storage substrate.
