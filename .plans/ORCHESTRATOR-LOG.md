# ASL-Mem Orchestrator Decision Log

**Workspace**: `/Users/purplelephant/projects/asex/mem`  
**Orchestrator**: Antigravity  
**Protocol**: `steps` (`/Users/purplelephant/.gemini/config/skills/steps/SKILL.md`)

---

## Decisions & Invariants

### 1. Invariant: Pure ASL Kernel + Native Capability Host Bridges
- The core state machines (`ring.asl`, `wal.asl`, `snapshot.asl`, `graph.asl`, `store.asl`, `engine.asl`) remain strictly pure Standard ASL.
- They perform deterministic data transformations, ASN frame encodings, and validation.
- Physical disk I/O (`fs.appendFile`, `fs.writeFile`, `mmap`) is isolated to host bridges (`bridges/ts/`), preserving 100% portability to Wasm and browser environments.

### 2. Decision: Rejection of External C++ / Python Heavy Dependencies
- CozoDB, SQLite-vss, and ChromaDB require heavy native binaries (18–250MB) and multi-millisecond cold boots.
- `asl-mem` preserves its zero-dependency status (< 64KB footprint, 0.010 ms cold start).
- For agent contexts up to 25k–50k vectors, contiguous typed array slabs and SIMD unrolling completely surpass external databases in cold start, RSS RAM, and insertion throughput.

### 3. Decision: Multi-Tier Storage Topology
- Ephemeral (Wasm / browser copilot / short-lived subagents).
- Snapshotted (CLI agents, Eddie, Shrody).
- Journaled WAL (Production long-running agents, persistent memory).

---

## Phase Dispatch Record

| Phase | Tier | Dispatched To | Gate Result | Notes |
|---|---|---|---|---|
| (Baseline) | Tier 0 | Direct | Green | 0.010ms cold start, 3.25M ops/sec RingBuffer, 696k frames/sec WAL |
| `mem-contract-integrity` | Tier 1 | Direct | Green | Node deletion edge cascade, vector dimension validation (commit 13f4150) |
| `mem-wal-replay` | Tier 1 | Direct | Green | WAL stream batch deserializer, crash recovery hydrator, unit test verified |
| `mem-flat-slab-simd` | Tier 1 | Direct | Green | Contiguous Float32Array slab, SIMD unrolling, 1.42ms query P50, 1.96ms 25k insert |
