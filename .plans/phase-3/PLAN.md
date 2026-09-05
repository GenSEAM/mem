# Phase 3: Host Filesystem Driver (`mem-host-fs-driver`)

**Phase**: `mem-host-fs-driver`  
**Tier**: Tier 1 (Standard)  
**Owns**: `mem/bridges/ts/driver.ts`, `mem/bridges/ts/index.ts`, `mem/tests/driver_test.js`  
**Prerequisites**: `mem-wal-replay` (Done)

---

## 1. Work Items

### Item 1: TypeScript Host Filesystem Driver
- **Files**: `mem/bridges/ts/driver.ts`, `mem/bridges/ts/index.ts`
- **Specification**:
  - Implement `HostFsDriver`:
    - `appendWal(path: string, frame: string): Promise<void>` (streaming append with `fs.promises.appendFile`)
    - `readWal(path: string): Promise<string>` (reads WAL content for recovery)
    - `writeSnapshotAtomic(path: string, data: string): Promise<void>` (writes to `${path}.tmp` then atomic `fs.promises.rename` to prevent corrupted snapshots on crash)
    - `readSnapshot(path: string): Promise<string>` (reads snapshot)
    - `fsync(path: string): Promise<void>` (ensures durability via file descriptor sync)
  - Export driver and types in `mem/bridges/ts/index.ts`.

### Item 2: Test Suite for Host Driver
- **Files**: `mem/tests/driver_test.js`
- **Specification**:
  - Verifies:
    1. Append WAL streaming: multi-entry sequence correctly written and read back.
    2. Atomic snapshot write: ensures file is atomically written via temp rename.
    3. Error handling: non-existent directories handled gracefully or auto-created.
  - Exits with code 0 on success.

- **Verification Gate**:
  ```bash
  node mem/tests/driver_test.js
  ```
