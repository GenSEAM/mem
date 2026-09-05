import { HostFsDriver } from "../bridges/ts/driver.js";
import * as fs from "node:fs/promises";
import * as os from "node:os";
import * as path from "node:path";
import assert from "node:assert";

async function runDriverTests() {
  const tmpDir = await fs.mkdtemp(path.join(os.tmpdir(), "asl-mem-driver-test-"));
  const walPath = path.join(tmpDir, "sub", "test.wal");
  const snapPath = path.join(tmpDir, "sub", "test.snap.asn");

  try {
    const driver = new HostFsDriver({ autoCreateDirs: true, syncOnAppend: true });

    // 1. Test streaming WAL append
    await driver.appendWal(walPath, "@wal:{1|1000|PUT-NODE|n1|payload-1}");
    await driver.appendWal(walPath, "@wal:{2|1001|PUT-NODE|n2|payload-2}");
    
    const walContent = await driver.readWal(walPath);
    assert(walContent.includes("@wal:{1|1000|PUT-NODE|n1|payload-1}"));
    assert(walContent.includes("@wal:{2|1001|PUT-NODE|n2|payload-2}"));
    const lines = walContent.trim().split("\n");
    assert.strictEqual(lines.length, 2);

    // 2. Test atomic snapshot write and read
    const snapData = "@snap:{v1|1000|2|2|1}\n@v:{v1|test|[0.1,0.2]}\n@n:{n1|tag|1000|1.0|content}";
    await driver.writeSnapshotAtomic(snapPath, snapData);
    
    const readBackSnap = await driver.readSnapshot(snapPath);
    assert.strictEqual(readBackSnap, snapData);

    // 3. Test non-existent file reads return empty string gracefully
    const missing = await driver.readWal(path.join(tmpDir, "does-not-exist.wal"));
    assert.strictEqual(missing, "");

    console.log("✓ HostFsDriver test passed cleanly (WAL append, atomic snapshot, fsync).");
    process.exit(0);
  } finally {
    await fs.rm(tmpDir, { recursive: true, force: true });
  }
}

runDriverTests().catch(err => {
  console.error("✗ HostFsDriver test failed:", err);
  process.exit(1);
});
