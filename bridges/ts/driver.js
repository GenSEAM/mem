/**
 * @genseam/asl-mem - Host Filesystem Persistence Driver (ESM)
 */

import * as fs from "node:fs/promises";
import * as path from "node:path";

export class HostFsDriver {
  constructor(config = {}) {
    this.config = {
      autoCreateDirs: true,
      syncOnAppend: false,
      ...config
    };
  }

  async ensureParentDir(filePath) {
    if (this.config.autoCreateDirs) {
      const dir = path.dirname(filePath);
      await fs.mkdir(dir, { recursive: true });
    }
  }

  async appendWal(logPath, frame) {
    await this.ensureParentDir(logPath);
    const line = frame.endsWith("\n") ? frame : frame + "\n";
    await fs.appendFile(logPath, line, "utf-8");
    if (this.config.syncOnAppend) {
      await this.fsync(logPath);
    }
  }

  async readWal(logPath) {
    try {
      return await fs.readFile(logPath, "utf-8");
    } catch (err) {
      if (err.code === "ENOENT") {
        return "";
      }
      throw err;
    }
  }

  async writeSnapshotAtomic(snapshotPath, content) {
    await this.ensureParentDir(snapshotPath);
    const tmpPath = `${snapshotPath}.tmp.${Date.now()}.${Math.random().toString(36).slice(2, 8)}`;
    await fs.writeFile(tmpPath, content, "utf-8");
    await fs.rename(tmpPath, snapshotPath);
  }

  async readSnapshot(snapshotPath) {
    try {
      return await fs.readFile(snapshotPath, "utf-8");
    } catch (err) {
      if (err.code === "ENOENT") {
        return "";
      }
      throw err;
    }
  }

  async fsync(filePath) {
    let handle;
    try {
      handle = await fs.open(filePath, "r+");
      await handle.sync();
    } catch (err) {
      if (err.code !== "ENOENT") throw err;
    } finally {
      if (handle) await handle.close();
    }
  }
}
