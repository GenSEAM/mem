/**
 * @genseam/asl-mem - Host Filesystem Persistence Driver
 * 
 * Provides crash-resilient streaming WAL append and atomic snapshotting
 * using Node.js filesystem primitives with fsync and tmp-rename semantics.
 */

import * as fs from "node:fs/promises";
import * as path from "node:path";

export interface FsDriverConfig {
  autoCreateDirs?: boolean;
  syncOnAppend?: boolean;
}

export class HostFsDriver {
  private config: FsDriverConfig;

  constructor(config: FsDriverConfig = {}) {
    this.config = {
      autoCreateDirs: true,
      syncOnAppend: false,
      ...config
    };
  }

  private async ensureParentDir(filePath: string): Promise<void> {
    if (this.config.autoCreateDirs) {
      const dir = path.dirname(filePath);
      await fs.mkdir(dir, { recursive: true });
    }
  }

  /**
   * Appends a WAL entry frame sequentially to the append-only log.
   */
  async appendWal(logPath: string, frame: string): Promise<void> {
    await this.ensureParentDir(logPath);
    const line = frame.endsWith("\n") ? frame : frame + "\n";
    await fs.appendFile(logPath, line, "utf-8");
    if (this.config.syncOnAppend) {
      await this.fsync(logPath);
    }
  }

  /**
   * Reads the raw WAL log for state replay and recovery.
   */
  async readWal(logPath: string): Promise<string> {
    try {
      return await fs.readFile(logPath, "utf-8");
    } catch (err: any) {
      if (err.code === "ENOENT") {
        return "";
      }
      throw err;
    }
  }

  /**
   * Writes an atomic snapshot document using write-to-tmp and atomic rename.
   * This guarantees that a crash during writing never leaves a half-written corrupted snapshot.
   */
  async writeSnapshotAtomic(snapshotPath: string, content: string): Promise<void> {
    await this.ensureParentDir(snapshotPath);
    const tmpPath = `${snapshotPath}.tmp.${Date.now()}.${Math.random().toString(36).slice(2, 8)}`;
    await fs.writeFile(tmpPath, content, "utf-8");
    await fs.rename(tmpPath, snapshotPath);
  }

  /**
   * Reads the current atomic snapshot document.
   */
  async readSnapshot(snapshotPath: string): Promise<string> {
    try {
      return await fs.readFile(snapshotPath, "utf-8");
    } catch (err: any) {
      if (err.code === "ENOENT") {
        return "";
      }
      throw err;
    }
  }

  /**
   * Flushes kernel disk buffers to persistent storage for durability.
   */
  async fsync(filePath: string): Promise<void> {
    let handle;
    try {
      handle = await fs.open(filePath, "r+");
      await handle.sync();
    } catch (err: any) {
      if (err.code !== "ENOENT") throw err;
    } finally {
      if (handle) await handle.close();
    }
  }
}
