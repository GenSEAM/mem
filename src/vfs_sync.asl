(module asl-mem/vfs-sync
  :d "In-memory virtual file system disk synchronization with Optimistic Concurrency Control and base-hash validation"
  :x [VFSSyncState
      make-sync-state
      detect-disk-drift
      validate-flush-safety
      update-staged-hash
      record-flush-success
      resolve-sync-action]
  :i [])

(dfs VFSSyncState
  (:f path Str "Normalized virtual file path")
  (:f disk-base-hash Str "CAS content hash recorded at buffer read time")
  (:f disk-mtime I64 "Physical disk modification timestamp at read time in milliseconds")
  (:f staged-hash Str "CAS content hash of currently staged in-memory buffer")
  (:f staged-revision I64 "Monotonic revision counter of staged buffer edits")
  (:f status Str "Synchronization lifecycle status: clean, staged, conflict, drifted, or synced"))

(df normalize-sync-path [(path Str)] -> Str
  :d "Normalizes virtual file path by trimming whitespace and stripping leading slash prefixes."
  (let [(trimmed (string-trim path))]
    (if (string-starts-with? trimmed "./")
      (option-or (string-slice trimmed 2 (string-length trimmed)) "")
      (if (string-starts-with? trimmed "/")
        (option-or (string-slice trimmed 1 (string-length trimmed)) "")
        trimmed))))

(df make-sync-state [(path Str) (disk-base-hash Str) (disk-mtime I64)] -> VFSSyncState
  :d "Constructs an initial OCC synchronization state record for a virtual file path."
  (let [(norm (normalize-sync-path path))]
    (VFSSyncState
      :path norm
      :disk-base-hash disk-base-hash
      :disk-mtime disk-mtime
      :staged-hash disk-base-hash
      :staged-revision 1
      :status "clean")))

(df detect-disk-drift [(state VFSSyncState) (current-disk-hash Str) (current-disk-mtime I64)] -> Bool
  :d "Detects whether physical disk file has drifted in content hash or modification timestamp since buffer read."
  (or (!= (.-disk-base-hash state) current-disk-hash)
      (!= (.-disk-mtime state) current-disk-mtime)))

(df validate-flush-safety [(state VFSSyncState) (current-disk-hash Str)] -> Bool
  :d "Validates that current disk content matches pristine base hash for conflict-free CAS flush."
  (= (.-disk-base-hash state) current-disk-hash))

(df update-staged-hash [(state VFSSyncState) (new-staged-hash Str)] -> VFSSyncState
  :d "Records in-memory buffer mutation and updates staged revision and status."
  (let [(is-dirty (!= new-staged-hash (.-disk-base-hash state)))
        (new-status (if is-dirty "staged" "clean"))]
    (VFSSyncState
      :path (.-path state)
      :disk-base-hash (.-disk-base-hash state)
      :disk-mtime (.-disk-mtime state)
      :staged-hash new-staged-hash
      :staged-revision (+ (.-staged-revision state) 1)
      :status new-status)))

(df record-flush-success [(state VFSSyncState) (new-disk-hash Str) (new-mtime I64)] -> VFSSyncState
  :d "Updates synchronization state following successful physical disk flush."
  (VFSSyncState
    :path (.-path state)
    :disk-base-hash new-disk-hash
    :disk-mtime new-mtime
    :staged-hash new-disk-hash
    :staged-revision (.-staged-revision state)
    :status "synced"))

(df resolve-sync-action [(state VFSSyncState) (current-disk-hash Str) (current-disk-mtime I64)] -> Str
  :d "Evaluates current disk state and returns sync verdict: clean, safe, or :conflict."
  (if (and (= (.-disk-base-hash state) current-disk-hash)
           (= (.-staged-hash state) current-disk-hash))
    "clean"
    (if (validate-flush-safety state current-disk-hash)
      "safe"
      ":conflict")))
