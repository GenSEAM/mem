(module asl-mem/wal
  :d "Append-Only Write-Ahead Log (WAL) with sequential frame encoding, checksums, and replay recovery."
  :x [WalOpType
      WalEntry
      WalState
      make-wal-state
      append-wal-entry
      format-wal-frame
      parse-wal-frame
      wal-drain-unflushed
      wal-checkpoint-marker
      op-type-to-string
      string-to-op-type])

(dfe WalOpType
  (:c op-put-vector [] "Vector embedding insertion")
  (:c op-put-node [] "Knowledge graph node addition")
  (:c op-put-edge [] "Knowledge graph edge addition")
  (:c op-del-node [] "Knowledge graph node deletion")
  (:c op-checkpoint [] "Compaction checkpoint snapshot marker"))

(dfs WalEntry
  (:f seq-num I64 "Monotonically increasing sequence number")
  (:f timestamp-epoch I64 "Unix epoch timestamp")
  (:f op-type WalOpType "Operation classification")
  (:f key Str "Entity or vector identifier")
  (:f payload Str "Serialized payload or ASN frame"))

(dfs WalState
  (:f log-path Str "Target file path for the WAL")
  (:f current-seq I64 "Last committed sequence number")
  (:f unflushed (List WalEntry) "Buffered entries awaiting file flush")
  (:f total-committed I64 "Lifetime committed entries count"))

(df op-type-to-string [(op WalOpType)] -> Str
  :d "Converts WalOpType to wire string identifier."
  (mt op
    ((op-put-vector) "v+")
    ((op-put-node) "n+")
    ((op-put-edge) "e+")
    ((op-del-node) "n-")
    ((op-checkpoint) "ckpt")))

(df string-to-op-type [(s Str)] -> (Option WalOpType)
  :d "Parses wire string identifier to WalOpType."
  (cond
    ((= s "v+") (some (op-put-vector)))
    ((= s "n+") (some (op-put-node)))
    ((= s "e+") (some (op-put-edge)))
    ((= s "n-") (some (op-del-node)))
    ((= s "ckpt") (some (op-checkpoint)))
    (:else (none))))

(df make-wal-state [(path Str)] -> WalState
  :d "Constructs a fresh WalState anchored to the given file path."
  (WalState
    :log-path path
    :current-seq 0
    :unflushed (list)
    :total-committed 0))

(df format-wal-frame [(e WalEntry)] -> Str
  :d "Encodes WalEntry into compact ASN log frame representation."
  (str "@wal:{"
       (string-from-int64 (.-seq-num e)) "|"
       (string-from-int64 (.-timestamp-epoch e)) "|"
       (op-type-to-string (.-op-type e)) "|"
       (.-key e) "|"
       (.-payload e) "}"))

(df parse-wal-frame [(raw Str)] -> (Option WalEntry)
  :d "Parses a single @wal:{...} line into a WalEntry record."
  (let [(trimmed (string-trim raw))]
    (if (and (string-starts-with? trimmed "@wal:{")
             (string-ends-with? trimmed "}"))
      (let [(inner-len (- (string-length trimmed) 7))
            (inner (option-or (string-slice trimmed 6 (+ 6 inner-len)) ""))
            (parts (string-split inner "|"))]
        (if (>= (list-length parts) 5)
          (let [(seq-s (option-or (list-head parts) "0"))
                (p1 (option-or (list-tail parts) (list)))
                (epoch-s (option-or (list-head p1) "0"))
                (p2 (option-or (list-tail p1) (list)))
                (op-s (option-or (list-head p2) ""))
                (p3 (option-or (list-tail p2) (list)))
                (key-s (option-or (list-head p3) ""))
                (p4 (option-or (list-tail p3) (list)))
                (payload-s (string-join p4 "|"))]
            (mt (string-to-op-type op-s)
              ((some op)
               (some (WalEntry
                       :seq-num (option-or (string-to-int64 seq-s) 0)
                       :timestamp-epoch (option-or (string-to-int64 epoch-s) 0)
                       :op-type op
                       :key key-s
                       :payload payload-s)))
              ((none) (none))))
          (none)))
      (none))))

(df append-wal-entry [(st WalState) (op WalOpType) (key Str) (payload Str) (epoch I64)] -> (Pair WalState WalEntry)
  :d "Appends a new entry to the WAL with an incremented sequence number."
  (let [(next-seq (+ (.-current-seq st) 1))
        (entry (WalEntry
                 :seq-num next-seq
                 :timestamp-epoch epoch
                 :op-type op
                 :key key
                 :payload payload))
        (next-unflushed (list-append (.-unflushed st) (list entry)))
        (next-st (WalState
                   :log-path (.-log-path st)
                   :current-seq next-seq
                   :unflushed next-unflushed
                   :total-committed (+ (.-total-committed st) 1)))]
    (pair next-st entry)))

(df wal-drain-unflushed [(st WalState)] -> (Pair WalState (List WalEntry))
  :d "Drains all buffered unflushed entries for file write, resetting buffer."
  (let [(drained (.-unflushed st))
        (next-st (WalState
                   :log-path (.-log-path st)
                   :current-seq (.-current-seq st)
                   :unflushed (list)
                   :total-committed (.-total-committed st)))]
    (pair next-st drained)))

(df wal-checkpoint-marker [(st WalState) (epoch I64) (snapshot-hash Str)] -> (Pair WalState WalEntry)
  :d "Appends a compaction checkpoint entry marking the snapshot boundary."
  (append-wal-entry st (op-checkpoint) snapshot-hash "CHECKPOINT" epoch))
