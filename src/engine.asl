(module asl-mem/engine
  :d "Unified ASL Storage Engine: orchestrating Ephemeral In-Memory, Periodic Snapshotting, and Journaled WAL storage tiers."
  :x [StorageMode
      StorageConfig
      MemoryEngine
      EngineStats
      make-engine
      engine-put-vector
      engine-put-node
      engine-put-edge
      engine-checkpoint
      engine-stats]
  :i [(store :a s)
      (graph :a g)
      (ring :a r)
      (wal :a w)
      (snapshot :a snap)])

(dfe StorageMode
  (:c mode-ephemeral [] "100% in-memory ring buffer, zero disk I/O")
  (:c mode-snapshot [] "In-memory with periodic or manual atomic snapshotting")
  (:c mode-journaled-wal [] "In-memory ring buffer backed by append-only WAL on disk"))

(dfs StorageConfig
  (:f mode StorageMode "Selected persistence and storage strategy")
  (:f ring-capacity I64 "Maximum slots in the working ring buffer")
  (:f wal-path Str "File path for the write-ahead log")
  (:f snapshot-path Str "File path for state snapshots"))

(dfs MemoryEngine
  (:f config StorageConfig "Engine operational parameters")
  (:f ring r/RingBuffer "Active working ring buffer")
  (:f vectors s/VectorStore "Active in-memory vector store")
  (:f graph g/KnowledgeGraph "Active in-memory knowledge graph")
  (:f wal w/WalState "Write-ahead log state machine")
  (:f last-checkpoint-epoch I64 "Epoch timestamp of most recent checkpoint"))

(dfs EngineStats
  (:f mode-name Str "Active operational mode name")
  (:f ring-utilization I64 "Working buffer capacity percent")
  (:f vector-count I64 "Total stored embedding vectors")
  (:f node-count I64 "Total knowledge graph nodes")
  (:f edge-count I64 "Total knowledge graph edges")
  (:f wal-entries-committed I64 "Count of committed WAL log entries")
  (:f evicted-count I64 "Total items evicted from working ring"))

(df make-engine [(mode StorageMode) (ring-cap I64) (wal-path Str) (snap-path Str)] -> MemoryEngine
  :d "Initializes a unified MemoryEngine with chosen storage tier configuration."
  (let [(cfg (StorageConfig
               :mode mode
               :ring-capacity ring-cap
               :wal-path wal-path
               :snapshot-path snap-path))
        (ring-buf (r/make-ring-buffer ring-cap (r/policy-spill)))
        (v-store (s/VectorStore :name "default" :dimensions 384 :items (list)))
        (k-graph (g/KnowledgeGraph :nodes (list) :edges (list)))
        (wal-st (w/make-wal-state wal-path))]
    (MemoryEngine
      :config cfg
      :ring ring-buf
      :vectors v-store
      :graph k-graph
      :wal wal-st
      :last-checkpoint-epoch 0)))

(df engine-put-vector [(eng MemoryEngine) (id Str) (text Str) (v (List F64)) (epoch I64)] -> MemoryEngine
  :d "Stores an embedding vector into working ring buffer, vector store, and WAL."
  (let [(item (s/VectorItem :id id :text text :vector v))
        (next-vectors (s/VectorStore
                        :name (.-name (.-vectors eng))
                        :dimensions (.-dimensions (.-vectors eng))
                        :items (list-append (.-items (.-vectors eng)) (list item))))
        (frame (snap/serialize-vector-item item))
        (push-res (r/ring-push (.-ring eng) frame))
        (wal-res (mt (.-mode (.-config eng))
                   ((mode-journaled-wal)
                    (let [(pair-res (w/append-wal-entry (.-wal eng) (w/op-put-vector) id frame epoch))]
                      (.-first pair-res)))
                   ((mode-ephemeral) (.-wal eng))
                   ((mode-snapshot) (.-wal eng))))]
    (MemoryEngine
      :config (.-config eng)
      :ring (.-buffer push-res)
      :vectors next-vectors
      :graph (.-graph eng)
      :wal wal-res
      :last-checkpoint-epoch (.-last-checkpoint-epoch eng))))

(df engine-put-node [(eng MemoryEngine) (node g/GraphNode) (epoch I64)] -> MemoryEngine
  :d "Stores a knowledge graph entity into working ring buffer, graph, and WAL."
  (let [(next-graph (g/add-node (.-graph eng) node))
        (frame (snap/serialize-graph-node-frame node))
        (push-res (r/ring-push (.-ring eng) frame))
        (wal-res (mt (.-mode (.-config eng))
                   ((mode-journaled-wal)
                    (let [(pair-res (w/append-wal-entry (.-wal eng) (w/op-put-node) (.-id node) frame epoch))]
                      (.-first pair-res)))
                   ((mode-ephemeral) (.-wal eng))
                   ((mode-snapshot) (.-wal eng))))]
    (MemoryEngine
      :config (.-config eng)
      :ring (.-buffer push-res)
      :vectors (.-vectors eng)
      :graph next-graph
      :wal wal-res
      :last-checkpoint-epoch (.-last-checkpoint-epoch eng))))

(df engine-put-edge [(eng MemoryEngine) (edge g/GraphEdge) (epoch I64)] -> MemoryEngine
  :d "Stores a knowledge graph relation edge into working ring buffer, graph, and WAL."
  (let [(next-graph (g/add-edge (.-graph eng) edge))
        (frame (snap/serialize-graph-edge edge))
        (push-res (r/ring-push (.-ring eng) frame))
        (wal-res (mt (.-mode (.-config eng))
                   ((mode-journaled-wal)
                    (let [(pair-res (w/append-wal-entry (.-wal eng) (w/op-put-edge) (.-source-id edge) frame epoch))]
                      (.-first pair-res)))
                   ((mode-ephemeral) (.-wal eng))
                   ((mode-snapshot) (.-wal eng))))]
    (MemoryEngine
      :config (.-config eng)
      :ring (.-buffer push-res)
      :vectors (.-vectors eng)
      :graph next-graph
      :wal wal-res
      :last-checkpoint-epoch (.-last-checkpoint-epoch eng))))

(df engine-checkpoint [(eng MemoryEngine) (epoch I64)] -> (Pair MemoryEngine Str)
  :d "Produces an atomic ASN snapshot string and records checkpoint marker in WAL."
  (let [(snap-str (snap/format-snapshot-ledger (.-vectors eng) (.-graph eng) epoch))
        (next-wal (mt (.-mode (.-config eng))
                    ((mode-journaled-wal)
                     (let [(pair-res (w/wal-checkpoint-marker (.-wal eng) epoch "v1"))]
                       (.-first pair-res)))
                    ((mode-ephemeral) (.-wal eng))
                    ((mode-snapshot) (.-wal eng))))
        (next-eng (MemoryEngine
                    :config (.-config eng)
                    :ring (.-ring eng)
                    :vectors (.-vectors eng)
                    :graph (.-graph eng)
                    :wal next-wal
                    :last-checkpoint-epoch epoch))]
    (pair next-eng snap-str)))

(df engine-stats [(eng MemoryEngine)] -> EngineStats
  :d "Returns operational telemetry metrics across ring buffer, vector store, and WAL."
  (let [(m-str (mt (.-mode (.-config eng))
                 ((mode-ephemeral) "ephemeral")
                 ((mode-snapshot) "snapshot")
                 ((mode-journaled-wal) "journaled-wal")))]
    (EngineStats
      :mode-name m-str
      :ring-utilization (r/ring-utilization (.-ring eng))
      :vector-count (list-length (.-items (.-vectors eng)))
      :node-count (list-length (.-nodes (.-graph eng)))
      :edge-count (list-length (.-edges (.-graph eng)))
      :wal-entries-committed (.-total-committed (.-wal eng))
      :evicted-count (r/ring-evicted-count (.-ring eng)))))
