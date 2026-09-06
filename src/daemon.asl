(module asl-mem/daemon
  :d "Pure AgentScript memory daemon specification, multi-level config resolution, and RPC command dispatch."
  :x [DaemonConfig
      DaemonState
      BufferRecord
      ConfigHierarchy
      make-daemon-config
      make-daemon-state
      resolve-hierarchical-config
      dispatch-rpc-op
      evict-lru-buffers
      untangle-step]
  :i [(store :a s)
      (graph :a g)
      (ring :a r)
      (wal :a w)])

(dfs DaemonConfig
  (:f socket-path Str "Unix domain socket file path")
  (:f max-clean-buffers I64 "Maximum clean virtual buffers in memory cache")
  (:f lru-eviction-batch I64 "Batch count for LRU buffer pruning")
  (:f dirty-flush-ms I64 "Interval in milliseconds for auto-flush checks")
  (:f airgap-enabled Bool "When true, blocks external network egress"))

(dfs ConfigHierarchy
  (:f user-config-path (Option Str) "User-level global configuration file")
  (:f workspace-config-path (Option Str) "Root workspace configuration file")
  (:f subproject-config-path (Option Str) "Subproject or package-level configuration file")
  (:f active-gates (List I64) "Resolved active verification gate IDs")
  (:f skip-gates (List I64) "Resolved skipped gate IDs"))

(dfs BufferRecord
  (:f rel-path Str "Workspace-relative file path")
  (:f content Str "In-memory file content buffer")
  (:f is-dirty Bool "True if modified in RAM and pending disk flush")
  (:f last-access-epoch I64 "Unix timestamp of last read or write"))

(dfs DaemonState
  (:f config DaemonConfig "Active daemon operational parameters")
  (:f indexed-files-count I64 "Total indexed source files in memory")
  (:f symbols-count I64 "Total tracked symbol records in AST graph")
  (:f dirty-buffers-count I64 "Count of unpersisted in-memory buffers")
  (:f is-ready Bool "True if snapshot is loaded and socket is listening"))

(df make-daemon-config [(sock Str) (max-buf I64) (airgap Bool)] -> DaemonConfig
  :d "Constructs canonical daemon operational parameters."
  (DaemonConfig
    :socket-path sock
    :max-clean-buffers max-buf
    :lru-eviction-batch 100
    :dirty-flush-ms 5000
    :airgap-enabled airgap))

(df make-daemon-state [(cfg DaemonConfig)] -> DaemonState
  :d "Initializes an empty daemon state with specified configuration."
  (DaemonState
    :config cfg
    :indexed-files-count 0
    :symbols-count 0
    :dirty-buffers-count 0
    :is-ready false))

(df resolve-hierarchical-config [(user (Option Str)) (ws (Option Str)) (sub (Option Str))] -> ConfigHierarchy
  :d "Resolves cascading configuration paths across multi-level project structures."
  (ConfigHierarchy
    :user-config-path user
    :workspace-config-path ws
    :subproject-config-path sub
    :active-gates [1 2 3 4 5 6 7]
    :skip-gates []))

(df dispatch-rpc-op [(op Str) (target Str) (state DaemonState)] -> Str
  :d "Dispatches incoming RPC requests across daemon subsystem handlers."
  (cond
    ((= op "ping") "(:ok :pong)")
    ((= op "status") (str "(:ready " (.-is-ready state) ")"))
    ((= op "gate") "(:gate :running)")
    ((= op "search") (str "(:search :target \"" target "\")"))
    (true "(:err :unknown-op)")))

(df evict-lru-buffers [(total-clean I64) (limit I64)] -> I64
  :d "Calculates the number of clean buffers to evict to preserve memory ceiling."
  (if (> total-clean limit)
    (- total-clean limit)
    0))

(df untangle-step [(dx F64) (dy F64) (target-dist F64) (spring-k F64)] -> F64
  :d "Computes Hooke relaxation force for planar graph untangling."
  (let [(dist (sqrt (+ (* dx dx) (* dy dy))))]
    (if (= dist 0.0)
      0.0
      (* (- dist target-dist) spring-k))))
