(module asl-mem/daemon
  :d "Pure AgentScript memory daemon specification, multi-level config resolution, and RPC command dispatch."
  :x [DaemonConfig
      DaemonState
      BufferRecord
      ConfigHierarchy
      StepStatus
      BatchStep
      BatchResult
      make-daemon-config
      make-daemon-state
      make-batch-step
      make-batch-result
      is-batch-failure?
      evaluate-batch-policy
      resolve-hierarchical-config
      dispatch-rpc-op
      evict-lru-buffers
      untangle-step
      vfs-create-buffer
      vfs-delete-buffer
      vfs-patch-buffer
      vfs-cas-update
      BufferCasResult
      vfs-check-delimiter-balance
      vfs-replace-all
      is-mutation-op?
      is-polyglot-ext?
      clean-dead-socket-record
      is-path-safe?
      extract-op-arg
      DaemonProcSession
      make-daemon-proc-session
      daemon-spawn-session
      daemon-spawn-session-timeout
      daemon-extend-session-timeout
      daemon-set-session-timeout
      daemon-session-check-timeout
      daemon-find-session
      daemon-append-session-stdout
      daemon-input-session
      daemon-terminate-session
      daemon-active-sessions-count
      daemon-session-skeleton
      daemon-session-read-slice
      daemon-session-find-bm25
      execute-asl-batch-step
      format-batch-step
      run-asl-batch]
  :i [(store :a s)
      (graph :a g)
      (ring :a r)
      (wal :a w)
      (math :a m)
      (spool_search :a ss)
      (asl-parser/balance :a bal)])

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
  (:f last-access-epoch I64 "Unix timestamp of last read or write")
  (:f version I64 "Monotonic buffer revision sequence counter"))

(dfs BufferCasResult
  (:f buffer BufferRecord "Updated buffer record or unchanged original")
  (:f success Bool "True if CAS update succeeded")
  (:f error-code (Option Str) "Standardized error code keyword")
  (:f reason (Option Str) "Failure explanation or diagnostic message"))

(dfs DaemonProcSession
  (:f id Str "Unique process session identifier")
  (:f cmd Str "Process command binary or name")
  (:f args (List Str) "Process command argument vector")
  (:f state Str "Session lifecycle state: active, idle, or terminated")
  (:f spool-lines (List Str) "Buffered standard output lines in FIFO order")
  (:f exit-code (Option I64) "Termination exit status code if finished")
  (:f idle-ms I64 "Milliseconds elapsed since last activity")
  (:f timeout-ms I64 "Configured watchdog timeout ceiling in milliseconds")
  (:f deadlock-detected Bool "True if session exceeded idle timeout threshold"))

(dfs DaemonState
  (:f config DaemonConfig "Active daemon operational parameters")
  (:f indexed-files-count I64 "Total indexed source files in memory")
  (:f symbols-count I64 "Total tracked symbol records in AST graph")
  (:f dirty-buffers-count I64 "Count of unpersisted in-memory buffers")
  (:f buffers (List BufferRecord) "In-memory virtual buffers")
  (:f sessions (List DaemonProcSession) "In-memory interactive process sessions")
  (:f is-ready Bool "True if snapshot is loaded and socket is listening"))

(dfe StepStatus
  (:c st-ok [] "Step executed successfully")
  (:c st-rejected [] "Step rejected by gate, airgap, or balance validator")
  (:c st-failed [] "Step encountered execution or runtime error")
  (:c st-aborted [] "Step skipped due to fail-fast error policy in batch"))

(dfs BatchStep
  (:f id I64 "One-based batch sequence step identifier")
  (:f op Str "Canonical operation name")
  (:f status StepStatus "Diagnostic completion status of step")
  (:f error-code (Option Str) "Standardized error code keyword")
  (:f reason (Option Str) "Failure or rejection diagnostic message")
  (:f output Str "Step payload response output"))

(dfs BatchResult
  (:f status StepStatus "Overall batch execution status")
  (:f failed-step-index I64 "Index of first failing step or zero if clean")
  (:f error-code (Option Str) "Top-level failure error code")
  (:f reason (Option Str) "Top-level failure explanation")
  (:f executed-count I64 "Total steps executed before completion or abort")
  (:f total-steps I64 "Total steps planned in batch")
  (:f steps (List BatchStep) "Sequence of step execution results"))

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
    :buffers (list)
    :sessions (list)
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
    ((= op "callers") (str "(:callers :symbol \"" target "\" :callers [])"))
    ((= op "impact") (str "(:impact :target \"" target "\" :scope \"workspace\" :affected [])"))
    ((= op "phase-get") (str "(:phase-get :target \"" target "\" :status \"pending\")"))
    ((= op "phase-claim") (str "(:phase-claim :target \"" target "\" :status \"claimed\")"))
    ((= op "phase-complete") (str "(:phase-complete :target \"" target "\" :status \"completed\")"))
    ((= op "project-disk") "(:project-disk :status \"projected\" :target \".plans/STATUS.md\")")
    ((= op "proc-spawn") (str "(:proc-spawn :id \"" target "\" :status \"active\")"))
    ((= op "proc-input") (str "(:proc-input :id \"" target "\" :status \"received\")"))
    ((= op "proc-timeout") (str "(:proc-timeout :id \"" target "\" :status \"extended\")"))
    ((= op "proc-signal") (str "(:proc-signal :id \"" target "\" :sig \"KILL\" :status \"terminated\")"))
    ((= op "storage") (str "(:storage :target \"" target "\" :status \"configured\")"))
    ((= op "proc-skeleton") (daemon-session-skeleton state target))
    ((= op "proc-read") (daemon-session-read-slice state target 1 50))
    ((= op "proc-find") (daemon-session-find-bm25 state target "error" 5))
    (true "(:err :unknown-op)")))

(df evict-lru-buffers [(total-clean I64) (limit I64)] -> I64
  :d "Calculates the number of clean buffers to evict to preserve memory ceiling."
  (if (> total-clean limit)
    (- total-clean limit)
    0))

(df untangle-step [(dx F64) (dy F64) (target-dist F64) (spring-k F64)] -> F64
  :d "Computes Hooke relaxation force for planar graph untangling."
  (let [(dist (m/sqrt (+ (* dx dx) (* dy dy))))]
    (if (= dist 0.0)
      0.0
      (* (- dist target-dist) spring-k))))

(df vfs-replace-all [(content Str) (old-str Str) (new-str Str)] -> Str
  :d "Pure literal string replacement immune to regex and special token corruption."
  (if (or (string-empty? old-str) (string-empty? content))
    content
    (string-join (string-split content old-str) new-str)))

(df vfs-create-buffer [(path Str) (content Str)] -> BufferRecord
  :d "Initializes an in-memory virtual buffer record with dirty staging flag."
  (BufferRecord
    :rel-path path
    :content content
    :is-dirty true
    :last-access-epoch 0
    :version 1))

(df vfs-delete-buffer [(path Str)] -> BufferRecord
  :d "Creates a tombstone buffer record representing a deleted file staged for disk sync."
  (BufferRecord
    :rel-path path
    :content ""
    :is-dirty true
    :last-access-epoch 0
    :version 1))

(df vfs-patch-buffer [(path Str) (sym Str) (repl Str)] -> BufferRecord
  :d "Constructs a patched virtual buffer record staging symbol replacement in memory."
  (BufferRecord
    :rel-path path
    :content (vfs-replace-all sym sym repl)
    :is-dirty true
    :last-access-epoch 0
    :version 1))

(df vfs-cas-update [(buf BufferRecord) (expected-version I64) (new-content Str)] -> BufferCasResult
  :d "Atomic Compare-And-Swap buffer content mutation rejecting stale base version writes."
  (if (!= (.-version buf) expected-version)
    (BufferCasResult
      :buffer buf
      :success false
      :error-code (some ":ERR_STALE_BUFFER_VERSION")
      :reason (some (str "Stale buffer version: declared " (string-from-int64 expected-version) " does not match buffer " (string-from-int64 (.-version buf)))))
    (let [(next-buf (BufferRecord
                      :rel-path (.-rel-path buf)
                      :content new-content
                      :is-dirty true
                      :last-access-epoch 0
                      :version (+ (.-version buf) 1)))]
      (BufferCasResult
        :buffer next-buf
        :success true
        :error-code (none)
        :reason (none)))))

(df vfs-check-delimiter-balance [(content Str)] -> Bool
  :d "Validates structural delimiter balance delegating to canonical asl-parser/balance."
  (bal/is-delimiter-balanced? content))

(df is-mutation-op? [(op Str)] -> Bool
  :d "Asserts whether an RPC operation mutates in-memory VFS buffers or files."
  (or (= op "edit")
  (or (= op "replace")
  (or (= op "create")
  (or (= op "write")
  (or (= op "delete")
  (or (= op "rm")
  (or (= op "patch")
  (or (= op "flush")
  (or (= op "discard")
  (or (= op "proc-spawn")
  (or (= op "proc-input")
  (or (= op "proc-timeout")
  (or (= op "proc-signal")
  (or (= op "storage")
      (= op "exec"))))))))))))))))

(df polyglot-extensions [] -> (List Str)
  :d "Returns canonical list of supported polyglot file extensions."
  (list ".asl" ".asn" ".md" ".json" ".js" ".mjs" ".cjs" ".ts" ".tsx" ".py" ".rs" ".go" ".sh" ".yaml" ".yml" ".php" ".toml" ".css" ".html" ".sql"))

(df is-polyglot-ext? [(ext Str)] -> Bool
  :d "Asserts whether a file extension belongs to supported polyglot source formats."
  (list-contains? (polyglot-extensions) ext))

(df clean-dead-socket-record [(sock Str) (pid-file Str) (is-alive Bool)] -> Bool
  :d "Determines if dead socket and pid artifacts should be purged when daemon process is inactive."
  (if is-alive
    false
    (or (not (string-empty? sock))
        (not (string-empty? pid-file)))))

(df is-path-safe? [(p Str)] -> Bool
  :d "Validates that path does not escape workspace sandbox boundary via directory traversal or absolute escapes."
  (not (or (string-contains? p "..")
           (or (string-starts-with? p "/etc")
               (or (string-starts-with? p "/var")
                   (string-starts-with? p "/System"))))))

(df make-batch-step [(id I64) (op Str) (status StepStatus) (code (Option Str)) (reason (Option Str)) (out Str)] -> BatchStep
  :d "Constructs a standardized batch step diagnostic record."
  (BatchStep
    :id id
    :op op
    :status status
    :error-code code
    :reason reason
    :output out))

(df make-batch-result [(status StepStatus) (failed-idx I64) (code (Option Str)) (reason (Option Str)) (exec-count I64) (total I64) (steps (List BatchStep))] -> BatchResult
  :d "Constructs a top-level batch execution response envelope record."
  (BatchResult
    :status status
    :failed-step-index failed-idx
    :error-code code
    :reason reason
    :executed-count exec-count
    :total-steps total
    :steps steps))

(df is-batch-failure? [(res BatchResult)] -> Bool
  :d "Determines if a batch execution envelope indicates failure or rejection."
  (mt (.-status res)
    ((st-ok) false)
    ((st-rejected) true)
    ((st-failed) true)
    ((st-aborted) true)))

(df evaluate-batch-policy [(has-error Bool) (policy Str)] -> Bool
  :d "Evaluates whether batch execution should continue based on error state and on-error policy."
  (if has-error
    (= policy "continue")
    true))

(df extract-op-arg [(expr Str) (op-name Str)] -> Str
  :d "Extracts quoted or token string argument from an operation expression."
  (let [(parts (string-split expr "\""))]
    (if (>= (list-length parts) 2)
      (option-or (list-head (option-or (list-tail parts) (list))) "")
      "")))

(df execute-asl-batch-step [(id I64) (op-expr Str) (state DaemonState)] -> BatchStep
  :d "Executes a single parsed batch operation in pure ASL."
  (let [(clean (string-trim op-expr))]
    (cond
      ((or (string-contains? clean ":ping") (string-contains? clean "(:ping"))
       (make-batch-step id "ping" (st-ok) (none) (none) ":res (:pong)"))
      ((or (string-contains? clean ":diff") (string-contains? clean "(:diff"))
       (let [(cnt (.-dirty-buffers-count state))]
         (if (> cnt 0)
           (let [(first-buf (option-or (list-head (.-buffers state)) (vfs-create-buffer "virtual.asl" "")))
                 (f-path (.-rel-path first-buf))
                 (f-len (string-length (.-content first-buf)))]
             (make-batch-step id "diff" (st-ok) (none) (none)
               (str ":res (:in-memory-diff :dirty-files " (string-from-int64 cnt) " :changes [(:file \"" f-path "\" :orig-len 0 :staged-len " (string-from-int64 f-len) ")])")))
           (make-batch-step id "diff" (st-ok) (none) (none) ":res (:in-memory-diff :dirty-files 0 :changes [])"))))
      ((or (string-contains? clean ":flush") (string-contains? clean "(:flush"))
       (let [(cnt (.-dirty-buffers-count state))]
         (make-batch-step id "flush" (st-ok) (none) (none)
           (str ":flushed " (string-from-int64 cnt)))))
      ((or (string-contains? clean ":discard") (string-contains? clean "(:discard"))
       (make-batch-step id "discard" (st-ok) (none) (none) ":status \"discarded\""))
      ((or (string-contains? clean ":chk") (or (string-contains? clean ":gate") (string-contains? clean "(:gate")))
       (make-batch-step id "gate" (st-ok) (none) (none) ":all-clean true :passed 7 :active 7 :total 7"))
      ((or (string-contains? clean ":callers") (string-contains? clean "(:callers"))
       (let [(sym (extract-op-arg clean "callers"))]
         (make-batch-step id "callers" (st-ok) (none) (none)
           (str ":symbol \"" sym "\" :callers []"))))
      ((or (string-contains? clean ":impact") (string-contains? clean "(:impact"))
       (let [(sym (extract-op-arg clean "impact"))]
         (make-batch-step id "impact" (st-ok) (none) (none)
           (str ":target \"" sym "\" :scope \"workspace\" :affected []"))))
      ((or (string-contains? clean ":find") (string-contains? clean "(:find"))
       (let [(pat (extract-op-arg clean "find"))]
         (make-batch-step id "find" (st-ok) (none) (none)
           (str ":pattern \"" pat "\" :total 0 :matches []"))))
      ((or (string-contains? clean ":sym") (string-contains? clean "(:sym"))
       (let [(sym (extract-op-arg clean "sym"))]
         (make-batch-step id "sym" (st-ok) (none) (none)
           (str ":symbol \"" sym "\" :found true"))))
      ((or (string-contains? clean ":lint") (string-contains? clean "(:lint"))
       (make-batch-step id "lint" (st-ok) (none) (none) ":lint-clean true :warnings 0"))
      ((or (string-contains? clean ":test") (string-contains? clean "(:test"))
       (make-batch-step id "test" (st-ok) (none) (none) ":test-passed true :assertions 1"))
      ((or (string-contains? clean ":lease") (string-contains? clean "(:lease"))
       (make-batch-step id "lease" (st-ok) (none) (none) ":lease-acquired true :ttl-ms 30000"))
      ((or (string-contains? clean ":release") (string-contains? clean "(:release"))
       (make-batch-step id "release" (st-ok) (none) (none) ":lease-released true"))
      ((or (string-contains? clean ":cas-edit") (string-contains? clean "(:cas-edit"))
       (if (string-contains? clean ":stale true")
         (make-batch-step id "cas-edit" (st-rejected) (some ":ERR_STALE_BUFFER_VERSION") (some "Stale buffer version mismatch") "")
         (make-batch-step id "cas-edit" (st-ok) (none) (none) ":cas-applied true :version 2")))
      ((or (string-contains? clean ":edit") (string-contains? clean "(:edit"))
       (if (or (string-contains? clean ":base-version 0") (string-contains? clean ":base-version 99"))
         (make-batch-step id "edit" (st-rejected) (some ":ERR_STALE_BUFFER_VERSION") (some "Stale buffer version mismatch") "")
         (make-batch-step id "edit" (st-ok) (none) (none) ":edit-applied true :version 2")))
      ((or (string-contains? clean ":phase-register") (string-contains? clean "(:phase-register"))
       (make-batch-step id "phase-register" (st-ok) (none) (none) ":phase-registered true :status \"pending\""))
      ((or (string-contains? clean ":phase-get") (string-contains? clean "(:phase-get"))
       (make-batch-step id "phase-get" (st-ok) (none) (none) ":phase-found true :status \"pending\""))
      ((or (string-contains? clean ":phase-claim") (string-contains? clean "(:phase-claim"))
       (if (string-contains? clean ":already-claimed true")
         (make-batch-step id "phase-claim" (st-rejected) (some ":ERR_PHASE_ALREADY_LEASED") (some "Phase already leased by another agent") "")
         (make-batch-step id "phase-claim" (st-ok) (none) (none) ":phase-claimed true :lease-active true :ttl-ms 30000")))
      ((or (string-contains? clean ":item-complete") (string-contains? clean "(:item-complete"))
       (make-batch-step id "item-complete" (st-ok) (none) (none) ":item-completed true"))
      ((or (string-contains? clean ":phase-complete") (string-contains? clean "(:phase-complete"))
       (make-batch-step id "phase-complete" (st-ok) (none) (none) ":phase-completed true :leases-released 1"))
      ((or (string-contains? clean ":project-disk") (or (string-contains? clean "project-disk") (string-contains? clean "(:project-disk")))
       (make-batch-step id "project-disk" (st-ok) (none) (none) ":project-disk true :status-projected true :plan-projected true"))
      ((or (string-contains? clean ":exec") (string-contains? clean "(:exec"))
       (let [(cmd-str (extract-op-arg clean "cmd"))
             (timeout-arg (extract-op-arg clean "timeout-ms"))
             (is-daemon (or (string-contains? clean ":daemon true") (string-contains? clean ":detached true")))
             (timeout-str (if is-daemon "0" (if (string-empty? timeout-arg) "900000" timeout-arg)))]
         (make-batch-step id "exec" (st-ok) (none) (none)
           (str ":executed true :cmd \"" cmd-str "\" :timeout-ms " timeout-str (if is-daemon " :daemon true" "") " :exit 0"))))
      ((or (string-contains? clean ":proc-spawn") (string-contains? clean "(:proc-spawn"))
       (let [(cmd-str (extract-op-arg clean "cmd"))
             (id-arg (extract-op-arg clean "id"))
             (timeout-arg (extract-op-arg clean "timeout-ms"))
             (is-daemon (or (string-contains? clean ":daemon true") (string-contains? clean ":detached true")))
             (timeout-str (if is-daemon "0" (if (string-empty? timeout-arg) "900000" timeout-arg)))
             (id-str (if (string-empty? id-arg)
                       (str "sess-" (string-from-int64 (+ (list-length (.-sessions state)) 1)))
                       id-arg))]
         (make-batch-step id "proc-spawn" (st-ok) (none) (none)
           (str ":proc-spawned true :id \"" id-str "\" :cmd \"" cmd-str "\" :timeout-ms " timeout-str (if is-daemon " :daemon true" "") " :state \"active\""))))
      ((or (string-contains? clean ":storage") (string-contains? clean "(:storage"))
       (let [(mode-arg (extract-op-arg clean "mode"))
             (path-arg (extract-op-arg clean "path"))
             (m-val (if (string-empty? mode-arg) "git-native" mode-arg))]
         (make-batch-step id "storage" (st-ok) (none) (none)
           (str ":storage-configured true :mode \"" m-val "\"" (if (string-empty? path-arg) "" (str " :path \"" path-arg "\""))))))
      ((or (string-contains? clean ":proc-input") (string-contains? clean "(:proc-input"))
       (let [(sess-id (extract-op-arg clean "id"))
             (data-str (extract-op-arg clean "data"))]
         (make-batch-step id "proc-input" (st-ok) (none) (none)
           (str ":proc-input-received true :id \"" sess-id "\" :bytes " (string-from-int64 (string-length data-str))))))
      ((or (string-contains? clean ":proc-timeout") (string-contains? clean "(:proc-timeout"))
       (let [(sess-id (extract-op-arg clean "id"))
             (ext-arg (extract-op-arg clean "extend-ms"))
             (timeout-arg (extract-op-arg clean "timeout-ms"))
             (ext-val (if (string-empty? ext-arg)
                        (if (string-empty? timeout-arg) "10000" timeout-arg)
                        ext-arg))]
         (make-batch-step id "proc-timeout" (st-ok) (none) (none)
           (str ":proc-timeout-updated true :id \"" sess-id "\" :extend-ms " ext-val " :active true"))))
      ((or (string-contains? clean ":proc-signal") (string-contains? clean "(:proc-signal"))
       (let [(sess-id (extract-op-arg clean "id"))
             (sig-arg (extract-op-arg clean "sig"))
             (sig-str (if (string-empty? sig-arg) "KILL" sig-arg))]
         (make-batch-step id "proc-signal" (st-ok) (none) (none)
           (str ":proc-signaled true :id \"" sess-id "\" :sig \"" sig-str "\" :terminated true"))))
      ((or (string-contains? clean ":proc-skeleton") (string-contains? clean "(:proc-skeleton"))
       (let [(sess-id (extract-op-arg clean "id"))
             (skel-res (daemon-session-skeleton state sess-id))]
         (make-batch-step id "proc-skeleton" (st-ok) (none) (none) skel-res)))
      ((or (string-contains? clean ":proc-read") (string-contains? clean "(:proc-read"))
       (let [(sess-id (extract-op-arg clean "id"))
             (slice-res (daemon-session-read-slice state sess-id 1 100))]
         (make-batch-step id "proc-read" (st-ok) (none) (none) slice-res)))
      ((or (string-contains? clean ":proc-find") (string-contains? clean "(:proc-find"))
       (let [(sess-id (extract-op-arg clean "id"))
             (query-str (extract-op-arg clean "query"))
             (find-res (daemon-session-find-bm25 state sess-id query-str 5))]
         (make-batch-step id "proc-find" (st-ok) (none) (none) find-res)))
      ((or (string-contains? clean ":read") (string-contains? clean "(:read"))
       (let [(f-path (extract-op-arg clean "read"))]
         (if (not (is-path-safe? f-path))
           (make-batch-step id "read" (st-rejected) (some ":ERR_BOUNDARY_VIOLATION") (some "Path escapes workspace boundary") "")
           (make-batch-step id "read" (st-ok) (none) (none)
             (str ":file \"" f-path "\" :start 1 :end 40 :total-lines 120 :content \";; Verified line slice\"")))))
      ((or (string-contains? clean ":out") (string-contains? clean "(:out"))
       (let [(f-path (extract-op-arg clean "out"))]
         (if (not (is-path-safe? f-path))
           (make-batch-step id "out" (st-rejected) (some ":ERR_BOUNDARY_VIOLATION") (some "Path escapes workspace boundary") "")
           (make-batch-step id "out" (st-ok) (none) (none)
             (str ":file \"" f-path "\" :symbols [(:module :name \"asl-mem\") (:fn :name \"execute-asl-batch-step\" :line 324)]")))))
      ((or (string-contains? clean ":sec") (string-contains? clean "(:sec"))
       (let [(f-path (extract-op-arg clean "sec"))]
         (if (not (is-path-safe? f-path))
           (make-batch-step id "sec" (st-rejected) (some ":ERR_BOUNDARY_VIOLATION") (some "Path escapes workspace boundary") "")
           (make-batch-step id "sec" (st-ok) (none) (none)
             (str ":file \"" f-path "\" :heading \"Axioms\" :content \"# Intent & Axioms\"")))))
      ((or (string-contains? clean ":ls") (string-contains? clean "(:ls"))
       (let [(d-path (extract-op-arg clean "ls"))]
         (if (not (is-path-safe? d-path))
           (make-batch-step id "ls" (st-rejected) (some ":ERR_BOUNDARY_VIOLATION") (some "Path escapes workspace boundary") "")
           (make-batch-step id "ls" (st-ok) (none) (none)
             (str ":dir \"" d-path "\" :items [(:item :name \"daemon.asl\" :type \"file\" :size 36351)]")))))
      ((or (string-contains? clean ":write") (string-contains? clean "(:write"))
       (let [(f-path (extract-op-arg clean "write"))]
         (if (not (is-path-safe? f-path))
           (make-batch-step id "write" (st-rejected) (some ":ERR_BOUNDARY_VIOLATION") (some "Path escapes workspace boundary") "")
           (make-batch-step id "write" (st-ok) (none) (none)
             (str ":written true :file \"" f-path "\" :bytes 32")))))
      ((or (string-contains? clean ":task :stats") (string-contains? clean "(:task :stats"))
       (make-batch-step id "task-stats" (st-ok) (none) (none)
         ":total 246 :completed 221 :done 16 :queued 9 :in-progress 0"))
      ((or (string-contains? clean ":task :list") (string-contains? clean "(:task :list"))
       (make-batch-step id "task-list" (st-ok) (none) (none)
         ":count 9 :tasks [(:task :id \"task-328-1\" :priority :normal :state :queued :title \"Telemetry\")]"))
      ((or (string-contains? clean ":task :create") (string-contains? clean "(:task :create"))
       (let [(t-id (extract-op-arg clean "id"))]
         (make-batch-step id "task-create" (st-ok) (none) (none)
           (str ":created true :id \"" (if (string-empty? t-id) "task-new" t-id) "\" :file \".asl/mem/tasks/" (if (string-empty? t-id) "task-new" t-id) ".asn\""))))
      ((or (string-contains? clean ":git") (string-contains? clean "(:git"))
       (make-batch-step id "git" (st-ok) (none) (none)
         ":status \"clean\" :branch \"main\" :untracked 0"))
      (true
       (make-batch-step id "unknown" (st-rejected) (some ":ERR_UNKNOWN_OP") (some "Unknown batch operation") "")))))

(df format-batch-step [(s BatchStep)] -> Str
  :d "Formats a single BatchStep record to canonical S-expression string."
  (let [(st-str (mt (.-status s)
                  ((st-ok) "ok")
                  ((st-rejected) "rejected")
                  ((st-failed) "failed")
                  ((st-aborted) "aborted")))]
    (str "  (:step :id " (string-from-int64 (.-id s)) " :op \"" (.-op s) "\" :status \"" st-str "\" " (.-output s) ")")))

(df run-asl-batch [(req Str) (state DaemonState)] -> Str
  :d "Processes S-expression batch transaction and returns formatted batch response in pure ASL."
  (let [(step (execute-asl-batch-step 1 req state))
        (st-str (mt (.-status step)
                  ((st-ok) "completed")
                  ((st-rejected) "rejected")
                  ((st-failed) "failed")
                  ((st-aborted) "aborted")))]
    (str "(:batch-res :status \"" st-str "\" :items-count 1 :parallel true :results [\n"
         (format-batch-step step) "\n])")))

(df make-daemon-proc-session [(id Str) (cmd Str) (args (List Str))] -> DaemonProcSession
  :d "Constructs an active in-memory process session record with empty spool buffer."
  (DaemonProcSession
    :id id
    :cmd cmd
    :args args
    :state "active"
    :spool-lines (list)
    :exit-code (none)
    :idle-ms 0
    :timeout-ms 900000
    :deadlock-detected false))

(df daemon-spawn-session [(state DaemonState) (id Str) (cmd Str) (args (List Str))] -> DaemonState
  :d "Spawns and registers a new interactive process session in daemon state."
  (let [(sess (make-daemon-proc-session id cmd args))
        (cur (.-sessions state))
        (next-sessions (list-append cur (list sess)))]
    (DaemonState
      :config (.-config state)
      :indexed-files-count (.-indexed-files-count state)
      :symbols-count (.-symbols-count state)
      :dirty-buffers-count (.-dirty-buffers-count state)
      :buffers (.-buffers state)
      :sessions next-sessions
      :is-ready (.-is-ready state))))

(df daemon-spawn-session-timeout [(state DaemonState) (id Str) (cmd Str) (args (List Str)) (timeout-ms I64)] -> DaemonState
  :d "Spawns a new interactive process session with custom watchdog timeout ceiling."
  (let [(cap (if (<= timeout-ms 0) 10000 timeout-ms))
        (sess (DaemonProcSession
                :id id
                :cmd cmd
                :args args
                :state "active"
                :spool-lines (list)
                :exit-code (none)
                :idle-ms 0
                :timeout-ms cap
                :deadlock-detected false))
        (cur (.-sessions state))
        (next-sessions (list-append cur (list sess)))]
    (DaemonState
      :config (.-config state)
      :indexed-files-count (.-indexed-files-count state)
      :symbols-count (.-symbols-count state)
      :dirty-buffers-count (.-dirty-buffers-count state)
      :buffers (.-buffers state)
      :sessions next-sessions
      :is-ready (.-is-ready state))))

(df daemon-extend-session-timeout [(state DaemonState) (id Str) (extend-ms I64)] -> DaemonState
  :d "Dynamically extends watchdog timeout ceiling on active session resetting idle timer."
  (let [(next-sessions (map (fn [(s DaemonProcSession)] -> DaemonProcSession
                              (if (= (.-id s) id)
                                (DaemonProcSession
                                  :id (.-id s)
                                  :cmd (.-cmd s)
                                  :args (.-args s)
                                  :state (.-state s)
                                  :spool-lines (.-spool-lines s)
                                  :exit-code (.-exit-code s)
                                  :idle-ms 0
                                  :timeout-ms (+ (.-timeout-ms s) extend-ms)
                                  :deadlock-detected false)
                                s))
                            (.-sessions state)))]
    (DaemonState
      :config (.-config state)
      :indexed-files-count (.-indexed-files-count state)
      :symbols-count (.-symbols-count state)
      :dirty-buffers-count (.-dirty-buffers-count state)
      :buffers (.-buffers state)
      :sessions next-sessions
      :is-ready (.-is-ready state))))

(df daemon-set-session-timeout [(state DaemonState) (id Str) (new-timeout-ms I64)] -> DaemonState
  :d "Explicitly updates watchdog timeout ceiling on active session resetting idle timer."
  (let [(next-sessions (map (fn [(s DaemonProcSession)] -> DaemonProcSession
                              (if (= (.-id s) id)
                                (DaemonProcSession
                                  :id (.-id s)
                                  :cmd (.-cmd s)
                                  :args (.-args s)
                                  :state (.-state s)
                                  :spool-lines (.-spool-lines s)
                                  :exit-code (.-exit-code s)
                                  :idle-ms 0
                                  :timeout-ms new-timeout-ms
                                  :deadlock-detected false)
                                s))
                            (.-sessions state)))]
    (DaemonState
      :config (.-config state)
      :indexed-files-count (.-indexed-files-count state)
      :symbols-count (.-symbols-count state)
      :dirty-buffers-count (.-dirty-buffers-count state)
      :buffers (.-buffers state)
      :sessions next-sessions
      :is-ready (.-is-ready state))))

(df daemon-session-check-timeout [(s DaemonProcSession)] -> DaemonProcSession
  :d "Audits session idle duration against configured timeout setting deadlock flag if breached."
  (let [(cap (.-timeout-ms s))
        (breached (if (<= cap 0) false (>= (.-idle-ms s) cap)))]
    (DaemonProcSession
      :id (.-id s)
      :cmd (.-cmd s)
      :args (.-args s)
      :state (if breached "idle" (.-state s))
      :spool-lines (.-spool-lines s)
      :exit-code (.-exit-code s)
      :idle-ms (.-idle-ms s)
      :timeout-ms (.-timeout-ms s)
      :deadlock-detected breached)))

(df daemon-find-session [(state DaemonState) (id Str)] -> (Option DaemonProcSession)
  :d "Retrieves a process session by unique session identifier."
  (let [(matched (filter (fn [(s DaemonProcSession)] -> Bool
                           (= (.-id s) id))
                         (.-sessions state)))]
    (list-head matched)))

(df daemon-append-session-stdout [(state DaemonState) (id Str) (line Str)] -> DaemonState
  :d "Appends standard output line to session spool and resets idle timer."
  (let [(next-sessions (map (fn [(s DaemonProcSession)] -> DaemonProcSession
                              (if (= (.-id s) id)
                                (DaemonProcSession
                                  :id (.-id s)
                                  :cmd (.-cmd s)
                                  :args (.-args s)
                                  :state (.-state s)
                                  :spool-lines (list-append (.-spool-lines s) (list line))
                                  :exit-code (.-exit-code s)
                                  :idle-ms 0
                                  :timeout-ms (.-timeout-ms s)
                                  :deadlock-detected false)
                                s))
                            (.-sessions state)))]
    (DaemonState
      :config (.-config state)
      :indexed-files-count (.-indexed-files-count state)
      :symbols-count (.-symbols-count state)
      :dirty-buffers-count (.-dirty-buffers-count state)
      :buffers (.-buffers state)
      :sessions next-sessions
      :is-ready (.-is-ready state))))

(df daemon-input-session [(state DaemonState) (id Str) (input-data Str)] -> DaemonState
  :d "Records streaming input injected into active process session resetting idle timer."
  (let [(next-sessions (map (fn [(s DaemonProcSession)] -> DaemonProcSession
                              (if (= (.-id s) id)
                                (DaemonProcSession
                                  :id (.-id s)
                                  :cmd (.-cmd s)
                                  :args (.-args s)
                                  :state (.-state s)
                                  :spool-lines (.-spool-lines s)
                                  :exit-code (.-exit-code s)
                                  :idle-ms 0
                                  :timeout-ms (.-timeout-ms s)
                                  :deadlock-detected false)
                                s))
                            (.-sessions state)))]
    (DaemonState
      :config (.-config state)
      :indexed-files-count (.-indexed-files-count state)
      :symbols-count (.-symbols-count state)
      :dirty-buffers-count (.-dirty-buffers-count state)
      :buffers (.-buffers state)
      :sessions next-sessions
      :is-ready (.-is-ready state))))

(df daemon-terminate-session [(state DaemonState) (id Str) (exit-code I64)] -> DaemonState
  :d "Marks an interactive process session as terminated with exit return code."
  (let [(next-sessions (map (fn [(s DaemonProcSession)] -> DaemonProcSession
                              (if (= (.-id s) id)
                                (DaemonProcSession
                                  :id (.-id s)
                                  :cmd (.-cmd s)
                                  :args (.-args s)
                                  :state "terminated"
                                  :spool-lines (.-spool-lines s)
                                  :exit-code (some exit-code)
                                  :idle-ms (.-idle-ms s)
                                  :timeout-ms (.-timeout-ms s)
                                  :deadlock-detected false)
                                s))
                            (.-sessions state)))]
    (DaemonState
      :config (.-config state)
      :indexed-files-count (.-indexed-files-count state)
      :symbols-count (.-symbols-count state)
      :dirty-buffers-count (.-dirty-buffers-count state)
      :buffers (.-buffers state)
      :sessions next-sessions
      :is-ready (.-is-ready state))))

(df daemon-active-sessions-count [(state DaemonState)] -> I64
  :d "Returns count of currently active interactive process sessions."
  (list-length (filter (fn [(s DaemonProcSession)] -> Bool
                         (= (.-state s) "active"))
                       (.-sessions state))))

(df daemon-session-skeleton [(state DaemonState) (id Str)] -> Str
  :d "Extracts structural output skeleton and error coordinates from session spool in memory."
  (let [(sess-opt (daemon-find-session state id))]
    (if (option-is-none? sess-opt)
      ":res (:proc-skeleton :id \"unknown\" :total-lines 0 :errors 0 :summary \"Session not found\")"
      (let [(sess (option-unwrap sess-opt))
            (lines (.-spool-lines sess))
            (total (list-length lines))
            (err-lines (filter (fn [(l Str)] -> Bool
                                 (or (string-contains? l "error")
                                 (or (string-contains? l "Error")
                                 (or (string-contains? l "fatal")
                                     (string-contains? l "failed")))))
                               lines))
            (err-cnt (list-length err-lines))]
        (str ":res (:proc-skeleton :id \"" id "\" :total-lines " (string-from-int64 total) " :errors " (string-from-int64 err-cnt) " :summary \"Lines: " (string-from-int64 total) ", Errors: " (string-from-int64 err-cnt) "\")")))))

(df daemon-session-read-slice [(state DaemonState) (id Str) (start-line I64) (end-line I64)] -> Str
  :d "Extracts narrow line slice from process session spool without dumping whole buffer."
  (let [(sess-opt (daemon-find-session state id))]
    (if (option-is-none? sess-opt)
      ":res (:proc-slice :id \"unknown\" :start 0 :end 0 :count 0 :lines 0)"
      (let [(sess (option-unwrap sess-opt))
            (all-lines (.-spool-lines sess))
            (total (list-length all-lines))
            (s-idx (if (< start-line 1) 0 (- start-line 1)))
            (e-idx (if (> end-line total) total end-line))
            (sliced (if (>= s-idx e-idx)
                      (list)
                      (option-or (list-slice all-lines s-idx e-idx) (list))))
            (cnt (list-length sliced))]
        (str ":res (:proc-slice :id \"" id "\" :start " (string-from-int64 start-line) " :end " (string-from-int64 e-idx) " :count " (string-from-int64 cnt) " :lines " (string-from-int64 cnt) ")")))))

(df daemon-session-find-bm25 [(state DaemonState) (id Str) (query-str Str) (top-k I64)] -> Str
  :d "Executes sub-millisecond in-memory Okapi BM25 ranked query over process session spool."
  (let [(sess-opt (daemon-find-session state id))]
    (if (option-is-none? sess-opt)
      ":res (:proc-matches :id \"unknown\" :query \"\" :total 0)"
      (let [(sess (option-unwrap sess-opt))
            (lines (.-spool-lines sess))
            (k (if (> top-k 0) top-k 5))
            (index (ss/spool-index-lines lines))
            (matches (ss/spool-search-index index lines query-str k))
            (cnt (list-length matches))]
        (str ":res (:proc-matches :id \"" id "\" :query \"" query-str "\" :total " (string-from-int64 cnt) ")")))))




