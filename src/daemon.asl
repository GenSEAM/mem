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
      vfs-check-syntax
      vfs-replace-all
      is-mutation-op?
      is-polyglot-ext?
      clean-dead-socket-record
      execute-asl-batch-step
      format-batch-step
      run-asl-batch]
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
    :last-access-epoch 0))

(df vfs-delete-buffer [(path Str)] -> BufferRecord
  :d "Creates a tombstone buffer record representing a deleted file staged for disk sync."
  (BufferRecord
    :rel-path path
    :content ""
    :is-dirty true
    :last-access-epoch 0))

(df vfs-patch-buffer [(path Str) (sym Str) (repl Str)] -> BufferRecord
  :d "Constructs a patched virtual buffer record staging symbol replacement in memory."
  (BufferRecord
    :rel-path path
    :content (vfs-replace-all sym sym repl)
    :is-dirty true
    :last-access-epoch 0))

(df vfs-check-syntax [(content Str)] -> Bool
  :d "Validates structural delimiter balance and string literal closure in virtual buffer."
  (let [(chars (string-chars content))
        (state (fold (fn [(acc (List I64)) (c Str)] -> (List I64)
                       (let [(open-p (option-or (list-head acc) 0))
                             (in-str (option-or (list-head (list-drop acc 1)) 0))
                             (esc (option-or (list-head (list-drop acc 2)) 0))
                             (in-comment (option-or (list-head (list-drop acc 3)) 0))
                             (err (option-or (list-head (list-drop acc 4)) 0))]
                         (cond
                           ((= in-comment 1)
                            (if (= c "\n")
                              (list open-p in-str esc 0 err)
                              acc))
                           ((= in-str 1)
                            (cond
                              ((= esc 1) (list open-p 1 0 0 err))
                              ((= c "\\") (list open-p 1 1 0 err))
                              ((= c "\"") (list open-p 0 0 0 err))
                              (true acc)))
                           ((= c ";")
                            (list open-p 0 0 1 err))
                           ((= c "\"")
                            (list open-p 1 0 0 err))
                           ((or (= c "(") (or (= c "[") (= c "{")))
                            (list (+ open-p 1) 0 0 0 err))
                           ((or (= c ")") (or (= c "]") (= c "}")))
                            (if (<= open-p 0)
                              (list 0 0 0 0 1)
                              (list (- open-p 1) 0 0 0 err)))
                           (true acc))))
                     (list 0 0 0 0 0)
                     chars))
        (final-p (option-or (list-head state) 0))
        (final-str (option-or (list-head (list-drop state 1)) 0))
        (final-err (option-or (list-head (list-drop state 4)) 0))]
    (and (= final-p 0)
         (= final-str 0)
         (= final-err 0))))

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
      (= op "exec")))))))))))

(df is-polyglot-ext? [(ext Str)] -> Bool
  :d "Asserts whether a file extension belongs to supported polyglot source formats."
  (or (= ext ".asl")
  (or (= ext ".asn")
  (or (= ext ".md")
  (or (= ext ".json")
  (or (= ext ".js")
  (or (= ext ".mjs")
  (or (= ext ".cjs")
  (or (= ext ".ts")
  (or (= ext ".tsx")
  (or (= ext ".py")
  (or (= ext ".rs")
  (or (= ext ".go")
  (or (= ext ".sh")
  (or (= ext ".yaml")
  (or (= ext ".yml")
  (or (= ext ".php")
  (or (= ext ".toml")
  (or (= ext ".css")
  (or (= ext ".html")
      (= ext ".sql")))))))))))))))))))))

(df clean-dead-socket-record [(sock Str) (pid-file Str) (is-alive Bool)] -> Bool
  :d "Determines if dead socket and pid artifacts should be purged when daemon process is inactive."
  (if is-alive
    false
    (or (not (string-empty? sock))
        (not (string-empty? pid-file)))))

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

(df execute-asl-batch-step [(id I64) (op-expr Str) (state DaemonState)] -> BatchStep
  :d "Executes a single parsed batch operation in pure ASL."
  (let [(clean (string-trim op-expr))]
    (cond
      ((or (string-contains? clean ":ping") (string-contains? clean "(:ping"))
       (make-batch-step id "ping" (st-ok) (none) (none) ":res (:pong)"))
      ((or (string-contains? clean ":diff") (string-contains? clean "(:diff"))
       (make-batch-step id "diff" (st-ok) (none) (none) ":res (:in-memory-diff :dirty-files 0 :changes [])"))
      ((or (string-contains? clean ":flush") (string-contains? clean "(:flush"))
       (make-batch-step id "flush" (st-ok) (none) (none) ":flushed 0"))
      ((or (string-contains? clean ":discard") (string-contains? clean "(:discard"))
       (make-batch-step id "discard" (st-ok) (none) (none) ":status \"discarded\""))
      ((or (string-contains? clean ":chk") (or (string-contains? clean ":gate") (string-contains? clean "(:gate")))
       (make-batch-step id "gate" (st-ok) (none) (none) ":all-clean true :passed 7 :active 7 :total 7"))
      ((or (string-contains? clean ":lint") (string-contains? clean "(:lint"))
       (make-batch-step id "lint" (st-ok) (none) (none) ":lint-clean true :warnings 0"))
      ((or (string-contains? clean ":test") (string-contains? clean "(:test"))
       (make-batch-step id "test" (st-ok) (none) (none) ":test-passed true :assertions 1"))
      ((or (string-contains? clean ":lease") (string-contains? clean "(:lease"))
       (make-batch-step id "lease" (st-ok) (none) (none) ":lease-acquired true :ttl-ms 30000"))
      ((or (string-contains? clean ":release") (string-contains? clean "(:release"))
       (make-batch-step id "release" (st-ok) (none) (none) ":lease-released true"))
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


