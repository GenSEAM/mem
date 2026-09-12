(module asl-mem/tests/task-recovery-test
  :d "Unit verification suite for checkpointed action DAG step recovery protocol."
  :x [TestCheckpointIntegrityValidation
      TestExtractHandoffState
      TestResumeActionDag
      TestRecoverTaskSessionSnapshot
      TestRecoveryAsnSerialization
      TestRecoveryModularAliases
      run-tests]
  :i [(tasks_store :a t)
      (task_recovery :a rec)])

(df make-sample-task [(id Str) (idx I64) (handoff Str) (sess-id Str)] -> t/TaskRecord
  :d "Constructs a sample TaskRecord fixture for recovery verification."
  (t/TaskRecord
    :id id
    :parent-id ""
    :phase "phase-397"
    :title "Sample DAG Task"
    :kind "feature"
    :lane "agent-core"
    :state "in-progress"
    :priority "high"
    :owner-role "implementer"
    :created-at 1000
    :updated-at 2000
    :started-at 1500
    :completed-at 0
    :owns (list "mem/src/a.asl")
    :target-symbols (list "sym-a")
    :related-symbols (list)
    :depends-on (list)
    :gate "asl check"
    :why "Verification test fixture"
    :spec "Execute 4-step action DAG"
    :acceptance-criteria (list "exit-code 0")
    :step-index idx
    :action-dag (list "Step 0: parse AST" "Step 1: check types" "Step 2: run tests" "Step 3: settle")
    :handoff-context handoff
    :receipts (list)
    :session-id sess-id
    :lease-expires-at 15000
    :effort "s"
    :risk "low"
    :root-cause ""
    :consequences (list)
    :drawbacks (list)))

(df TestCheckpointIntegrityValidation [] -> Bool
  :d "Verifies boundary checks for checkpoint integrity under valid and invalid states."
  (let [(t-valid-start (make-sample-task "task-valid-0" 0 "" "sess-1"))
        (t-valid-mid (make-sample-task "task-valid-2" 2 "(:state :parsed)" "sess-1"))
        (t-valid-end (make-sample-task "task-valid-4" 4 "(:state :done)" "sess-1"))
        (t-neg-idx (make-sample-task "task-neg" -1 "" "sess-1"))
        (t-oob-idx (make-sample-task "task-oob" 5 "" "sess-1"))
        (t-corrupt (make-sample-task "task-bad" 2 "CORRUPTED_STREAM_FAIL" "sess-1"))
        (t-empty-id (make-sample-task "" 1 "(:ok)" "sess-1"))]
    (assert (rec/verify-checkpoint-integrity t-valid-start) "Start step 0 must be valid")
    (assert (rec/verify-checkpoint-integrity t-valid-mid) "Mid step 2 must be valid")
    (assert (rec/verify-checkpoint-integrity t-valid-end) "End step 4 must be valid")
    (assert (not (rec/verify-checkpoint-integrity t-neg-idx)) "Negative step-index must fail integrity")
    (assert (not (rec/verify-checkpoint-integrity t-oob-idx)) "Out-of-bounds step-index must fail integrity")
    (assert (not (rec/verify-checkpoint-integrity t-corrupt)) "Corrupted handoff must fail integrity")
    (assert (not (rec/verify-checkpoint-integrity t-empty-id)) "Empty task ID must fail integrity")
    true))

(df TestExtractHandoffState [] -> Bool
  :d "Verifies extraction of handoff context from intact and corrupt tasks."
  (let [(t-clean (make-sample-task "task-c1" 1 "(:ast-tree :clean)" "sess-1"))
        (t-empty (make-sample-task "task-c2" 0 "" "sess-1"))
        (t-bad (make-sample-task "task-c3" 1 "CORRUPTED_BLOB" "sess-1"))]
    (assert (= (rec/extract-handoff-state t-clean) "(:ast-tree :clean)") "Clean handoff state must match exactly")
    (assert (= (rec/extract-handoff-state t-empty) "") "Empty handoff state must return empty string")
    (assert (= (rec/extract-handoff-state t-bad) "") "Corrupted task handoff extraction must return empty string")
    true))

(df TestResumeActionDag [] -> Bool
  :d "Verifies slicing of action DAG from arbitrary step index offsets."
  (let [(dag (list "Step 0: parse AST" "Step 1: check types" "Step 2: run tests" "Step 3: settle"))
        (res-0 (rec/resume-action-dag dag 0))
        (res-2 (rec/resume-action-dag dag 2))
        (res-3 (rec/resume-action-dag dag 3))
        (res-4 (rec/resume-action-dag dag 4))
        (res-neg (rec/resume-action-dag dag -2))
        (res-oob (rec/resume-action-dag dag 10))]
    (assert (= (list-length res-0) 4) "Resuming from 0 must yield all 4 steps")
    (assert (= (list-length res-2) 2) "Resuming from 2 must yield remaining 2 steps")
    (assert (= (option-or (list-get res-2 0) "") "Step 2: run tests") "First resumed item at index 2 must be Step 2")
    (assert (= (option-or (list-get res-2 1) "") "Step 3: settle") "Second resumed item at index 2 must be Step 3")
    (assert (= (list-length res-3) 1) "Resuming from 3 must yield exactly 1 step")
    (assert (= (option-or (list-get res-3 0) "") "Step 3: settle") "Resumed item at index 3 must be Step 3")
    (assert (= (list-length res-4) 0) "Resuming from completion index 4 must yield empty list")
    (assert (= (list-length res-neg) 4) "Resuming from negative index must yield full DAG")
    (assert (= (list-length res-oob) 0) "Resuming beyond DAG length must yield empty list")
    true))

(df TestRecoverTaskSessionSnapshot [] -> Bool
  :d "Verifies recovery snapshot construction preserves previous session and step context."
  (let [(t (make-sample-task "task-397-02" 2 "(:vfs-checkpoint :rev 42)" "crashed-agent-alpha"))
        (snap (rec/recover-task-session t "replacement-agent-beta" 25000))]
    (assert (= (.-task-id snap) "task-397-02") "Snapshot task id must match original task")
    (assert (= (.-previous-session-id snap) "crashed-agent-alpha") "Snapshot previous session id must record crashed agent")
    (assert (= (.-recovered-session-id snap) "replacement-agent-beta") "Snapshot recovered session id must record replacement agent")
    (assert (= (.-step-index snap) 2) "Snapshot step index must be exactly preserved at 2")
    (assert (= (.-total-steps snap) 4) "Snapshot total steps must equal 4")
    (assert (= (list-length (.-remaining-steps snap)) 2) "Snapshot remaining steps length must be 2")
    (assert (= (option-or (list-get (.-remaining-steps snap) 0) "") "Step 2: run tests") "First pending step must be Step 2")
    (assert (= (.-handoff-state snap) "(:vfs-checkpoint :rev 42)") "Snapshot handoff state must be preserved")
    (assert (.-is-valid snap) "Snapshot is-valid flag must be true")
    (assert (= (.-recovered-at snap) 25000) "Snapshot recovered-at must match recovery epoch")
    true))

(df TestRecoveryAsnSerialization [] -> Bool
  :d "Verifies ASN serialization format for recovery snapshot."
  (let [(t (make-sample-task "task-397-02" 2 "(:vfs :clean)" "sess-old"))
        (snap (rec/recover-task-session t "sess-new" 30000))
        (asn-str (rec/format-recovery-asn snap))]
    (assert (string-contains? asn-str "(:recovery-snapshot") "Output must contain (:recovery-snapshot")
    (assert (string-contains? asn-str ":task-id \"task-397-02\"") "Output must contain task-id")
    (assert (string-contains? asn-str ":previous-session-id \"sess-old\"") "Output must contain previous session")
    (assert (string-contains? asn-str ":recovered-session-id \"sess-new\"") "Output must contain recovered session")
    (assert (string-contains? asn-str ":step-index 2") "Output must contain step-index 2")
    (assert (string-contains? asn-str ":total-steps 4") "Output must contain total-steps 4")
    (assert (string-contains? asn-str "Step 2: run tests") "Output must contain remaining step 2")
    (assert (string-contains? asn-str ":is-valid true") "Output must contain :is-valid true")
    (assert (string-contains? asn-str ":recovered-at 30000") "Output must contain recovered-at 30000")
    true))

(df TestRecoveryModularAliases [] -> Bool
  :d "Verifies 1-to-2 token modular aliases match primary function behavior."
  (let [(t (make-sample-task "task-397-mod" 1 "(:mod-state true)" "sess-orig"))
        (valid? (rec/verify t))
        (handoff (rec/extract t))
        (snap (rec/snapshot t "sess-rep" 40000))
        (rem (rec/resume (.-action-dag t) 1))
        (asn-str (rec/format-recovery snap))]
    (assert valid? "Modular verify must return true")
    (assert (= handoff "(:mod-state true)") "Modular extract must return handoff string")
    (assert (= (.-recovered-session-id snap) "sess-rep") "Modular snapshot must have recovered session")
    (assert (= (list-length rem) 3) "Modular resume must yield 3 steps")
    (assert (string-contains? asn-str "(:recovery-snapshot") "Modular format must serialize to ASN")
    true))

(df run-tests [] -> Bool
  :d "Runs all task recovery unit tests."
  (and (TestCheckpointIntegrityValidation)
       (and (TestExtractHandoffState)
            (and (TestResumeActionDag)
                 (and (TestRecoverTaskSessionSnapshot)
                      (and (TestRecoveryAsnSerialization)
                           (TestRecoveryModularAliases)))))))

(run-tests)
