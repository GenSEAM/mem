(module asl-mem/tests/tasks-test
  :d "Unit verification suite for Holistic Task Protocol, TaskReceipt, ownership and blast-radius collision detection, in-flight handoff, and parallel wave scheduling."
  :x [test-task-construction-and-accessors
      test-task-ownership-and-intersection
      test-task-dependency-and-readiness
      test-task-parallel-eligibility
      test-task-lifecycle-transitions
      test-task-asn-formatting
      test-holistic-task-construction-and-accessors
      test-task-receipt-and-verification
      test-task-blast-radius-collision
      test-task-in-flight-handoff-and-steps
      test-task-holistic-asn-formatting
      test-task-session-leases-and-recovery
      test-post-action-queue-lifecycle
      test-gap-task-construction-and-dimensions
      test-cybernetic-task-schema-extensions
      run-tests]
  :i [(tasks :a t)
      (tasks_store :a ts)])

(df test-task-construction-and-accessors [] -> Bool
  :d "Verifies legacy TaskRecord construction and accessor fields."
  (let [(task (t/make-task
                "task-325-1"
                "phase-325"
                "Unified LLM Client"
                "harness"
                "queued"
                "high"
                1757321000
                (list "harness/src/llm_client.asl")
                (list "task-324-5")
                "asl test harness/tests/llm_client_test.asl"
                "Grounding dispatcher"))]
    (assert (= (.-id task) "task-325-1") "Task id must match")
    (assert (= (.-phase task) "phase-325") "Phase must match")
    (assert (= (.-title task) "Unified LLM Client") "Title must match")
    (assert (= (.-lane task) "harness") "Lane must match")
    (assert (= (.-state task) "queued") "Initial state must be queued")
    (assert (= (.-priority task) "high") "Priority must be high")
    (assert (= (.-created-at task) 1757321000) "Created-at must match")
    (assert (= (.-updated-at task) 1757321000) "Updated-at must equal created-at initially")
    (assert (= (list-length (.-owns task)) 1) "Owns list length must be 1")
    (assert (= (list-length (.-depends-on task)) 1) "Depends-on length must be 1")
    (assert (= (.-gate task) "asl test harness/tests/llm_client_test.asl") "Gate command must match")
    (assert (= (list-length (.-receipts task)) 0) "Receipts must be empty initially")
    true))

(df test-task-ownership-and-intersection [] -> Bool
  :d "Verifies file ownership inspection and mutual intersection detection."
  (let [(t1 (t/make-task "t1" "p1" "Task 1" "main" "queued" "high" 100
                         (list "mem/src/telemetry.asl" "mem/src/hydration.asl")
                         (list) "gate1" "why1"))
        (t2 (t/make-task "t2" "p1" "Task 2" "main" "queued" "high" 100
                         (list "harness/src/llm_client.asl")
                         (list) "gate2" "why2"))
        (t3 (t/make-task "t3" "p1" "Task 3" "main" "queued" "high" 100
                         (list "mem/src/telemetry.asl")
                         (list) "gate3" "why3"))
        (t4 (t/make-task "t4" "p1" "Task 4" "main" "queued" "high" 100
                         (list "-")
                         (list) "gate4" "why4"))
        (t5 (t/make-task "t5" "p1" "Task 5" "main" "queued" "high" 100
                         (list "-")
                         (list) "gate5" "why5"))]
    (assert (t/task-owns-file? t1 "mem/src/telemetry.asl") "t1 owns telemetry.asl")
    (assert (t/task-owns-file? t1 "./mem/src/telemetry.asl") "t1 owns ./mem/src/telemetry.asl via normalization")
    (assert (t/task-owns-file? t1 "/mem/src/telemetry.asl") "t1 owns /mem/src/telemetry.asl via normalization")
    (assert (not (t/task-owns-file? t1 "harness/src/llm_client.asl")) "t1 does not own llm_client.asl")
    (assert (not (t/task-owns-intersect? t1 t2)) "t1 and t2 have disjoint file sets")
    (assert (t/task-owns-intersect? t1 t3) "t1 and t3 both own telemetry.asl")
    (assert (= (list-length (t/task-overlap-files t1 t3)) 1) "Overlap count between t1 and t3 must be 1")
    (assert (not (t/task-owns-intersect? t4 t5)) "Dash placeholders must not be considered file collision")
    true))

(df test-task-dependency-and-readiness [] -> Bool
  :d "Verifies prerequisite dependency resolution and ready queue detection."
  (let [(t1 (t/make-task "t1" "p1" "Task 1" "main" "queued" "high" 100 (list) (list) "gate1" "why1"))
        (t2 (t/make-task "t2" "p1" "Task 2" "main" "queued" "high" 100 (list) (list "t1") "gate2" "why2"))
        (t3 (t/make-task "t3" "p1" "Task 3" "main" "queued" "high" 100 (list) (list "t1" "t2") "gate3" "why3"))]
    (assert (t/task-dependencies-satisfied? t1 (list)) "Task 1 has no dependencies and is satisfied")
    (assert (not (t/task-dependencies-satisfied? t2 (list))) "Task 2 requires t1 and is not satisfied initially")
    (assert (t/task-dependencies-satisfied? t2 (list "t1")) "Task 2 is satisfied once t1 is completed")
    (assert (not (t/task-dependencies-satisfied? t3 (list "t1"))) "Task 3 requires both t1 and t2")
    (assert (t/task-dependencies-satisfied? t3 (list "t1" "t2")) "Task 3 is satisfied once t1 and t2 are completed")
    (let [(all-tasks (list t1 t2 t3))
          (ready0 (t/task-find-ready all-tasks (list)))]
      (assert (= (list-length ready0) 1) "Initially only t1 is ready")
      (assert (= (.-id (option-or (list-head ready0) t2)) "t1") "Ready task must be t1")
      (let [(ready1 (t/task-find-ready all-tasks (list "t1")))]
        (assert (= (list-length ready1) 2) "Once t1 completes, t1 and t2 satisfy deps")
        true))))

(df test-task-parallel-eligibility [] -> Bool
  :d "Verifies parallel execution eligibility between tasks."
  (let [(t1 (t/make-task "t1" "p1" "Task 1" "main" "queued" "high" 100
                         (list "pkg1/src/a.asl") (list) "gate1" "why1"))
        (t2 (t/make-task "t2" "p1" "Task 2" "main" "queued" "high" 100
                         (list "pkg2/src/b.asl") (list) "gate2" "why2"))
        (t3 (t/make-task "t3" "p1" "Task 3" "main" "queued" "high" 100
                         (list "pkg1/src/a.asl") (list) "gate3" "why3"))
        (t4 (t/make-task "t4" "p1" "Task 4" "main" "queued" "high" 100
                         (list "pkg3/src/c.asl") (list "t1") "gate4" "why4"))]
    (assert (t/task-parallel-eligible? t1 t2) "t1 and t2 have disjoint files and no dependency - parallel eligible")
    (assert (not (t/task-parallel-eligible? t1 t3)) "t1 and t3 share pkg1/src/a.asl - not parallel eligible")
    (assert (not (t/task-parallel-eligible? t1 t4)) "t4 depends on t1 - not parallel eligible")
    (assert (not (t/task-parallel-eligible? t1 t1)) "Same task cannot be parallel to itself")
    true))

(df test-task-lifecycle-transitions [] -> Bool
  :d "Verifies claim, start, and complete lifecycle transitions."
  (let [(t0 (t/make-task "t1" "p1" "Task 1" "main" "queued" "high" 1000 (list "a.asl") (list) "gate1" "why1"))
        (t-claimed (t/task-claim t0 2000))
        (t-started (t/task-start t-claimed 2500))
        (t-done (t/task-complete t-started "exit 0: 10 assertions passed" 3000))]
    (assert (= (.-state t-claimed) "claimed") "State must be claimed")
    (assert (= (.-updated-at t-claimed) 2000) "Updated-at must be 2000")
    (assert (= (.-state t-started) "in-progress") "State must be in-progress")
    (assert (= (.-updated-at t-started) 2500) "Updated-at must be 2500")
    (assert (= (.-state t-done) "completed") "State must be completed")
    (assert (= (.-updated-at t-done) 3000) "Updated-at must be 3000")
    (assert (= (list-length (.-receipts t-done)) 1) "Receipts count must be 1")
    (assert (= (.-id t-done) "t1") "ID preserved across transitions")
    true))

(df test-task-asn-formatting [] -> Bool
  :d "Verifies formatting task to canonical ASN format."
  (let [(task (t/make-task "task-100-1" "phase-100" "Grammar Sync" "main" "completed" "high" 1757000000
                          (list "src/a.asl" "src/b.asl") (list "task-99-1") "asl gate" "Verification"))
        (asn (t/task-format-asn task))]
    (assert (string-contains? asn "(:task") "Must contain :task tag")
    (assert (string-contains? asn ":id \"task-100-1\"") "Must contain task ID")
    (assert (string-contains? asn ":phase \"phase-100\"") "Must contain phase")
    (assert (string-contains? asn ":createdAt 1757000000") "Must contain createdAt")
    (assert (string-contains? asn ":owns [\"src/a.asl\" \"src/b.asl\"]") "Must contain owns")
    (assert (string-contains? asn ":dependsOn [\"task-99-1\"]") "Must contain dependsOn")
    (assert (string-contains? asn ":gate \"asl gate\"") "Must contain gate")
    true))

(df test-holistic-task-construction-and-accessors [] -> Bool
  :d "Verifies holistic task record construction with explicit why, where, target paths, blast radius symbols, and action DAG."
  (let [(ht (t/make-holistic-task
              "task-382-01"
              "task-382-root"
              "phase-382"
              "Holistic Task Protocol Implementation"
              "code-mutation"
              "main"
              "queued"
              "high"
              "implementer"
              1773100000000
              (list "mem/src/tasks.asl" "mem/tests/tasks_test.asl")
              (list "make-holistic-task" "TaskReceipt" "task-advance-step")
              (list "task-owns-file?" "TaskStore")
              (list "task-381-05")
              "asl test --strict-falsify mem/tests/tasks_test.asl"
              "Canonical holistic task representation enforcing C0001, C0002, D0034"
              "Step 1: define struct; Step 2: author constructor; Step 3: verify with strict falsify"
              (list "dual-case >= 2 asserts" "exit 0" "zero comments C0001")
              (list "define struct" "author constructor" "verify with gate")))]
    (assert (= (.-id ht) "task-382-01") "Holistic task ID must match")
    (assert (= (.-parent-id ht) "task-382-root") "Parent ID must match")
    (assert (= (.-phase ht) "phase-382") "Phase must match")
    (assert (= (.-kind ht) "code-mutation") "Kind must be code-mutation")
    (assert (= (.-owner-role ht) "implementer") "Owner role must be implementer")
    (assert (= (.-step-index ht) 0) "Initial step index must be 0")
    (assert (= (.-handoff-context ht) "") "Initial handoff context must be empty")
    (assert (= (list-length (.-target-symbols ht)) 3) "Target symbols count must be 3")
    (assert (= (list-length (.-related-symbols ht)) 2) "Related symbols count must be 2")
    (assert (= (list-length (.-action-dag ht)) 3) "Action DAG length must be 3")
    (assert (= (list-length (.-acceptance-criteria ht)) 3) "Acceptance criteria count must be 3")
    (assert (string-contains? (.-why ht) "C0001") "Why must cite architectural invariant")
    true))

(df test-task-receipt-and-verification [] -> Bool
  :d "Verifies physical TaskReceipt creation, formatting, and strict validation rules."
  (let [(good-rc (t/make-task-receipt
                   0
                   42
                   18
                   32
                   10
                   0
                   (list "mem/src/tasks.asl")
                   "hash-abc-123"
                   (list "C0001" "C0002" "D0034")
                   "32 assertions evaluated cleanly"))
        (bad-rc-exit (t/make-task-receipt
                       1
                       50
                       20
                       30
                       9
                       1
                       (list "mem/src/tasks.asl")
                       "hash-abc-123"
                       (list "C0001")
                       "Command failed with exit 1"))
        (bad-rc-zero-asserts (t/make-task-receipt
                              0
                              10
                              15
                              0
                              0
                              0
                              (list)
                              "hash-none"
                              (list)
                              "Empty test output"))]
    (assert (t/receipt-is-verified? good-rc) "Clean receipt with exit 0 and asserts > 0 must be verified")
    (assert (not (t/receipt-is-verified? bad-rc-exit)) "Receipt with exit 1 must be rejected")
    (assert (not (t/receipt-is-verified? bad-rc-zero-asserts)) "Receipt with zero evaluated assertions must be rejected as vacuous")
    (let [(rc-asn (t/format-receipt-asn good-rc))]
      (assert (string-contains? rc-asn "(:receipt") "Serialized receipt must contain (:receipt tag")
      (assert (string-contains? rc-asn ":exit 0") "Serialized receipt must contain :exit 0")
      (assert (string-contains? rc-asn ":asserts 32") "Serialized receipt must contain asserts count")
      (assert (string-contains? rc-asn ":mutated [\"mem/src/tasks.asl\"]") "Serialized receipt must contain mutated files")
      (assert (string-contains? rc-asn ":hash \"hash-abc-123\"") "Serialized receipt must contain diff hash")
      true)))

(df test-task-blast-radius-collision [] -> Bool
  :d "Verifies detection of AST symbol and blast-radius caller collisions."
  (let [(t1 (t/make-holistic-task
              "t1" "" "p1" "Task 1" "code-mutation" "main" "queued" "normal" "implementer" 100
              (list "pkg1/file.asl")
              (list "symA" "symB")
              (list "caller1" "caller2")
              (list) "gate1" "why1" "spec1" (list) (list)))
        (t2-disjoint (t/make-holistic-task
                       "t2" "" "p1" "Task 2" "code-mutation" "main" "queued" "normal" "implementer" 100
                       (list "pkg2/other.asl")
                       (list "symX" "symY")
                       (list "callerX")
                       (list) "gate2" "why2" "spec2" (list) (list)))
        (t3-sym-collision (t/make-holistic-task
                            "t3" "" "p1" "Task 3" "code-mutation" "main" "queued" "normal" "implementer" 100
                            (list "pkg3/third.asl")
                            (list "symB")
                            (list)
                            (list) "gate3" "why3" "spec3" (list) (list)))
        (t4-blast-collision (t/make-holistic-task
                              "t4" "" "p1" "Task 4" "code-mutation" "main" "queued" "normal" "implementer" 100
                              (list "pkg4/fourth.asl")
                              (list "caller1")
                              (list)
                              (list) "gate4" "why4" "spec4" (list) (list)))]
    (assert (not (t/task-overlap-blast-radius? t1 t2-disjoint)) "Disjoint symbol tasks must not collide in blast radius")
    (assert (t/task-overlap-blast-radius? t1 t3-sym-collision) "Direct target symbol overlap symB must be detected as collision")
    (assert (t/task-overlap-blast-radius? t1 t4-blast-collision) "Target symbol touching related caller caller1 must be detected as blast radius collision")
    (assert (t/task-parallel-eligible? t1 t2-disjoint) "Disjoint tasks must be eligible for parallel execution")
    (assert (not (t/task-parallel-eligible? t1 t3-sym-collision)) "Symbol-colliding tasks must NOT be eligible for parallel execution")
    (assert (not (t/task-parallel-eligible? t1 t4-blast-collision)) "Blast-radius colliding tasks must NOT be eligible for parallel execution")
    true))

(df test-task-in-flight-handoff-and-steps [] -> Bool
  :d "Verifies in-flight execution step advancement, state handoff capture, and verified receipt attachment."
  (let [(task0 (t/make-holistic-task
                 "ht-1" "" "p382" "Active In-Flight Task" "code-mutation" "main" "queued" "high" "implementer" 1000
                 (list "a.asl") (list "sym1") (list) (list) "gate" "why" "spec" (list) (list "step 1" "step 2" "step 3")))
        (task-started (t/task-start task0 1500))
        (task-step1 (t/task-advance-step task-started 2000))
        (task-handoff (t/task-capture-handoff task-step1 "(:handoff :checkpoint \"cp-1\" :vars [\"k1\" \"v1\"])" 2500))
        (receipt (t/make-task-receipt 0 35 16 28 8 0 (list "a.asl") "hash-1" (list "C0001") "8 passed"))
        (task-completed (t/task-attach-receipt task-handoff receipt 3000))]
    (assert (= (.-state task-started) "in-progress") "Started task must be in-progress")
    (assert (= (.-started-at task-started) 1500) "Started-at timestamp must be 1500")
    (assert (= (.-step-index task-step1) 1) "Step index must advance to 1")
    (assert (= (.-updated-at task-step1) 2000) "Updated-at timestamp must be 2000")
    (assert (= (.-step-index task-handoff) 1) "Step index preserved across handoff")
    (assert (string-contains? (.-handoff-context task-handoff) "checkpoint \"cp-1\"") "Handoff context must contain serialized state snapshot")
    (assert (= (.-updated-at task-handoff) 2500) "Updated-at updated on handoff capture")
    (assert (= (.-state task-completed) "completed") "Task must transition to completed on valid receipt")
    (assert (= (.-completed-at task-completed) 3000) "Completed-at timestamp must be 3000")
    (assert (= (list-length (.-receipts task-completed)) 1) "Task must have 1 attached receipt")
    (assert (string-contains? (option-or (list-head (.-receipts task-completed)) "") ":asserts 28") "Attached receipt must contain physical assertion count")
    true))

(df test-task-holistic-asn-formatting [] -> Bool
  :d "Verifies full serialization of holistic task specification to ASN."
  (let [(ht (t/make-holistic-task
              "task-holistic-99"
              "task-holistic-parent"
              "phase-382"
              "Universal Task Serialization"
              "code-mutation"
              "main"
              "completed"
              "urgent"
              "planner"
              1773000000000
              (list "src/target.asl")
              (list "ExportedFunc")
              (list "DependentCaller")
              (list "task-prev-01")
              "asl gate"
              "Architectural verification"
              "Implement serialization"
              (list "criteria-1" "criteria-2")
              (list "action-1" "action-2")))
        (asn (t/task-format-holistic-asn ht))]
    (assert (string-contains? asn "(:task") "Must contain :task tag")
    (assert (string-contains? asn ":id \"task-holistic-99\"") "Must contain ID")
    (assert (string-contains? asn ":parentId \"task-holistic-parent\"") "Must contain parent-id")
    (assert (string-contains? asn ":kind :code-mutation") "Must contain kind")
    (assert (string-contains? asn ":ownerRole \"planner\"") "Must contain owner-role")
    (assert (string-contains? asn ":targetSymbols [\"ExportedFunc\"]") "Must contain target-symbols")
    (assert (string-contains? asn ":relatedSymbols [\"DependentCaller\"]") "Must contain related-symbols")
    (assert (string-contains? asn ":acceptanceCriteria [\"criteria-1\" \"criteria-2\"]") "Must contain acceptance-criteria")
    (assert (string-contains? asn ":actionDag [\"action-1\" \"action-2\"]") "Must contain action-dag")
    true))

(df test-task-session-leases-and-recovery [] -> Bool
  :d "Verifies session leases, crash recovery checkpoint preservation, and child task spawning."
  (let [(t0 (t/make-holistic-task
              "task-rec-1" "" "p388" "Session Lease Task" "code-mutation" "main" "queued" "high" "implementer" 1000
              (list "mem/src/tasks.asl") (list "task-recover-session") (list) (list) "gate" "why" "spec" (list) (list "step 1" "step 2")))
        (t-claimed (t/task-claim-session t0 "session-alpha" 5000 1000))]
    (assert (= (.-session-id t-claimed) "session-alpha") "Session ID must match claimed session")
    (assert (= (.-lease-expires-at t-claimed) 6000) "Lease expires at now + lease-ms")
    (assert (= (.-state t-claimed) "claimed") "State must be claimed")
    (assert (t/task-is-claimed-by? t-claimed "session-alpha") "Task must be claimed by session-alpha")
    (assert (not (t/task-is-claimed-by? t-claimed "session-beta")) "Task must not be claimed by session-beta")
    (assert (not (t/task-is-lease-expired? t-claimed 5999)) "Lease is not expired before deadline")
    (assert (t/task-is-lease-expired? t-claimed 6000) "Lease is expired at deadline")
    (assert (t/task-is-lease-expired? t-claimed 7000) "Lease is expired after deadline")
    (let [(t-started (t/task-start t-claimed 1500))
          (t-step1 (t/task-advance-step t-started 2000))
          (t-handoff (t/task-capture-handoff t-step1 "(:checkpoint \"step-1-saved\")" 2500))
          (t-recovered (t/task-recover-session t-handoff "session-beta" 5000 7000))]
      (assert (= (.-session-id t-recovered) "session-beta") "Recovered task must be claimed by session-beta")
      (assert (= (.-lease-expires-at t-recovered) 12000) "Recovered task lease expires at 12000")
      (assert (= (.-state t-recovered) "in-progress") "Recovered task transitions to in-progress")
      (assert (= (.-step-index t-recovered) 1) "Recovered task must preserve step-index")
      (assert (= (.-handoff-context t-recovered) "(:checkpoint \"step-1-saved\")") "Recovered task must preserve handoff context")
      (let [(child (t/task-spawn-subtask t-recovered "task-rec-1-sub1" "Discovered Child" (list "new/file.asl") 7500))]
        (assert (= (.-id child) "task-rec-1-sub1") "Spawned child ID must match")
        (assert (= (.-parent-id child) "task-rec-1") "Spawned child parent-id must match parent task")
        (assert (= (.-kind child) "task-spawn") "Spawned child kind must be task-spawn")
        (assert (= (.-state child) "queued") "Spawned child initial state must be queued")
        (assert (= (list-length (.-depends-on child)) 1) "Spawned child must depend on parent")
        (assert (= (option-or (list-head (.-depends-on child)) "") "task-rec-1") "Spawned child depends on parent ID")
        (assert (= (.-session-id child) "") "Spawned child must be unassigned initially")
        (let [(asn (t/task-format-holistic-asn t-recovered))]
          (assert (string-contains? asn ":sessionId \"session-beta\"") "Serialized task contains session-id")
          (assert (string-contains? asn ":leaseExpiresAt 12000") "Serialized task contains lease-expires-at")
          true)))))

(df test-post-action-queue-lifecycle [] -> Bool
  :d "Verifies PostAction construction, enqueueing, ASN formatting, and queue draining lifecycle."
  (let [(act1 (ts/make-post-action "post-426-01" "completed" "review" "phase-426" "Review model calibration receipts" "payload-calib-data"))
        (act2 (ts/make-post-action "post-426-02" "completed" "followup-plan" "phase-427" "Plan browser gallery deployment" "payload-gallery-spec"))
        (q0 (ts/make-post-action-queue "consequent-planning-queue"))
        (q1 (ts/post-action-enqueue q0 act1))
        (q2 (ts/post-action-enqueue q1 act2))]
    (assert (= (.-queue-id q0) "consequent-planning-queue") "Queue ID matches")
    (assert (= (list-length (.-actions q0)) 0) "Initial queue is empty")
    (assert (= (list-length (.-actions q1)) 1) "Queue has 1 action after first enqueue")
    (assert (= (list-length (.-actions q2)) 2) "Queue has 2 actions after second enqueue")
    (assert (= (.-action-id act1) "post-426-01") "Action ID matches")
    (assert (= (.-trigger-state act1) "completed") "Trigger state matches")
    (assert (= (.-kind act1) "review") "Kind matches")
    (assert (= (.-target-phase act1) "phase-426") "Target phase matches")
    (let [(act-asn (ts/format-post-action-asn act1))]
      (assert (string-contains? act-asn "(:post-action") "Serialized ASN contains :post-action tag")
      (assert (string-contains? act-asn ":id \"post-426-01\"") "Serialized ASN contains action ID")
      (assert (string-contains? act-asn ":trigger \"completed\"") "Serialized ASN contains trigger state")
      (assert (string-contains? act-asn ":kind \"review\"") "Serialized ASN contains kind")
      (let [(q-drained (ts/post-action-drain q2))]
        (assert (= (list-length (.-actions q-drained)) 0) "Actions list is empty after drain")
        (assert (= (.-processed-count q-drained) 2) "Processed count advances to 2 after drain")
        true))))

(df test-gap-task-construction-and-dimensions [] -> Bool
  :d "Verifies first-class GAP task construction, cybernetic dimensions, and canonical ASN serialization."
  (let [(gap (t/make-gap-task
               "gap-governance-01"
               "phase-403"
               "Monolithic GAPS Contention"
               "governance"
               "high"
               1773285000
               (list "mem/src/tasks.asl")
               "asl test mem/tests/tasks_test.asl"
               "C0001"
               "m"
               "medium"
               "Single monolithic file causes multi-agent concurrent write collisions"
               (list "Lock contention" "Merge conflicts")
               (list "Requires schema migration across task runners")))]
    (assert (= (.-id gap) "gap-governance-01") "Gap id matches")
    (assert (= (.-kind gap) "gap") "Gap entity kind must be gap")
    (assert (= (.-effort gap) "m") "Effort must be m")
    (assert (= (.-risk gap) "medium") "Risk must be medium")
    (assert (= (.-root-cause gap) "Single monolithic file causes multi-agent concurrent write collisions") "Root cause matches")
    (assert (= (list-length (.-consequences gap)) 2) "Consequences count must be 2")
    (assert (= (list-length (.-drawbacks gap)) 1) "Drawbacks count must be 1")
    (assert (= (.-state gap) "queued") "Initial state is queued")
    (let [(gap-asn (t/task-format-asn gap))]
      (assert (string-contains? gap-asn ":kind :gap") "ASN formatting includes :kind :gap")
      (assert (string-contains? gap-asn ":effort :m") "ASN formatting includes :effort :m")
      (assert (string-contains? gap-asn ":risk :medium") "ASN formatting includes :risk :medium")
      (assert (string-contains? gap-asn ":rootCause") "ASN formatting includes root cause")
      (assert (string-contains? gap-asn "Single monolithic file") "ASN contains root cause string")
      true)))

(df test-cybernetic-task-schema-extensions [] -> Bool
  :d "Verifies cybernetic task schema extensions, lifecycle state transitions, and anti-collision properties."
  (let [(ct (t/make-cybernetic-task
              "task-gov-02"
              "phase-403"
              "Cybernetic Task Protocol"
              "governance"
              "governance"
              "queued"
              "urgent"
              1773285000
              (list "mem/src/tasks_store.asl")
              (list)
              "asl check"
              "D0054"
              "s"
              "critical"
              "Schema deficiency"
              (list "Context bloat")
              (list "Backwards compatibility burden")))
        (t-disjoint (t/make-task "t-dis" "p1" "Disjoint" "main" "queued" "low" 100
                                 (list "harness/src/a.asl") (list) "gate" "why"))]
    (assert (= (.-effort ct) "s") "Cybernetic task effort is s")
    (assert (= (.-risk ct) "critical") "Cybernetic task risk is critical")
    (assert (not (t/task-owns-intersect? ct t-disjoint)) "Cybernetic task respects file boundary anti-collision")
    (let [(claimed (t/task-claim ct 1773286000))]
      (assert (= (.-state claimed) "claimed") "Claim transitions state")
      (assert (= (.-effort claimed) "s") "Claim preserves effort dimension")
      (assert (= (.-risk claimed) "critical") "Claim preserves risk dimension")
      (let [(started (t/task-start claimed 1773287000))]
        (assert (= (.-state started) "in-progress") "Start transitions state")
        (assert (= (.-effort started) "s") "Start preserves effort dimension")
        (let [(settled (t/task-complete started "(:receipt :exit 0 :asserts 5)" 1773288000))]
          (assert (= (.-state settled) "completed") "Complete transitions state")
          (assert (= (.-effort settled) "s") "Complete preserves effort dimension")
          (assert (= (list-length (.-receipts settled)) 1) "Attached receipt present")
          true)))))

(df run-tests [] -> Bool
  :d "Executes complete unit test suite for asl-mem/tasks."
  (do
    (assert (test-task-construction-and-accessors) "test-task-construction-and-accessors failed")
    (assert (test-task-ownership-and-intersection) "test-task-ownership-and-intersection failed")
    (assert (test-task-dependency-and-readiness) "test-task-dependency-and-readiness failed")
    (assert (test-task-parallel-eligibility) "test-task-parallel-eligibility failed")
    (assert (test-task-lifecycle-transitions) "test-task-lifecycle-transitions failed")
    (assert (test-task-asn-formatting) "test-task-asn-formatting failed")
    (assert (test-holistic-task-construction-and-accessors) "test-holistic-task-construction-and-accessors failed")
    (assert (test-task-receipt-and-verification) "test-task-receipt-and-verification failed")
    (assert (test-task-blast-radius-collision) "test-task-blast-radius-collision failed")
    (assert (test-task-in-flight-handoff-and-steps) "test-task-in-flight-handoff-and-steps failed")
    (assert (test-task-holistic-asn-formatting) "test-task-holistic-asn-formatting failed")
    (assert (test-task-session-leases-and-recovery) "test-task-session-leases-and-recovery failed")
    (assert (test-post-action-queue-lifecycle) "test-post-action-queue-lifecycle failed")
    (assert (test-gap-task-construction-and-dimensions) "test-gap-task-construction-and-dimensions failed")
    (assert (test-cybernetic-task-schema-extensions) "test-cybernetic-task-schema-extensions failed")
    (println "PASS (asl-mem/tasks: all assertions passed cleanly across legacy, holistic, cybernetic, and post-action task protocols)")
    true))
