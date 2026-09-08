(module asl-mem/tests/tasks-test
  :d "Unit verification suite for TaskRecord, ownership collision detection, dependency satisfaction, and parallel execution eligibility."
  :x [test-task-construction-and-accessors
      test-task-ownership-and-intersection
      test-task-dependency-and-readiness
      test-task-parallel-eligibility
      test-task-lifecycle-transitions
      test-task-asn-formatting
      run-tests]
  :i [(tasks :a t)])

(df test-task-construction-and-accessors [] -> Bool
  :d "Verifies TaskRecord construction and accessor fields."
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
    (assert (string-contains? asn ":created-at 1757000000") "Must contain created-at")
    (assert (string-contains? asn ":owns [\"src/a.asl\" \"src/b.asl\"]") "Must contain owns")
    (assert (string-contains? asn ":depends-on [\"task-99-1\"]") "Must contain depends-on")
    (assert (string-contains? asn ":gate \"asl gate\"") "Must contain gate")
    true))

(df run-tests [] -> Bool
  :d "Executes complete unit test suite for asl-mem/tasks."
  (do
    (assert (test-task-construction-and-accessors) "test-task-construction-and-accessors failed")
    (assert (test-task-ownership-and-intersection) "test-task-ownership-and-intersection failed")
    (assert (test-task-dependency-and-readiness) "test-task-dependency-and-readiness failed")
    (assert (test-task-parallel-eligibility) "test-task-parallel-eligibility failed")
    (assert (test-task-lifecycle-transitions) "test-task-lifecycle-transitions failed")
    (assert (test-task-asn-formatting) "test-task-asn-formatting failed")
    (println "PASS (asl-mem/tasks: all 26 assertions passed cleanly)")
    true))
