(module asl-mem/tests/task-modular-test
  :d "Unit verification suite for task/* and proc/* 1-to-2 token modular aliases per d46 and d47."
  :x [run-tests
      test-task-claim-and-state
      test-task-settle
      test-proc-status
      test-proc-list]
  :i [(tasks :a task)
      (asl-sh/process :a proc)
      (asl-sh/apm :a apm)])

(df test-task-claim-and-state [] -> Bool
  :d "Verifies task/claim transitions task to claimed state and task/state reads state accurately."
  (let [(t0 (task/make-task "t1" "p1" "Test Task" "main" "planned" "normal" 1000 (list "src/a.asl") (list) "asl check" "why1"))
        (initial-state (task/state t0))
        (t1 (task/claim t0 2000))
        (claimed-state (task/state t1))]
    (assert (= initial-state "planned") "Initial task state must be planned")
    (assert (= claimed-state "claimed") "task/claim must transition task state to claimed")
    (assert (= (.-updated-at t1) 2000) "Claimed task updated-at must reflect claim epoch")
    (assert (= (.-id t1) "t1") "Task id must be preserved across claim")
    true))

(df test-task-settle [] -> Bool
  :d "Verifies task/settle transitions task to completed state, updates timestamp, and attaches receipt."
  (let [(t0 (task/make-task "t2" "p1" "Task Settle" "main" "planned" "normal" 1000 (list "src/b.asl") (list) "asl test" "why2"))
        (t1 (task/claim t0 2000))
        (receipt "(:receipt :exit-code 0 :asserts 10)")
        (t2 (task/settle t1 receipt 3000))
        (settled-state (task/state t2))]
    (assert (= settled-state "completed") "task/settle must transition task to completed state")
    (assert (= (.-completed-at t2) 3000) "task/settle completed-at must match completion epoch")
    (assert (= (list-length (.-receipts t2)) 1) "Settled task must have exactly 1 attached receipt")
    (assert (= (option-or (list-get (.-receipts t2) 0) "") receipt) "Attached receipt must match settled payload")
    true))

(df test-proc-status [] -> Bool
  :d "Verifies proc/status supervision verdict for within-deadline and timed-out steps."
  (let [(v-ok (proc/status 50 1000 "build-step"))
        (v-timeout (proc/status 15000 10000 "hanging-step"))]
    (assert (= (.-status v-ok) ":ok") "Step within deadline must have :ok status")
    (assert (= (.-exit-code v-ok) 0) "Step within deadline must have exit-code 0")
    (assert (not (.-worker-recycled v-ok)) "Step within deadline must not recycle worker")
    (assert (= (.-status v-timeout) ":recycled") "Exceeded deadline must have :recycled status")
    (assert (= (.-exit-code v-timeout) 124) "Exceeded deadline must emit exit code 124")
    (assert (.-worker-recycled v-timeout) "Exceeded deadline must flag worker as recycled")
    true))

(df test-proc-list [] -> Bool
  :d "Verifies proc/list process table rendering for empty and populated daemon registries."
  (let [(e1 (apm/make-daemon-entry "d-abc12" 4510 128 ":active" "step-eval"))
        (empty-tbl (proc/list (list)))
        (pop-tbl (proc/list (list e1)))]
    (assert (string-contains? empty-tbl "DAEMON ID") "Empty process list table must render header")
    (assert (string-contains? pop-tbl "DAEMON ID") "Populated table must contain header")
    (assert (string-contains? pop-tbl "d-abc12") "Populated table must contain daemon identifier")
    (assert (string-contains? pop-tbl "4510") "Populated table must contain pid")
    (assert (string-contains? pop-tbl ":active") "Populated table must contain daemon status")
    true))

(df run-tests [] -> Bool
  :d "Runs all task and process modular alias tests."
  (and (test-task-claim-and-state)
       (and (test-task-settle)
            (and (test-proc-status)
                 (test-proc-list)))))
