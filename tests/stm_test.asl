(module asl-mem/tests/stm-test
  :d "Falsifiable verification test suite for Multi-File Software Transactional Memory with OCC"
  :x [run-tests
      TestStmBeginAndSnapshot
      TestStmWriteAndDirtyTracking
      TestStmAtomicCommitCleanTrunk
      TestStmConcurrentReconciliation
      TestStmCollisionAbortsAtomically
      TestStmRollback]
  :i [(stm :a s)])

(df TestStmBeginAndSnapshot [] -> Bool
  :d "Verifies transaction begins with active status and reads snapshot into read-set."
  (let [(tx0 (s/stm-begin "tx-101" "agent-alpha" 100))
        (tx1 (s/stm-read tx0 "src/module_a.asl" "(df f1 [] -> I64 10)"))
        (snap-opt (s/stm-get-snapshot tx1 "src/module_a.asl"))]
    (assert (= (.-status tx0) "active") "Transaction status must be active")
    (assert (= (.-tx-id tx0) "tx-101") "Transaction ID must match")
    (assert (= (.-agent-id tx0) "agent-alpha") "Agent ID must match")
    (mt snap-opt
      ((none) (assert false "Snapshot must exist after stm-read"))
      ((some snap)
       (assert (= (.-path snap) "src/module_a.asl") "Snapshot path must match")
       (assert (not (.-dirty snap)) "Snapshot must not be dirty after initial read")
       (assert (= (.-base-content snap) "(df f1 [] -> I64 10)") "Base content must match trunk")))
    true))

(df TestStmWriteAndDirtyTracking [] -> Bool
  :d "Verifies write stages dirty buffer and preserves original base content."
  (let [(tx0 (s/stm-begin "tx-102" "agent-beta" 101))
        (tx1 (s/stm-read tx0 "src/lib.asl" "(df x [] -> I64 1)"))
        (tx2 (s/stm-write tx1 "src/lib.asl" "(df x [] -> I64 999)"))
        (snap-opt (s/stm-get-snapshot tx2 "src/lib.asl"))
        (dirty (s/stm-dirty-files tx2))]
    (assert (= (list-length dirty) 1) "Must report exactly 1 dirty file")
    (assert (list-contains? dirty "src/lib.asl") "Dirty list must include src/lib.asl")
    (mt snap-opt
      ((none) (assert false "Snapshot must be present"))
      ((some snap)
       (assert (.-dirty snap) "Snapshot must be marked dirty after write")
       (assert (= (.-base-content snap) "(df x [] -> I64 1)") "Base content must remain unchanged")
       (assert (= (.-staged-content snap) "(df x [] -> I64 999)") "Staged content must contain new value")))
    true))

(df TestStmAtomicCommitCleanTrunk [] -> Bool
  :d "Verifies multi-file transaction commits atomically when trunk matches base."
  (let [(tx0 (s/stm-begin "tx-103" "agent-gamma" 102))
        (tx1 (s/stm-read tx0 "file1.asl" "(df a [] -> I64 1)"))
        (tx2 (s/stm-read tx1 "file2.asl" "(df b [] -> I64 2)"))
        (tx3 (s/stm-write tx2 "file1.asl" "(df a [] -> I64 100)"))
        (tx4 (s/stm-write tx3 "file2.asl" "(df b [] -> I64 200)"))
        (trunk-paths (list "file1.asl" "file2.asl"))
        (trunk-contents (list "file1.asl:(df a [] -> I64 1)" "file2.asl:(df b [] -> I64 2)"))
        (res (s/stm-validate-and-merge tx4 trunk-paths trunk-contents))]
    (assert (.-committed res) "Commit must succeed when trunk is clean")
    (assert (= (list-length (.-touched-files res)) 2) "Must touch both files")
    (assert (= (list-length (.-conflicted-files res)) 0) "Must have 0 conflicted files")
    (assert (= (list-length (.-reconciled-files res)) 0) "Zero files reconciled if clean")
    (assert (= (list-length (.-merged-contents res)) 2) "Must return merged contents for both files")
    true))

(df TestStmConcurrentReconciliation [] -> Bool
  :d "Verifies multi-file transaction auto-reconciles via AST merge when trunk modified adjacent function."
  (let [(base-code (str "(df f1 [] -> I64 1)\n\n(df f2 [] -> I64 2)"))
        (staged-code (str "(df f1 [] -> I64 999)\n\n(df f2 [] -> I64 2)"))
        (trunk-code (str "(df f1 [] -> I64 1)\n\n(df f2 [] -> I64 888)"))
        (tx0 (s/stm-begin "tx-104" "agent-delta" 103))
        (tx1 (s/stm-read tx0 "module.asl" base-code))
        (tx2 (s/stm-write tx1 "module.asl" staged-code))
        (trunk-paths (list "module.asl"))
        (trunk-contents (list (str "module.asl:" trunk-code)))
        (res (s/stm-validate-and-merge tx2 trunk-paths trunk-contents))]
    (assert (.-committed res) "Commit must succeed via automatic AST reconciliation")
    (assert (= (list-length (.-reconciled-files res)) 1) "Must report 1 reconciled file")
    (assert (list-contains? (.-reconciled-files res) "module.asl") "Reconciled list must include module.asl")
    (let [(merged-first (option-or (list-get (.-merged-contents res) 0) ""))]
      (assert (string-contains? merged-first "999") "Merged content must include transaction edit")
      (assert (string-contains? merged-first "888") "Merged content must include concurrent trunk edit"))
    true))

(df TestStmCollisionAbortsAtomically [] -> Bool
  :d "Verifies conflicting modification on same function aborts transaction atomically."
  (let [(base-code (str "(df f1 [] -> I64 1)\n\n(df f2 [] -> I64 2)"))
        (staged-code (str "(df f1 [] -> I64 111)\n\n(df f2 [] -> I64 2)"))
        (trunk-code (str "(df f1 [] -> I64 222)\n\n(df f2 [] -> I64 2)"))
        (tx0 (s/stm-begin "tx-105" "agent-epsilon" 104))
        (tx1 (s/stm-read tx0 "collision.asl" base-code))
        (tx2 (s/stm-write tx1 "collision.asl" staged-code))
        (trunk-paths (list "collision.asl"))
        (trunk-contents (list (str "collision.asl:" trunk-code)))
        (res (s/stm-validate-and-merge tx2 trunk-paths trunk-contents))]
    (assert (not (.-committed res)) "Commit must fail when unresolvable AST collision occurs")
    (assert (= (list-length (.-conflicted-files res)) 1) "Must report 1 conflicted file")
    (assert (list-contains? (.-conflicted-files res) "collision.asl") "Conflicted list must contain collision.asl")
    (mt (.-error-message res)
      ((none) (assert false "Error message must be populated on abort"))
      ((some msg) (assert (string-contains? msg "AST collision") "Error message must mention AST collision")))
    true))

(df TestStmRollback [] -> Bool
  :d "Verifies explicit transaction rollback sets status to aborted and records reason."
  (let [(tx0 (s/stm-begin "tx-106" "agent-zeta" 105))
        (tx1 (s/stm-rollback tx0 "Manual agent compensation"))]
    (assert (= (.-status tx1) "aborted") "Status must be aborted")
    (assert (= (list-length (.-compensations tx1)) 1) "Must contain 1 compensation entry")
    (assert (list-contains? (.-compensations tx1) "Manual agent compensation") "Compensation reason must match")
    true))

(df run-tests [] -> Bool
  :d "Executes all Software Transactional Memory test suites."
  (and (TestStmBeginAndSnapshot)
       (and (TestStmWriteAndDirtyTracking)
            (and (TestStmAtomicCommitCleanTrunk)
                 (and (TestStmConcurrentReconciliation)
                      (and (TestStmCollisionAbortsAtomically)
                           (TestStmRollback)))))))
