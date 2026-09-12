(module mem-worklist-test
  :d "Unit tests for cursor-driven persistent file worklist under ADR-0081."
  :x [run-tests
      test-worklist-creation
      test-cursor-bounded-batches
      test-mandatory-skip-reason
      test-marking-done-and-findings
      test-concurrent-walkers-no-collision
      test-worklist-asn-serialization]
  :i [(asl-mem/worklist :a wl)])

(df test-worklist-creation [] -> Bool
  :d "Verifies creation of worklist and initial entry state."
  (let [(e1 (wl/make-worklist-entry "src/a.asl" "audit ast"))
        (e2 (wl/make-worklist-entry "src/b.asl" "check exports"))
        (w (wl/worklist-create "wl-test-1" (list e1 e2)))]
    (assert (= (.-id w) "wl-test-1") "worklist ID must match")
    (assert (= (.-cursor w) 0) "initial cursor must be 0")
    (assert (= (list-length (.-entries w)) 2) "must contain 2 entries")
    (assert (= (.-status e1) "pending") "initial entry status must be pending")
    (assert (= (.-reason e1) "") "initial reason must be empty")
    (assert (not (wl/worklist-is-complete? w)) "worklist must not be complete initially")
    true))

(df test-cursor-bounded-batches [] -> Bool
  :d "Verifies that cursor yields bounded batches and advances monotonically."
  (let [(entries (list
                   (wl/make-worklist-entry "file1.asl" "intent1")
                   (wl/make-worklist-entry "file2.asl" "intent2")
                   (wl/make-worklist-entry "file3.asl" "intent3")
                   (wl/make-worklist-entry "file4.asl" "intent4")
                   (wl/make-worklist-entry "file5.asl" "intent5")))
        (w0 (wl/worklist-create "wl-batch" entries))
        (step1 (wl/worklist-cursor-next w0 2))
        (b1 (get step1 :batch))
        (w1 (get step1 :worklist))
        (step2 (wl/worklist-cursor-next w1 2))
        (b2 (get step2 :batch))
        (w2 (get step2 :worklist))
        (step3 (wl/worklist-cursor-next w2 2))
        (b3 (get step3 :batch))
        (w3 (get step3 :worklist))]
    (assert (= (list-length b1) 2) "batch 1 must yield 2 entries")
    (assert (= (get step1 :cursor) 2) "cursor must advance to 2")
    (assert (= (get step1 :has-more) true) "step 1 must have more")
    (assert (= (list-length b2) 2) "batch 2 must yield 2 entries")
    (assert (= (get step2 :cursor) 4) "cursor must advance to 4")
    (assert (= (get step2 :has-more) true) "step 2 must have more")
    (assert (= (list-length b3) 1) "batch 3 must yield remaining 1 entry")
    (assert (= (get step3 :cursor) 5) "cursor must reach total 5")
    (assert (= (get step3 :has-more) false) "step 3 must not have more")
    true))

(df test-mandatory-skip-reason [] -> Bool
  :d "Verifies strict rejection of skipped entries without reason."
  (let [(e1 (wl/make-worklist-entry "target.asl" "refactor"))
        (w0 (wl/worklist-create "wl-skip" (list e1)))
        (empty-res (wl/worklist-mark-skipped w0 "target.asl" ""))
        (ws-res (wl/worklist-mark-skipped w0 "target.asl" "   "))
        (valid-res (wl/worklist-mark-skipped w0 "target.asl" "already refactored in Phase 464"))]
    (assert (is-err? empty-res) "skipping with empty reason must fail")
    (assert (is-err? ws-res) "skipping with whitespace reason must fail")
    (assert (is-ok? valid-res) "skipping with valid reason must succeed")
    (let [(w-valid (result-or valid-res w0))
          (entry-opt (wl/worklist-find-entry w-valid "target.asl"))]
      (assert (is-some? entry-opt) "entry must be found")
      (let [(entry (option-unwrap entry-opt))]
        (assert (= (.-status entry) "skipped") "status must be skipped")
        (assert (= (.-reason entry) "already refactored in Phase 464") "reason must be stored")
        true))))

(df test-marking-done-and-findings [] -> Bool
  :d "Verifies marking entry done and recording finding notes."
  (let [(e1 (wl/make-worklist-entry "eval.asl" "inspect bounds"))
        (w0 (wl/worklist-create "wl-done" (list e1)))
        (w1 (wl/worklist-mark-done w0 "eval.asl" "AST verified zero recursion leaks"))
        (entry-opt (wl/worklist-find-entry w1 "eval.asl"))]
    (assert (is-some? entry-opt) "entry must exist")
    (let [(entry (option-unwrap entry-opt))]
      (assert (= (.-status entry) "done") "status must be done")
      (assert (= (.-notes entry) "AST verified zero recursion leaks") "findings notes must be recorded")
      (assert (wl/worklist-is-complete? w1) "worklist must be complete after all items done")
      true)))

(df test-concurrent-walkers-no-collision [] -> Bool
  :d "Verifies two concurrent walkers receive non-overlapping entry sets."
  (let [(entries (list
                   (wl/make-worklist-entry "f1.asl" "i1")
                   (wl/make-worklist-entry "f2.asl" "i2")
                   (wl/make-worklist-entry "f3.asl" "i3")
                   (wl/make-worklist-entry "f4.asl" "i4")))
        (w0 (wl/worklist-create "wl-conc" entries))
        (claim1 (wl/worklist-claim-batch w0 "walker-1" 2))
        (b1 (get claim1 :claimed))
        (w1 (get claim1 :worklist))
        (claim2 (wl/worklist-claim-batch w1 "walker-2" 2))
        (b2 (get claim2 :claimed))]
    (assert (= (list-length b1) 2) "walker 1 must claim 2")
    (assert (= (list-length b2) 2) "walker 2 must claim 2")
    (let [(p1-0 (.-path (option-unwrap (list-get b1 0))))
          (p1-1 (.-path (option-unwrap (list-get b1 1))))
          (p2-0 (.-path (option-unwrap (list-get b2 0))))
          (p2-1 (.-path (option-unwrap (list-get b2 1))))]
      (assert (= p1-0 "f1.asl") "walker 1 got f1")
      (assert (= p1-1 "f2.asl") "walker 1 got f2")
      (assert (= p2-0 "f3.asl") "walker 2 got f3")
      (assert (= p2-1 "f4.asl") "walker 2 got f4")
      (assert (not (= p1-0 p2-0)) "no collision between walkers")
      (assert (not (= p1-1 p2-1)) "no collision between walkers")
      true)))

(df test-worklist-asn-serialization [] -> Bool
  :d "Verifies ASN serialization of worklist structure."
  (let [(e1 (wl/make-worklist-entry "mod.asl" "parse AST"))
        (w (wl/worklist-create "wl-ser" (list e1)))
        (asn-doc (wl/worklist-format-asn w))]
    (assert (string-contains? asn-doc "(:worklist") "must contain worklist header")
    (assert (string-contains? asn-doc ":id \"wl-ser\"") "must contain id")
    (assert (string-contains? asn-doc ":path \"mod.asl\"") "must contain entry path")
    (assert (string-contains? asn-doc ":status :pending") "must contain pending status")
    true))

(df run-tests [] -> Bool
  :d "Executes all worklist test assertions."
  (do
    (assert (test-worklist-creation) "test-worklist-creation must pass")
    (assert (test-cursor-bounded-batches) "test-cursor-bounded-batches must pass")
    (assert (test-mandatory-skip-reason) "test-mandatory-skip-reason must pass")
    (assert (test-marking-done-and-findings) "test-marking-done-and-findings must pass")
    (assert (test-concurrent-walkers-no-collision) "test-concurrent-walkers-no-collision must pass")
    (assert (test-worklist-asn-serialization) "test-worklist-asn-serialization must pass")
    true))
