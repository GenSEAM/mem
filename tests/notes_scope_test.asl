(module mem-notes-scope-test
  :d "Unit tests for note scope lifecycles, retirement, promotion, and bounded active store under ADR-0081."
  :x [run-tests
      test-scope-close-retires-step-notes
      test-promoted-note-survives-scope-close
      test-epic-notes-outlive-task-scope
      test-active-store-strictly-bounded
      test-amnesia-evict-below-threshold]
  :i [(asl-mem/notes :a n)
      (asl-mem/amnesia :a amn)
      (asl-mem/view-layer :a vl)])

(df test-scope-close-retires-step-notes [] -> Bool
  :d "Verifies that closing a step scope retires step notes while preserving task and epic notes."
  (let [(n-step (n/make-note "s1" "Step observation" "step-topic" (list "t1") "step"))
        (n-task (n/make-note "t1" "Task observation" "task-topic" (list "t2") "task"))
        (n-epic (n/make-note "e1" "Epic observation" "epic-topic" (list "t3") "epic"))
        (initial (list n-step n-task n-epic))
        (after-step (n/note-close-scope initial "step"))]
    (assert (= (list-length initial) 3) "initial list must contain 3 notes")
    (assert (= (list-length after-step) 2) "after step close, must contain 2 notes")
    (assert (not (list-contains? (map (fn [(x n/NoteRecord)] -> Str (.-id x)) after-step) "s1"))
            "step note s1 must be retired")
    (assert (list-contains? (map (fn [(x n/NoteRecord)] -> Str (.-id x)) after-step) "t1")
            "task note t1 must be retained")
    (assert (list-contains? (map (fn [(x n/NoteRecord)] -> Str (.-id x)) after-step) "e1")
            "epic note e1 must be retained")
    true))

(df test-promoted-note-survives-scope-close [] -> Bool
  :d "Verifies that a note promoted to task or adr survives closure of its originating scope."
  (let [(n-step1 (n/make-note "s1" "Promoted finding" "arch" (list "t1") "step"))
        (n-step2 (n/make-note "s2" "Discardable note" "temp" (list "t2") "step"))
        (promoted (n/note-promote-to-task n-step1 "Task46503"))
        (notes-set (list promoted n-step2))
        (after-close (n/note-close-scope notes-set "step"))]
    (assert (= (.-status promoted) "promoted-to-task") "promoted note must have status promoted-to-task")
    (assert (= (list-length after-close) 1) "only promoted note should remain after step close")
    (assert (= (.-id (option-unwrap (list-get after-close 0))) "s1") "promoted note s1 must survive")
    (assert (string-contains? (.-consequence (option-unwrap (list-get after-close 0))) "Task46503")
            "promoted note must cite target task")
    true))

(df test-epic-notes-outlive-task-scope [] -> Bool
  :d "Verifies that closing a task scope retires task notes while preserving epic notes."
  (let [(n-task (n/make-note "t1" "Task finding" "topic-t" (list "t1") "task"))
        (n-epic (n/make-note "e1" "Architectural axiom" "topic-e" (list "t2") "epic"))
        (notes-set (list n-task n-epic))
        (after-task-close (n/note-close-scope notes-set "task"))]
    (assert (= (list-length after-task-close) 1) "only epic note survives task scope close")
    (assert (= (.-id (option-unwrap (list-get after-task-close 0))) "e1") "epic note must survive")
    (assert (= (.-scope (option-unwrap (list-get after-task-close 0))) "epic") "scope must be epic")
    true))

(df test-active-store-strictly-bounded [] -> Bool
  :d "Verifies that active note store remains strictly bounded under continuous generation."
  (let [(notes (list
                 (n/make-note "n1" "fact 1" "top" (list) "step")
                 (n/make-note "n2" "fact 2" "top" (list) "step")
                 (n/make-note "n3" "fact 3" "top" (list) "step")
                 (n/make-note "n4" "fact 4" "top" (list) "step")
                 (n/make-note "n5" "fact 5" "top" (list) "step")
                 (n/make-note "n6" "fact 6" "top" (list) "step")
                 (n/make-note "n7" "fact 7" "top" (list) "step")
                 (n/make-note "n8" "fact 8" "top" (list) "step")))
        (bounded (n/note-prune-bounded notes 4))]
    (assert (= (list-length notes) 8) "raw notes count is 8")
    (assert (= (list-length bounded) 4) "pruned store must be strictly bounded at 4")
    (assert (<= (list-length bounded) 4) "must not exceed max capacity")
    (assert (= (.-id (option-unwrap (list-get bounded 0))) "n5") "oldest notes evicted first")
    (assert (= (.-id (option-unwrap (list-get bounded 3))) "n8") "newest notes retained")
    true))

(df test-amnesia-evict-below-threshold [] -> Bool
  :d "Verifies that amnesia evict-below-threshold removes chunks below confidence bar."
  (let [(c-high (vl/MemoryChunk
                  :id "chunk-high"
                  :ts 1789280000000
                  :conf 0.85
                  :summary "High confidence AST fact"
                  :facts (list "AST verified")
                  :directives (list)
                  :refs (list)))
        (c-low (vl/MemoryChunk
                 :id "chunk-low"
                 :ts 1789280000000
                 :conf 0.25
                 :summary "Low confidence speculative fact"
                 :facts (list "Speculation")
                 :directives (list)
                 :refs (list)))
        (chunks (list c-high c-low))
        (retained (amn/evict-below-threshold chunks 0.50))]
    (assert (= (list-length chunks) 2) "input contains 2 chunks")
    (assert (= (list-length retained) 1) "retained contains 1 chunk")
    (assert (= (.-id (option-unwrap (list-get retained 0))) "chunk-high") "only high confidence chunk retained")
    true))

(df run-tests [] -> Bool
  :d "Executes all notes scope lifecycle test assertions."
  (do
    (assert (test-scope-close-retires-step-notes) "test-scope-close-retires-step-notes must pass")
    (assert (test-promoted-note-survives-scope-close) "test-promoted-note-survives-scope-close must pass")
    (assert (test-epic-notes-outlive-task-scope) "test-epic-notes-outlive-task-scope must pass")
    (assert (test-active-store-strictly-bounded) "test-active-store-strictly-bounded must pass")
    (assert (test-amnesia-evict-below-threshold) "test-amnesia-evict-below-threshold must pass")
    true))
