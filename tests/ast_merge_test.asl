(module asl-mem/tests/ast-merge-test
  :d "Falsifiable verification test suite for Universal Homoiconic AST 3-Way Merge Engine"
  :x [run-tests
      TestDisjointFormMerge
      TestIdenticalMutationDeduplication
      TestDivergentModificationConflict
      TestDeleteVsModifyConflict
      TestSexpListSetUnion
      TestTaskFormMerge]
  :i [(ast_merge :a am)])

(df TestDisjointFormMerge [] -> Bool
  :d "Verifies clean resolution of non-overlapping AST forms."
  (let [(base-code (str "(module app :x [f1 f2])\n\n"
                        "(df f1 [] -> I64 10)\n\n"
                        "(df f2 [] -> I64 20)"))
        (branch-a (str "(module app :x [f1 f2])\n\n"
                       "(df f1 [] -> I64 999)\n\n"
                       "(df f2 [] -> I64 20)"))
        (branch-b (str "(module app :x [f1 f2])\n\n"
                       "(df f1 [] -> I64 10)\n\n"
                       "(df f2 [] -> I64 888)"))
        (res (am/universal-ast-3way-merge base-code branch-a branch-b))
        (merged (.-merged-content res))]
    (assert (.-clean res) "Disjoint form mutations must merge cleanly without conflict")
    (assert (= (list-length (.-conflicts res)) 0) "Clean merge must report 0 conflicts")
    (assert (= (list-length (.-collisions res)) 0) "Clean merge must have empty collisions list")
    (assert (= (.-merged-forms-count res) 3) "Must preserve all 3 AST forms")
    (assert (string-contains? merged "999") "Merged content must include branch A mutation")
    (assert (string-contains? merged "888") "Merged content must include branch B mutation")
    (assert (string-contains? merged "module app") "Merged content must preserve module declaration")
    true))

(df TestIdenticalMutationDeduplication [] -> Bool
  :d "Verifies identical concurrent mutations across branches are deduplicated to single form."
  (let [(base-code (str "(df f1 [] -> I64 10)\n\n(df f2 [] -> I64 20)"))
        (branch-a (str "(df f1 [] -> I64 42)\n\n(df f2 [] -> I64 20)"))
        (branch-b (str "(df f1 [] -> I64 42)\n\n(df f2 [] -> I64 20)"))
        (res (am/universal-ast-3way-merge base-code branch-a branch-b))
        (merged (.-merged-content res))]
    (assert (.-clean res) "Identical mutations must merge cleanly")
    (assert (= (list-length (.-conflicts res)) 0) "Identical mutations must have 0 conflicts")
    (assert (= (.-merged-forms-count res) 2) "Deduplicated merge must preserve exactly 2 forms")
    (assert (string-contains? merged "42") "Merged content must contain updated form")
    true))

(df TestDivergentModificationConflict [] -> Bool
  :d "Verifies divergent concurrent modifications to same form emit structured ASTCollisionRecord."
  (let [(base-code (str "(df f1 [] -> I64 10)\n\n(df f2 [] -> I64 20)"))
        (branch-a (str "(df f1 [] -> I64 111)\n\n(df f2 [] -> I64 20)"))
        (branch-b (str "(df f1 [] -> I64 222)\n\n(df f2 [] -> I64 20)"))
        (res (am/universal-ast-3way-merge base-code branch-a branch-b))
        (cols (.-collisions res))]
    (assert (not (.-clean res)) "Divergent mutations to same form must trigger collision")
    (assert (= (list-length (.-conflicts res)) 1) "Must report exactly 1 conflict")
    (assert (= (list-length cols) 1) "Must contain exactly 1 ASTCollisionRecord")
    (let [(c (option-or (list-get cols 0) (am/ASTCollisionRecord :symbol-id "" :base-form "" :branch-a "" :branch-b "" :collision-type "")))]
      (assert (= (.-symbol-id c) "f1") "Conflicted symbol ID must be f1")
      (assert (= (.-collision-type c) "divergent-modification") "Collision type must be divergent-modification")
      (assert (string-contains? (.-branch-a c) "111") "Collision record branch A must contain 111")
      (assert (string-contains? (.-branch-b c) "222") "Collision record branch B must contain 222"))
    true))

(df TestDeleteVsModifyConflict [] -> Bool
  :d "Verifies deletion in one branch vs modification in another produces structured collision."
  (let [(base-code (str "(df f1 [] -> I64 10)\n\n(df f2 [] -> I64 20)"))
        (branch-a (str "(df f2 [] -> I64 20)"))
        (branch-b (str "(df f1 [] -> I64 999)\n\n(df f2 [] -> I64 20)"))
        (res (am/universal-ast-3way-merge base-code branch-a branch-b))
        (cols (.-collisions res))]
    (assert (not (.-clean res)) "Delete vs modify must trigger collision")
    (assert (= (list-length (.-conflicts res)) 1) "Must report exactly 1 conflict")
    (let [(c (option-or (list-get cols 0) (am/ASTCollisionRecord :symbol-id "" :base-form "" :branch-a "" :branch-b "" :collision-type "")))]
      (assert (= (.-symbol-id c) "f1") "Conflicted symbol must be f1")
      (assert (= (.-collision-type c) "delete-vs-modify") "Collision type must be delete-vs-modify"))
    true))

(df TestSexpListSetUnion [] -> Bool
  :d "Verifies merge-sexp-lists unions added items without duplicates."
  (let [(base (list "core" "mem"))
        (list-a (list "core" "mem" "bus" "vdom"))
        (list-b (list "core" "mem" "bus" "voice"))
        (merged (am/merge-sexp-lists base list-a list-b))]
    (assert (= (list-length merged) 5) "Merged list must contain exactly 5 unique entries")
    (assert (list-contains? merged "core") "Merged list must contain core")
    (assert (list-contains? merged "mem") "Merged list must contain mem")
    (assert (list-contains? merged "bus") "Merged list must contain bus")
    (assert (list-contains? merged "vdom") "Merged list must contain vdom")
    (assert (list-contains? merged "voice") "Merged list must contain voice")
    true))

(df TestTaskFormMerge [] -> Bool
  :d "Verifies ASN task S-expression forms merge cleanly across branches."
  (let [(base-tasks (str "(:task :id \"T1\" :state :completed)\n\n"
                         "(:task :id \"T2\" :state :ready)"))
        (branch-a (str "(:task :id \"T1\" :state :completed)\n\n"
                       "(:task :id \"T2\" :state :in_progress)\n\n"
                       "(:task :id \"T3\" :state :ready)"))
        (branch-b (str "(:task :id \"T1\" :state :completed)\n\n"
                       "(:task :id \"T2\" :state :ready)\n\n"
                       "(:task :id \"T4\" :state :ready)"))
        (res (am/universal-ast-3way-merge base-tasks branch-a branch-b))
        (merged (.-merged-content res))]
    (assert (.-clean res) "Disjoint task additions and status advancement must merge cleanly")
    (assert (= (.-merged-forms-count res) 4) "Merged task list must contain all 4 tasks")
    (assert (string-contains? merged "T3") "Merged tasks must include T3 from branch A")
    (assert (string-contains? merged "T4") "Merged tasks must include T4 from branch B")
    (assert (string-contains? merged ":in_progress") "Merged tasks must preserve in_progress status for T2")
    true))

(df run-tests [] -> Bool
  :d "Executes all Universal Homoiconic AST 3-Way Merge test suites."
  (and (TestDisjointFormMerge)
       (and (TestIdenticalMutationDeduplication)
            (and (TestDivergentModificationConflict)
                 (and (TestDeleteVsModifyConflict)
                      (and (TestSexpListSetUnion)
                           (TestTaskFormMerge)))))))
