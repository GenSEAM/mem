(module asl-mem/tests/pointer-orchestrator-test
  :d "Comprehensive unit test suite for dynamic context prioritization and pointer orchestration"
  :x [test-context-chunk-scoring-and-density
      test-context-chunk-ranking-and-budget-boundary
      test-perceptual-pointer-creation-and-formats
      test-dynamic-pointer-expansion-and-dereference
      test-pointer-orchestrator-budget-enforcement
      test-cross-tier-budget-boundaries
      run-tests]
  :i [(pointer_orchestrator :a po)
      (context_priority :a cp)])

(df make-string-1000 [] -> Str
  :d "Generates a deterministic 1000-character string payload"
  (let [(s10 "0123456789")
        (s100 (str s10 s10 s10 s10 s10 s10 s10 s10 s10 s10))]
    (str s100 s100 s100 s100 s100 s100 s100 s100 s100 s100)))

(df make-string-2400 [] -> Str
  :d "Generates a deterministic 2400-character string payload"
  (let [(s1000 (make-string-1000))
        (s10 "0123456789")
        (s100 (str s10 s10 s10 s10 s10 s10 s10 s10 s10 s10))
        (s400 (str s100 s100 s100 s100))]
    (str s1000 s1000 s400)))

(df test-context-chunk-scoring-and-density [] -> Bool
  :d "Verifies dynamic scoring formula, relevance weighting, and field accessors"
  (let [(c1 (cp/score-context-chunk "chunk-ast-1" "ast-outline" 100 0.9))
        (c2 (cp/score-context-chunk "chunk-diff-2" "recent-diff" 100 0.4))
        (c3 (cp/score-context-chunk "chunk-bulky-3" "ast-outline" 500 0.9))]
    (assert (= (.-chunk-id c1) "chunk-ast-1") "Chunk id matches")
    (assert (= (.-kind c1) "ast-outline") "Chunk kind matches")
    (assert (> (.-priority-score c1) (.-priority-score c2)) "Higher relevance yields higher priority")
    (assert (> (.-priority-score c1) (.-priority-score c3)) "Bulky footprint penalized for equal relevance")
    (assert (not (.-retained c1)) "Initial retained status is false")
    true))

(df test-context-chunk-ranking-and-budget-boundary [] -> Bool
  :d "Verifies descending priority ranking and token budget boundary application"
  (let [(c1 (cp/score-context-chunk "c1" "ast-outline" 300 0.9))
        (c2 (cp/score-context-chunk "c2" "task-memory" 200 0.8))
        (c3 (cp/score-context-chunk "c3" "recent-diff" 400 0.7))
        (c4 (cp/score-context-chunk "c4" "ast-outline" 200 0.2))
        (c5 (cp/score-context-chunk "c5" "ast-outline" 600 0.1))
        (ranked (cp/rank-context-chunks (list c4 c2 c5 c1 c3)))
        (first-c (option-or (list-head ranked) c4))
        (budgeted (cp/apply-token-budget ranked 1200))
        (retained (cp/filter-retained-chunks budgeted))
        (total-tok (cp/total-retained-tokens budgeted))
        (first-budgeted (option-or (list-head budgeted) c4))
        (last-budgeted (option-or (list-head (list-reverse budgeted)) c4))]
    (assert (= (.-chunk-id first-c) (.-chunk-id c2)) "Top ranked chunk is highest priority")
    (assert (>= (list-length retained) 3) "At least 3 chunks retained within budget")
    (assert (<= total-tok 1200) "Total retained tokens within budget ceiling")
    (assert (> total-tok 0) "Total retained tokens positive")
    (assert (.-retained first-budgeted) "First chunk retained")
    (assert (not (.-retained last-budgeted)) "Trailing chunk evicted")
    true))

(df test-perceptual-pointer-creation-and-formats [] -> Bool
  :d "Verifies perceptual pointer construction, S-expression formatting, and round-trip parsing"
  (let [(p1 (po/make-pointer-spec "ptr-sym-vfs" "sym" "vfs-read" 12 "VFS read symbol"))
        (p2 (po/make-pointer-spec "ptr-file-vfs" "file" "mem/src/vfs.asl" 10 "VFS module source"))
        (p3 (po/make-pointer-spec "ptr-chunk-1" "chunk" "chunk-001" 14 "AST context chunk"))
        (fmt (po/format-pointer-spec p1))
        (parsed-opt (po/parse-pointer-spec fmt))
        (parsed-ok (mt parsed-opt
                     ((none) false)
                     ((some parsed)
                      (and (= (.-id parsed) "ptr-sym-vfs")
                           (= (.-tokens parsed) 12)))))]
    (assert (= (.-kind p1) "sym") "Pointer kind is sym")
    (assert (= (.-tokens p2) 10) "Pointer tokens match")
    (assert (= (.-target p3) "chunk-001") "Target chunk id matches")
    (assert (string-starts-with? fmt "(:ptr :kind \"sym\"") "Serialized prefix matches S-expression specification")
    (assert parsed-ok "Parsed pointer matches serialized attributes")
    true))

(df test-dynamic-pointer-expansion-and-dereference [] -> Bool
  :d "Verifies progressive disclosure on-demand expansion and collapse mechanics"
  (let [(p (po/make-pointer-spec "ptr-sym-vfs" "sym" "vfs-read" 12 "VFS read symbol"))
        (raw-body (make-string-1000))
        (expanded (po/deref-pointer p raw-body))
        (collapsed (po/collapse-pointer expanded))]
    (assert (not (.-is-expanded p)) "Pointer unexpanded initially")
    (assert (= (po/total-active-tokens (list p)) 12) "Compact token footprint is 12")
    (assert (.-is-expanded expanded) "Pointer marked expanded after dereference")
    (assert (= (po/total-active-tokens (list expanded)) 250) "Active tokens equals 250 after dereference")
    (assert (not (.-is-expanded collapsed)) "Pointer marked unexpanded after collapse")
    (assert (= (po/total-active-tokens (list collapsed)) 12) "Active tokens returns to 12 after collapse")
    true))

(df test-pointer-orchestrator-budget-enforcement [] -> Bool
  :d "Verifies dynamic pointer expansion budget constraints and minimal context boundary"
  (let [(payload2400 (make-string-2400))
        (p1 (po/deref-pointer (po/make-pointer-spec "p1" "sym" "s1" 15 "s1") payload2400))
        (p2 (po/deref-pointer (po/make-pointer-spec "p2" "sym" "s2" 15 "s2") payload2400))
        (p3 (po/deref-pointer (po/make-pointer-spec "p3" "sym" "s3" 15 "s3") payload2400))
        (p4 (po/deref-pointer (po/make-pointer-spec "p4" "sym" "s4" 15 "s4") payload2400))
        (p5 (po/deref-pointer (po/make-pointer-spec "p5" "sym" "s5" 15 "s5") payload2400))
        (p6 (po/deref-pointer (po/make-pointer-spec "p6" "sym" "s6" 15 "s6") payload2400))
        (orchestrated (po/orchestrate-context-pointers (list p1 p2 p3 p4 p5 p6) 1500))
        (first-p (option-or (list-head orchestrated) p1))
        (tail-1 (option-or (list-tail orchestrated) (list)))
        (second-p (option-or (list-head tail-1) p1))
        (last-p (option-or (list-head (list-reverse orchestrated)) p1))]
    (assert (.-is-expanded first-p) "First priority pointer expanded")
    (assert (.-is-expanded second-p) "Second priority pointer expanded")
    (assert (not (.-is-expanded last-p)) "Trailing pointer collapsed into compact stub")
    (assert (< (po/total-active-tokens orchestrated) 1500) "Total active tokens strictly below budget ceiling")
    (assert (po/is-context-minimal? orchestrated 1500) "Context remains strictly minimal under 1500 token ceiling")
    true))

(df test-cross-tier-budget-boundaries [] -> Bool
  :d "Verifies multi-tier context ceilings exact-fit and overflow boundary conditions"
  (let [(slm-ret (cp/total-retained-tokens (cp/apply-token-budget (list (cp/score-context-chunk "c1" "k" 2000 0.9) (cp/score-context-chunk "c2" "k" 1000 0.8)) 2500)))
        (mid-ret (cp/total-retained-tokens (cp/apply-token-budget (list (cp/score-context-chunk "c1" "k" 5000 0.9) (cp/score-context-chunk "c2" "k" 4000 0.8)) 8000)))
        (minimal-check (po/is-context-minimal? (list (po/make-pointer-spec "p1" "sym" "t1" 10 "s") (po/make-pointer-spec "p2" "sym" "t2" 15 "s")) 1500))
        (res-exact (cp/apply-token-budget (list (cp/score-context-chunk "c-exact" "k" 500 0.9)) 500))
        (exact-fit (let [(h (option-or (list-head res-exact) (cp/score-context-chunk "d" "k" 0 0.0)))] (.-retained h)))
        (res-over (cp/apply-token-budget (list (cp/score-context-chunk "c-over" "k" 501 0.9)) 500))
        (overflow-evicted (let [(h (option-or (list-head res-over) (cp/score-context-chunk "d" "k" 0 0.0)))] (not (.-retained h))))]
    (assert (<= slm-ret 2500) "SLM token budget ceiling enforced")
    (assert (<= mid-ret 8000) "Mid-tier token budget ceiling enforced")
    (assert minimal-check "Minimal base context invariant verified")
    (assert exact-fit "Exact-fit chunk retained")
    (assert overflow-evicted "Overflow chunk evicted")
    true))

(df run-tests [] -> Bool
  :d "Executes complete test suite for dynamic context priority and pointer orchestration"
  (and (test-context-chunk-scoring-and-density)
       (and (test-context-chunk-ranking-and-budget-boundary)
            (and (test-perceptual-pointer-creation-and-formats)
                 (and (test-dynamic-pointer-expansion-and-dereference)
                      (and (test-pointer-orchestrator-budget-enforcement)
                           (test-cross-tier-budget-boundaries)))))))
