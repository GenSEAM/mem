(module asl-mem/speculative-fork
  :d "In-Memory Speculative Mutation Testing and Epistemic Fuzzing Harness"
  :x [
    SpeculativeMutant
    SpeculativeFuzzResult
    create-speculative-mutant
    fork-mutant-cluster
    evaluate-mutant-survival
    summarize-fuzz-run
  ])

(dfs SpeculativeMutant
  (:f id Str "Mutant identifier")
  (:f mutation-type Str "Type of AST mutation: invert-cond, delete-assert, perturb-const")
  (:f target-symbol Str "Target function or form identifier")
  (:f mutated-content Str "Full mutated code content")
  (:f survived Bool "True if test suite passed unexpectedly on mutant (leak)"))

(dfs SpeculativeFuzzResult
  (:f target-symbol Str "Target symbol mutated")
  (:f total-mutants I64 "Number of speculative mutants evaluated")
  (:f killed-mutants I64 "Number of mutants correctly detected and killed by tests")
  (:f surviving-mutants I64 "Number of mutants that survived (coverage gaps)")
  (:f kill-rate-pct I64 "Mutation kill rate percentage"))

(df create-speculative-mutant [(id Str)
                              (m-type Str)
                              (sym Str)
                              (mutated Str)] -> SpeculativeMutant
  :d "Constructs a speculative mutant record."
  (SpeculativeMutant
    :id id
    :mutation-type m-type
    :target-symbol sym
    :mutated-content mutated
    :survived false))

(df fork-mutant-cluster [(base-content Str) (target-sym Str)] -> (List SpeculativeMutant)
  :d "Forks in-memory mutant cluster with 3 discrete structural perturbations."
  (let [(m1-txt (if (string-contains? base-content "(if ")
                  (string-replace base-content "(if " "(if (not ")
                  (str base-content "\n;; mutant-invert-cond")))
        (m2-txt (if (string-contains? base-content "(assert ")
                  (string-replace base-content "(assert " "(assert (not ")
                  (str base-content "\n;; mutant-negate-assert")))
        (m3-txt (if (string-contains? base-content " 0)")
                  (string-replace base-content " 0)" " -1)")
                  (if (string-contains? base-content " 10)")
                    (string-replace base-content " 10)" " 999)")
                    (str base-content "\n;; mutant-perturb-const"))))
        (mutant1 (create-speculative-mutant (str target-sym "-mut-1") "invert-cond" target-sym m1-txt))
        (mutant2 (create-speculative-mutant (str target-sym "-mut-2") "negate-assert" target-sym m2-txt))
        (mutant3 (create-speculative-mutant (str target-sym "-mut-3") "perturb-const" target-sym m3-txt))]
    (list mutant1 mutant2 mutant3)))

(df evaluate-mutant-survival [(mutant SpeculativeMutant) (test-passes Bool)] -> SpeculativeMutant
  :d "Records mutant survival: if test passes on mutated code, mutant survived (test gap)."
  (SpeculativeMutant
    :id (.-id mutant)
    :mutation-type (.-mutation-type mutant)
    :target-symbol (.-target-symbol mutant)
    :mutated-content (.-mutated-content mutant)
    :survived test-passes))

(df summarize-fuzz-run [(target-sym Str) (mutants (List SpeculativeMutant))] -> SpeculativeFuzzResult
  :d "Summarizes mutation kill rate and coverage gaps across mutant cluster."
  (let [(total (list-length mutants))
        (survived-count (fold (fn [(acc I64) (m SpeculativeMutant)] -> I64
                                (if (.-survived m) (+ acc 1) acc))
                              0
                              mutants))
        (killed-count (- total survived-count))
        (kill-rate (if (<= total 0) 0 (/ (* killed-count 100) total)))]
    (SpeculativeFuzzResult
      :target-symbol target-sym
      :total-mutants total
      :killed-mutants killed-count
      :surviving-mutants survived-count
      :kill-rate-pct kill-rate)))
