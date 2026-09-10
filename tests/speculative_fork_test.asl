(module asl-mem/tests/speculative-fork-test
  :d "Falsifiable verification test suite for In-Memory Speculative Mutation Testing"
  :x [run-tests
      TestMutantClusterCreation
      TestMutantSurvivalEvaluation
      TestFuzzSummaryComputation]
  :i [(speculative_fork :a sf)])

(df TestMutantClusterCreation [] -> Bool
  :d "Verifies in-memory cluster forks 3 discrete mutation operators."
  (let [(code "(df calculate [(x I64)] -> I64 (if (> x 0) (assert (= x x)) 10))")
        (cluster (sf/fork-mutant-cluster code "calculate"))]
    (assert (= (list-length cluster) 3) "Cluster must contain exactly 3 mutants")
    (let [(m1 (option-or (list-get cluster 0) (sf/SpeculativeMutant :id "" :mutation-type "" :target-symbol "" :mutated-content "" :survived false)))
          (m2 (option-or (list-get cluster 1) (sf/SpeculativeMutant :id "" :mutation-type "" :target-symbol "" :mutated-content "" :survived false)))
          (m3 (option-or (list-get cluster 2) (sf/SpeculativeMutant :id "" :mutation-type "" :target-symbol "" :mutated-content "" :survived false)))]
      (assert (= (.-mutation-type m1) "invert-cond") "Mutant 1 must be invert-cond")
      (assert (= (.-mutation-type m2) "negate-assert") "Mutant 2 must be negate-assert")
      (assert (= (.-mutation-type m3) "perturb-const") "Mutant 3 must be perturb-const")
      (assert (string-contains? (.-mutated-content m1) "(if (not ") "Mutant 1 must invert condition")
      (assert (string-contains? (.-mutated-content m2) "(assert (not ") "Mutant 2 must negate assert")
      (assert (string-contains? (.-mutated-content m3) " -1)") "Mutant 3 must perturb zero constant"))
    true))

(df TestMutantSurvivalEvaluation [] -> Bool
  :d "Verifies mutant evaluation flags surviving mutants as test coverage gaps."
  (let [(m (sf/create-speculative-mutant "mut-1" "invert-cond" "fn1" "code"))
        (killed (sf/evaluate-mutant-survival m false))
        (survived (sf/evaluate-mutant-survival m true))]
    (assert (not (.-survived killed)) "Failing test suite must kill mutant")
    (assert (.-survived survived) "Passing test suite on mutant indicates mutant survived")
    true))

(df TestFuzzSummaryComputation [] -> Bool
  :d "Verifies fuzzing run summary computes kill rate and gap counts accurately."
  (let [(m1 (sf/create-speculative-mutant "m1" "t1" "f1" "c1"))
        (m2 (sf/create-speculative-mutant "m2" "t2" "f1" "c2"))
        (eval1 (sf/evaluate-mutant-survival m1 false))
        (eval2 (sf/evaluate-mutant-survival m2 true))
        (summary (sf/summarize-fuzz-run "f1" (list eval1 eval2)))]
    (assert (= (.-total-mutants summary) 2) "Total mutants must be 2")
    (assert (= (.-killed-mutants summary) 1) "Killed mutants must be 1")
    (assert (= (.-surviving-mutants summary) 1) "Surviving mutants must be 1")
    (assert (= (.-kill-rate-pct summary) 50) "Kill rate must be 50%")
    true))

(df run-tests [] -> Bool
  :d "Executes all Speculative Mutation Fuzzing test suites."
  (and (TestMutantClusterCreation)
       (and (TestMutantSurvivalEvaluation)
            (TestFuzzSummaryComputation))))
