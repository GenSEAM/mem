(module asl-mem/tests/research-test
  :d "Unit verification suite for research memory direct-hit cache and proxy routing."
  :x [run-tests
      test-research-direct-hit-zero-tokens
      test-research-cache-miss-delegation
      test-research-store-and-recall]
  :i [(research :a res)])

(df test-research-direct-hit-zero-tokens [] -> Bool
  :d "Validates that high-confidence cached facts yield direct hit with zero token overhead."
  (let [(fact1 (res/make-research-fact "fact-1" "agent memory architecture" "Deterministic direct hit returns zero tokens" "https://asl.dev/mem" 0.95))
        (cache (res/store-research-fact fact1 (list)))
        (outcome (res/query-research-memory "agent memory architecture" cache 0.85))]
    (assert (.-is-direct-hit outcome) "High confidence fact must trigger direct hit")
    (assert (= (.-token-cost outcome) 0) "Direct hit must incur zero token cost")
    (assert (not (.-delegated outcome)) "Direct hit must not be delegated")
    (assert (= (.-fact outcome) "Deterministic direct hit returns zero tokens") "Retrieved fact must match canonical fact text")
    true))

(df test-research-cache-miss-delegation [] -> Bool
  :d "Validates that low-confidence queries and cache misses transparently delegate to research agent."
  (let [(outcome-empty (res/query-research-memory "unseen esoteric theorem" (list) 0.85))
        (low-fact (res/make-research-fact "fact-2" "speculative query" "unverified hypothesis" "forum" 0.60))
        (cache2 (res/store-research-fact low-fact (list)))
        (outcome-low (res/query-research-memory "speculative query" cache2 0.85))]
    (assert (not (.-is-direct-hit outcome-empty)) "Cache miss must not be direct hit")
    (assert (= (.-token-cost outcome-empty) 150) "Cache miss token cost must be 150")
    (assert (.-delegated outcome-empty) "Cache miss must delegate to research agent")
    (assert (not (.-is-direct-hit outcome-low)) "Low confidence fact must not trigger direct hit")
    (assert (= (.-token-cost outcome-low) 150) "Low confidence fact query token cost must be 150")
    (assert (.-delegated outcome-low) "Low confidence fact query must delegate to research agent")
    true))

(df test-research-store-and-recall [] -> Bool
  :d "Validates incremental fact cache storage and subsequent direct hit recall."
  (let [(cache0 (list))
        (miss (res/query-research-memory "compiler pipeline phases" cache0 0.85))]
    (assert (.-delegated miss) "Initial query before storage must delegate")
    (let [(new-fact (res/make-research-fact "fact-3" "compiler pipeline phases" "Lex, Parse, Check, Codegen" "manual" 0.92))
          (cache1 (res/store-research-fact new-fact cache0))
          (hit (res/query-research-memory "compiler pipeline phases" cache1 0.85))]
      (assert (.-is-direct-hit hit) "Subsequent query after storage must be direct hit")
      (assert (= (.-token-cost hit) 0) "Subsequent direct hit must incur 0 token cost")
      (assert (not (.-delegated hit)) "Subsequent direct hit must not delegate")
      (assert (= (.-fact hit) "Lex, Parse, Check, Codegen") "Recalled fact must match stored payload")
      (let [(updated-fact (res/make-research-fact "fact-3" "compiler pipeline phases" "Lex, Parse, Check, Codegen, Verify" "manual-v2" 0.99))
            (cache2 (res/store-research-fact updated-fact cache1))]
        (assert (= (list-length cache2) 1) "Updating fact must maintain unique cache entry")
        (let [(hit2 (res/query-research-memory "compiler pipeline phases" cache2 0.85))]
          (assert (= (.-fact hit2) "Lex, Parse, Check, Codegen, Verify") "Recalled fact must reflect updated payload")
          true)))))

(df run-tests [] -> Bool
  :d "Executes all research memory unit tests."
  (and (test-research-direct-hit-zero-tokens)
       (and (test-research-cache-miss-delegation)
            (test-research-store-and-recall))))
