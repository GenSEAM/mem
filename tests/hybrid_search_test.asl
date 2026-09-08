(module asl-mem/tests/hybrid-search-test
  :doc "Comprehensive unit test suite for 64-bit SimHash, Hamming distance, RRF fusion, prompt caching, and CAS subtree memoization."
  :x [run-tests
      test-simhash-fnv-and-popcount
      test-simhash-identical-text
      test-simhash-small-perturbation
      test-simhash-unrelated-text
      test-rrf-scoring-and-fusion
      test-prompt-cache-prefix-stability
      test-subtree-memoization-and-incremental-diff]
  :i [(simhash :a sh)
      (rrf :a rrf)
      (subtree_cache :a sc)
      (prompt_cache :a pc)])

(df test-simhash-fnv-and-popcount [] -> Bool
  :doc "Verifies FNV-1a 64-bit hashing and popcount bitwise invariants."
  (let [(h-empty (sh/fnv1a-64 ""))
        (h-hello (sh/fnv1a-64 "hello"))
        (h-hello2 (sh/fnv1a-64 "hello"))
        (h-world (sh/fnv1a-64 "world"))
        (pop-0 (sh/popcount-64 0))
        (pop-1 (sh/popcount-64 1))
        (pop-7 (sh/popcount-64 7))
        (pop-15 (sh/popcount-64 15))]
    (assert (!= h-empty 0) "FNV-1a empty string hash must not be zero")
    (assert (= h-hello h-hello2) "FNV-1a must be deterministic for identical strings")
    (assert (!= h-hello h-world) "FNV-1a must produce different hashes for distinct strings")
    (assert (= pop-0 0) "Popcount of 0 must be 0")
    (assert (= pop-1 1) "Popcount of 1 must be 1")
    (assert (= pop-7 3) "Popcount of 7 (111b) must be 3")
    (assert (= pop-15 4) "Popcount of 15 (1111b) must be 4")
    true))

(df test-simhash-identical-text [] -> Bool
  :doc "Verifies identical texts and punctuation variants yield Hamming distance 0 and similarity 1.0."
  (let [(t1 "pure agentscript high speed in memory hybrid search engine")
        (t2 "pure agentscript high speed in memory hybrid search engine")
        (t3 "pure agentscript, high speed in-memory hybrid search engine!")
        (h1 (sh/simhash-64 t1))
        (h2 (sh/simhash-64 t2))
        (h3 (sh/simhash-64 t3))
        (dist-12 (sh/hamming-distance h1 h2))
        (dist-13 (sh/hamming-distance h1 h3))
        (sim-12 (sh/simhash-similarity h1 h2))
        (sim-13 (sh/simhash-similarity h1 h3))]
    (assert (!= h1 0) "SimHash of non-empty text must be non-zero")
    (assert (= h1 h2) "SimHash of identical text must be bitwise identical")
    (assert (= dist-12 0) "Hamming distance of identical text must be 0")
    (assert (= dist-13 0) "Hamming distance of punctuation variant text must be 0")
    (assert (>= sim-12 0.999) "Similarity of identical text must be 1.0")
    (assert (>= sim-13 0.999) "Similarity of punctuation variant text must be 1.0")
    true))

(df test-simhash-small-perturbation [] -> Bool
  :doc "Verifies small single-word perturbations yield small Hamming distances <= 6."
  (let [(t-base "deterministic compiler pipeline emitting compact bytecode for webassembly target")
        (t-pert "deterministic compiler pipeline emitting compact binary for webassembly target")
        (h-base (sh/simhash-64 t-base))
        (h-pert (sh/simhash-64 t-pert))
        (dist (sh/hamming-distance h-base h-pert))
        (sim (sh/simhash-similarity h-base h-pert))]
    (assert (> dist 0) "Perturbed text must produce non-identical hash")
    (assert (<= dist 8) "Single word perturbation Hamming distance must be bounded by 8")
    (assert (> sim 0.85) "Perturbed text similarity must remain above 0.85")
    (assert (<= sim 1.0) "Similarity must be bounded above by 1.0")
    true))

(df test-simhash-unrelated-text [] -> Bool
  :doc "Verifies completely unrelated texts yield near-orthogonal Hamming distance around 32."
  (let [(t-quantum "quantum entanglement qubit superposition decoherence state vector hermitian")
        (t-cooking "culinary recipe italian pasta tomato garlic olive basil parmesan simmering")
        (h-q (sh/simhash-64 t-quantum))
        (h-c (sh/simhash-64 t-cooking))
        (dist (sh/hamming-distance h-q h-c))
        (sim (sh/simhash-similarity h-q h-c))]
    (assert (>= dist 18) "Unrelated texts Hamming distance must be >= 18")
    (assert (<= dist 46) "Unrelated texts Hamming distance must be <= 46")
    (assert (< sim 0.75) "Unrelated texts similarity must be below 0.75")
    (assert (> sim 0.25) "Unrelated texts similarity must be above 0.25")
    true))

(df test-rrf-scoring-and-fusion [] -> Bool
  :doc "Verifies Reciprocal Rank Fusion component scores and multi-channel consensus ranking."
  (let [(s0 (rrf/compute-rrf-score 0 60))
        (s1 (rrf/compute-rrf-score 1 60))
        (s2 (rrf/compute-rrf-score 2 60))
        (c1-a (rrf/RankedCandidate :id "doc-consensus" :score 0.0 :rank 1))
        (c2-a (rrf/RankedCandidate :id "doc-lexical-only" :score 0.0 :rank 2))
        (c1-b (rrf/RankedCandidate :id "doc-consensus" :score 0.0 :rank 1))
        (c3-b (rrf/RankedCandidate :id "doc-semantic-only" :score 0.0 :rank 2))
        (channel-a (list c1-a c2-a))
        (channel-b (list c1-b c3-b))
        (fused (rrf/hybrid-code-search channel-a channel-b))
        (top-opt (list-head fused))
        (top-cand (option-or top-opt (rrf/RankedCandidate :id "" :score 0.0 :rank 0)))]
    (assert (= s0 0.0) "Invalid rank score must be 0.0")
    (assert (> s1 0.015) "Rank 1 RRF score must exceed 0.015")
    (assert (> s1 s2) "Rank 1 RRF score must exceed Rank 2 RRF score")
    (assert (= (list-length fused) 3) "Fused results must contain all 3 distinct candidate IDs")
    (assert (= (.-id top-cand) "doc-consensus") "Multi-channel consensus item must rank top")
    (assert (> (.-score top-cand) 0.03) "Consensus candidate score must exceed single channel score")
    (assert (= (.-rank top-cand) 1) "Top candidate must be assigned rank 1")
    true))

(df test-prompt-cache-prefix-stability [] -> Bool
  :doc "Verifies prompt prefix freezing and bit-for-bit prefix stability across dynamic turns."
  (let [(axioms (list "d-0015: Token Arbitrage" "d-0016: Agent-Native Autonomy" "d-0017: Falsifiable Observability"))
        (tools (list "run_command" "view_file" "grep_search" "replace_file_content"))
        (prefix (pc/freeze-system-prefix axioms tools))
        (turn1 (pc/assemble-cached-prompt prefix (list "User: Report system status." "Agent: Status is nominal.")))
        (turn2 (pc/assemble-cached-prompt prefix (list "User: Execute task 336." "Agent: Running tests...")))
        (is-stable (pc/verify-prefix-stability turn1 turn2))
        (corrupted-prompt (str "(:system-prefix\n  :axioms [\"modified\"])\n(:dynamic-turn-marker)\nUser: test"))
        (is-unstable (pc/verify-prefix-stability turn1 corrupted-prompt))]
    (assert (> (string-length (.-axioms-hash prefix)) 0) "Axioms content hash must be non-empty")
    (assert (> (string-length (.-tools-hash prefix)) 0) "Tools content hash must be non-empty")
    (assert (string-contains? (.-canonical-prefix prefix) "(:system-prefix") "Prefix must contain :system-prefix")
    (assert (string-contains? (.-canonical-prefix prefix) "(:dynamic-turn-marker)") "Prefix must contain marker")
    (assert is-stable "Prefix must be bit-for-bit stable across divergent dynamic turns")
    (assert (not is-unstable) "Corrupted prefix must fail stability verification")
    true))

(df test-subtree-memoization-and-incremental-diff [] -> Bool
  :doc "Verifies AST subtree CAS indexing, memoized lookup, and incremental diff reconciliation."
  (let [(src1 (str "(module asl-mem/test-source\n"
                   "  :doc \"Test source module\")\n\n"
                   "(dfs MyRecord\n"
                   "  (:f id Str \"record identifier\"))\n\n"
                   "(df compute-val [(x I64)] -> I64\n"
                   "  :doc \"computes value\"\n"
                   "  (+ x 10))\n"))
        (c1 (sc/index-subtrees src1))
        (entries1 (.-entries c1))
        (src2 (str "(module asl-mem/test-source\n"
                   "  :doc \"Test source module\")\n\n"
                   "(dfs MyRecord\n"
                   "  (:f id Str \"record identifier\"))\n\n"
                   "(df compute-val [(x I64)] -> I64\n"
                   "  :doc \"computes value\"\n"
                   "  (+ x 99))\n"))
        (res (sc/reconcile-incremental-diff c1 src2))
        (unchanged (first res))
        (c2 (second res))
        (src1-identical (sc/reconcile-incremental-diff c1 src1))
        (unchanged-all (first src1-identical))]
    (assert (= (map-size entries1) 3) "Source 1 must contain 3 indexed top-level subtrees")
    (assert (= (list-length unchanged) 2) "Source 2 must reuse exactly 2 unchanged subtrees from cache")
    (assert (= (map-size (.-entries c2)) 3) "Updated cache must contain 3 entries")
    (assert (= (list-length unchanged-all) 3) "Identical source must reuse 100% of subtrees from cache")
    true))

(df run-tests [] -> Bool
  :doc "Runs all hybrid search and prompt cache unit tests."
  (and (test-simhash-fnv-and-popcount)
       (and (test-simhash-identical-text)
            (and (test-simhash-small-perturbation)
                 (and (test-simhash-unrelated-text)
                      (and (test-rrf-scoring-and-fusion)
                           (and (test-prompt-cache-prefix-stability)
                                (test-subtree-memoization-and-incremental-diff))))))))
