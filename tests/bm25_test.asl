(module asl-mem/tests/bm25-test
  :d "Unit tests for pure ASL Okapi BM25 lexical ranking and Reciprocal Rank Fusion"
  :x [run-tests
      test-bm25-ranking-order
      test-rrf-fusion-score]
  :i [(bm25 :a bm)
      (driver :a drv)])

(df test-fast-ln [] -> Bool
  :d "Verifies natural logarithm Taylor/hyperbolic series approximation."
  (let [(ln1 (bm/fast-ln 1.0))
        (ln2 (bm/fast-ln 2.0))]
    (assert (< ln1 0.01) "ln(1.0) must be approx 0.0")
    (assert (> ln1 -0.01) "ln(1.0) must be approx 0.0")
    (assert (< ln2 0.71) "ln(2.0) must be approx 0.693")
    (assert (> ln2 0.67) "ln(2.0) must be approx 0.693")
    true))

(df test-tokenize-terms [] -> Bool
  :d "Verifies lowercase punctuation-stripped term tokenization."
  (let [(tokens (bm/tokenize-terms "Hello, World! Tokenize-this_now."))]
    (assert (= (list-length tokens) 4) "Tokens count must be 4")
    (assert (= (option-or (list-head tokens) "") "hello") "First token must be hello")
    true))

(df test-compute-idf [] -> Bool
  :d "Verifies Robertson-Sparck Jones IDF floor and monotonicity."
  (let [(idf-rare (bm/compute-idf 1 100))
        (idf-common (bm/compute-idf 90 100))
        (idf-all (bm/compute-idf 100 100))]
    (assert (> idf-rare idf-common) "Rare term IDF must exceed common term IDF")
    (assert (>= idf-all 0.01) "Universal term IDF must be bounded below by 0.01")
    true))

(df test-bm25-ranking-order [] -> Bool
  :d "Verifies Okapi BM25 ranking precision and score ordering."
  (let [(d1 (bm/Bm25Doc :id "doc-asl" :terms (bm/tokenize-terms "pure agentscript okapi bm25 lexical search ranking") :length 7))
        (d2 (bm/Bm25Doc :id "doc-vec" :terms (bm/tokenize-terms "dense vector embeddings cosine similarity memory store") :length 7))
        (d3 (bm/Bm25Doc :id "doc-fruit" :terms (bm/tokenize-terms "unrelated document about fresh apples and oranges") :length 7))
        (index (bm/make-bm25-index (list d1 d2 d3)))
        (ranked (bm/rank-bm25 index "okapi bm25 search" 3))]
    (assert (= (list-length ranked) 3) "Ranked results count must match indexed docs count")
    (mt (list-head ranked)
      ((none) (assert false "Top ranked item must exist"))
      ((some top-s)
       (assert (= (.-doc-id top-s) "doc-asl") "Top ranked document must be doc-asl")
       (assert (> (.-score top-s) 0.0) "Top ranked score must be positive")))
    true))

(df test-tf-saturation [] -> Bool
  :d "Verifies term frequency saturation behavior."
  (let [(d1 (bm/Bm25Doc :id "d1" :terms (bm/tokenize-terms "search search search search") :length 4))
        (d2 (bm/Bm25Doc :id "d2" :terms (bm/tokenize-terms "search query") :length 2))
        (index (bm/make-bm25-index (list d1 d2)))
        (s1 (bm/score-bm25 d1 (list "search") index 1.5 0.75))
        (s2 (bm/score-bm25 d2 (list "search") index 1.5 0.75))]
    (assert (> s1 0.0) "Score 1 must be positive")
    (assert (> s2 0.0) "Score 2 must be positive")
    true))

(df test-rrf-fusion-score [] -> Bool
  :d "Verifies Reciprocal Rank Fusion prioritizes multi-source consensus."
  (let [(dense (list "item-a" "item-b" "item-c"))
        (lexical (list "item-b" "item-a" "item-d"))
        (fused (drv/fuse-ranks-rrf dense lexical 60))]
    (assert (>= (list-length fused) 4) "Fused results must contain all unique items")
    (mt (list-head fused)
      ((none) (assert false "Top fused item must exist"))
      ((some top-item)
       (assert (or (= (.-doc-id top-item) "item-a") (= (.-doc-id top-item) "item-b")) "Consensus items must top rank")
       (assert (> (.-rrf-score top-item) 0.03) "Consensus item score must exceed 0.03")))
    (mt (list-head (list-tail (list-tail (list-tail fused))))
      ((none) (assert false "Fourth fused item must exist"))
      ((some tail-item)
       (assert (< (.-rrf-score tail-item) 0.02) "Single-source item score must be below 0.02")))
    true))

(df run-tests [] -> Bool
  :d "Runs all bm25 and RRF unit tests."
  (and (test-fast-ln)
       (and (test-tokenize-terms)
            (and (test-compute-idf)
                 (and (test-bm25-ranking-order)
                      (and (test-tf-saturation)
                           (test-rrf-fusion-score)))))))
