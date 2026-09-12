(module asl-mem/bm25
  :d "Deterministic pure ASL Okapi BM25 lexical ranking engine."
  :x [Bm25Doc
      Bm25Index
      Bm25Score
      tokenize-terms
      make-bm25-index
      compute-idf
      calc-idf
      score-bm25
      sort-bm25-scores
      rank-bm25]
  :i [(math :a m)
      (simhash :a sh)])

(dfs Bm25Doc
  (:f id Str "Document or clause identifier")
  (:f terms (List Str) "Tokenized lowercase terms")
  (:f length I64 "Document token length"))

(dfs Bm25Index
  (:f doc-count I64 "Total number of documents")
  (:f avg-doc-length F64 "Average document token length")
  (:f docs (List Bm25Doc) "Indexed documents")
  (:f term-doc-counts (Map Str I64) "Document frequency (DF) per term"))

(dfs Bm25Score
  (:f doc-id Str "Document identifier")
  (:f score F64 "Computed BM25 score"))

(df tokenize-terms [(text Str)] -> (List Str)
  :d "Lowercases and splits text into alphanumeric term tokens."
  (let [(cleaned (sh/strip-punct text))
        (lowered (string-lower cleaned))
        (raw-tokens (string-split lowered " "))
        (trimmed (map string-trim raw-tokens))]
    (filter (fn [(t Str)] -> Bool (> (string-length t) 0)) trimmed)))

(df dedup-terms-loop [(terms (List Str)) (seen (Map Str Bool)) (acc (List Str))] -> (List Str)
  (if (list-empty? terms)
      acc
      (let [(t (option-or (list-head terms) ""))
            (rest (option-or (list-tail terms) (list)))]
        (if (map-has? seen t)
            (dedup-terms-loop rest seen acc)
            (dedup-terms-loop rest (map-set seen t true) (list-append acc (list t)))))))

(df dedup-terms [(terms (List Str))] -> (List Str)
  :d "Returns list of unique terms preserving order."
  (dedup-terms-loop terms (map-empty) (list)))

(df count-term-freq-loop [(terms (List Str)) (target Str) (count I64)] -> I64
  (if (list-empty? terms)
      count
      (let [(t (option-or (list-head terms) ""))
            (rest (option-or (list-tail terms) (list)))]
        (if (= t target)
            (count-term-freq-loop rest target (+ count 1))
            (count-term-freq-loop rest target count)))))

(df term-frequency [(terms (List Str)) (target Str)] -> I64
  :d "Counts occurrences of target term in document terms list."
  (count-term-freq-loop terms target 0))

(df update-df-counts-loop [(unique-terms (List Str)) (df-map (Map Str I64))] -> (Map Str I64)
  (if (list-empty? unique-terms)
      df-map
      (let [(t (option-or (list-head unique-terms) ""))
            (rest (option-or (list-tail unique-terms) (list)))
            (curr-df (mt (map-get df-map t) ((some c) c) ((none) 0)))]
        (update-df-counts-loop rest (map-set df-map t (+ curr-df 1))))))

(df build-df-map-loop [(docs (List Bm25Doc)) (df-map (Map Str I64))] -> (Map Str I64)
  (if (list-empty? docs)
      df-map
      (let [(d (option-or (list-head docs) (Bm25Doc :id "" :terms (list) :length 0)))
            (rest (option-or (list-tail docs) (list)))
            (unique-terms (dedup-terms (.-terms d)))
            (next-df (update-df-counts-loop unique-terms df-map))]
        (build-df-map-loop rest next-df))))

(df calc-total-length [(docs (List Bm25Doc)) (acc I64)] -> I64
  (if (list-empty? docs)
      acc
      (let [(d (option-or (list-head docs) (Bm25Doc :id "" :terms (list) :length 0)))
            (rest (option-or (list-tail docs) (list)))]
        (calc-total-length rest (+ acc (.-length d))))))

(df make-bm25-index [(docs (List Bm25Doc))] -> Bm25Index
  :d "Builds inverted term-document frequency index and average length."
  (let [(total (list-length docs))
        (tot-len (calc-total-length docs 0))
        (avg-len (if (= total 0) 0.0 (/ (int64-to-float64 tot-len) (int64-to-float64 total))))
        (df-map (build-df-map-loop docs (map-empty)))]
    (Bm25Index
      :doc-count total
      :avg-doc-length avg-len
      :docs docs
      :term-doc-counts df-map)))

(df compute-idf [(df-count I64) (total-docs I64)] -> F64
  :d "Computes smoothed Robertson-Sparck Jones IDF bounded below by 0.01."
  (let [(n-f (int64-to-float64 total-docs))
        (df-f (int64-to-float64 df-count))
        (numerator (+ (- n-f df-f) 0.5))
        (denominator (+ df-f 0.5))
        (arg (+ 1.0 (/ numerator denominator)))
        (raw-idf (m/fast-ln arg))]
    (if (< raw-idf 0.01)
        0.01
        raw-idf)))

(df calc-idf [(df-count I64) (total-docs I64)] -> F64
  :d "Alias for compute-idf."
  (compute-idf df-count total-docs))

(df score-bm25-term [(term Str) (doc Bm25Doc) (index Bm25Index) (k1 F64) (b F64)] -> F64
  (let [(tf (term-frequency (.-terms doc) term))]
    (if (<= tf 0)
        0.0
        (let [(tf-f (int64-to-float64 tf))
              (df-c (mt (map-get (.-term-doc-counts index) term) ((some c) c) ((none) 0)))
              (idf (compute-idf df-c (.-doc-count index)))
              (doc-len (int64-to-float64 (.-length doc)))
              (avg-len (.-avg-doc-length index))
              (len-ratio (if (<= avg-len 0.0) 1.0 (/ doc-len avg-len)))
              (k-val (* k1 (+ (- 1.0 b) (* b len-ratio))))
              (num (* tf-f (+ k1 1.0)))
              (denom (+ tf-f k-val))]
          (if (<= denom 0.0)
              0.0
              (* idf (/ num denom)))))))

(df score-bm25-loop [(q-terms (List Str)) (doc Bm25Doc) (index Bm25Index) (k1 F64) (b F64) (acc F64)] -> F64
  (if (list-empty? q-terms)
      acc
      (let [(t (option-or (list-head q-terms) ""))
            (rest (option-or (list-tail q-terms) (list)))
            (term-score (score-bm25-term t doc index k1 b))]
        (score-bm25-loop rest doc index k1 b (+ acc term-score)))))

(df score-bm25 [(doc Bm25Doc) (query-terms (List Str)) (index Bm25Index) (k1 F64) (b F64)] -> F64
  :d "Calculates Okapi BM25 relevance score for a document against query terms."
  (let [(unique-q (dedup-terms query-terms))]
    (score-bm25-loop unique-q doc index k1 b 0.0)))

(df insert-bm25-score [(item Bm25Score) (sorted (List Bm25Score))] -> (List Bm25Score)
  (if (list-empty? sorted)
      (list item)
      (let [(head-s (option-or (list-head sorted) (Bm25Score :doc-id "" :score 0.0)))
            (tail-s (option-or (list-tail sorted) (list)))]
        (if (> (.-score item) (.-score head-s))
            (list-cons item sorted)
            (list-cons head-s (insert-bm25-score item tail-s))))))

(df sort-bm25-scores-loop [(unsorted (List Bm25Score)) (sorted (List Bm25Score))] -> (List Bm25Score)
  (if (list-empty? unsorted)
      sorted
      (let [(item (option-or (list-head unsorted) (Bm25Score :doc-id "" :score 0.0)))
            (rest (option-or (list-tail unsorted) (list)))]
        (sort-bm25-scores-loop rest (insert-bm25-score item sorted)))))

(df sort-bm25-scores [(scores (List Bm25Score))] -> (List Bm25Score)
  :d "Pure ASL insertion sort descending by score."
  (sort-bm25-scores-loop scores (list)))

(df score-all-docs-loop [(docs (List Bm25Doc)) (q-terms (List Str)) (index Bm25Index) (k1 F64) (b F64) (acc (List Bm25Score))] -> (List Bm25Score)
  (if (list-empty? docs)
      acc
      (let [(d (option-or (list-head docs) (Bm25Doc :id "" :terms (list) :length 0)))
            (rest (option-or (list-tail docs) (list)))
            (s (score-bm25 d q-terms index k1 b))
            (score-rec (Bm25Score :doc-id (.-id d) :score s))]
        (score-all-docs-loop rest q-terms index k1 b (list-append acc (list score-rec))))))

(df rank-bm25 [(index Bm25Index) (query Str) (top-k I64)] -> (List Bm25Score)
  :d "Tokenizes query, computes scores across indexed documents, and returns top-k sorted items."
  (let [(q-terms (tokenize-terms query))
        (all-scores (score-all-docs-loop (.-docs index) q-terms index 1.5 0.75 (list)))
        (sorted (sort-bm25-scores all-scores))
        (k (max 0 top-k))]
    (list-take sorted (min (list-length sorted) k))))
