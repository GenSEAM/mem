(module asl-mem/driver
  :d "Pure AgentScript memory matrix driver, vector store adapter, and hybrid rank fusion."
  :x [VectorRecord
      create-record
      cosine-similarity
      RrfScore
      sort-rrf-scores
      fuse-ranks-rrf
      reciprocal-rank-fusion]
  :i [(math :a m)])

(dfs VectorRecord
  (:f id Str "Unique memory entry ID")
  (:f content Str "Indexed semantic text")
  (:f embedding (List F64) "Dense vector embedding"))

(dfs RrfScore
  (:f doc-id Str "Document or clause identifier")
  (:f rrf-score F64 "Fused reciprocal rank score")
  (:f dense-rank I64 "Rank in dense vector retrieval")
  (:f bm25-rank I64 "Rank in lexical retrieval"))

(df create-record [(id Str) (text Str) (emb (List F64))] -> VectorRecord
  :d "Constructs vector record instance."
  (VectorRecord :id id :content text :embedding emb))

(df cosine-similarity [(v1 (List F64)) (v2 (List F64))] -> F64
  :d "Computes cosine similarity between two vectors via Euclidean L2 normalization."
  (m/vector-cosine-sim v1 v2))

(df find-rank-loop [(id Str) (items (List Str)) (idx I64)] -> I64
  (if (list-empty? items)
      -1
      (let [(h (option-or (list-head items) ""))
            (rest (option-or (list-tail items) (list)))]
        (if (= h id)
            idx
            (find-rank-loop id rest (+ idx 1))))))

(df find-rank [(id Str) (items (List Str))] -> I64
  :d "Finds 1-based index of identifier in ranked list or -1 if absent."
  (find-rank-loop id items 1))

(df dedup-str-loop [(items (List Str)) (seen (Map Str Bool)) (acc (List Str))] -> (List Str)
  (if (list-empty? items)
      acc
      (let [(h (option-or (list-head items) ""))
            (rest (option-or (list-tail items) (list)))]
        (if (map-has? seen h)
            (dedup-str-loop rest seen acc)
            (dedup-str-loop rest (map-set seen h true) (list-append acc (list h)))))))

(df dedup-str-list [(items (List Str))] -> (List Str)
  :d "Deduplicates list of identifiers."
  (dedup-str-loop items (map-empty) (list)))

(df compute-single-rrf [(id Str) (dense-ids (List Str)) (lexical-ids (List Str)) (k I64)] -> RrfScore
  (let [(d-rank (find-rank id dense-ids))
        (l-rank (find-rank id lexical-ids))
        (k-f (int64-to-float64 (max 1 k)))
        (d-score (if (> d-rank 0) (/ 1.0 (+ k-f (int64-to-float64 d-rank))) 0.0))
        (l-score (if (> l-rank 0) (/ 1.0 (+ k-f (int64-to-float64 l-rank))) 0.0))
        (total-score (+ d-score l-score))]
    (RrfScore
      :doc-id id
      :rrf-score total-score
      :dense-rank d-rank
      :bm25-rank l-rank)))

(df insert-rrf-score [(item RrfScore) (sorted (List RrfScore))] -> (List RrfScore)
  (if (list-empty? sorted)
      (list item)
      (let [(head-s (option-or (list-head sorted) (RrfScore :doc-id "" :rrf-score 0.0 :dense-rank 0 :bm25-rank 0)))
            (tail-s (option-or (list-tail sorted) (list)))]
        (if (> (.-rrf-score item) (.-rrf-score head-s))
            (list-cons item sorted)
            (list-cons head-s (insert-rrf-score item tail-s))))))

(df sort-rrf-scores-loop [(unsorted (List RrfScore)) (sorted (List RrfScore))] -> (List RrfScore)
  (if (list-empty? unsorted)
      sorted
      (let [(item (option-or (list-head unsorted) (RrfScore :doc-id "" :rrf-score 0.0 :dense-rank 0 :bm25-rank 0)))
            (rest (option-or (list-tail unsorted) (list)))]
        (sort-rrf-scores-loop rest (insert-rrf-score item sorted)))))

(df sort-rrf-scores [(scores (List RrfScore))] -> (List RrfScore)
  :d "Pure ASL insertion sort descending by fused RRF score."
  (sort-rrf-scores-loop scores (list)))

(df fuse-ranks-rrf [(dense-ids (List Str)) (lexical-ids (List Str)) (k I64)] -> (List RrfScore)
  :d "Merges dense and lexical ranked candidate lists using Reciprocal Rank Fusion (default k = 60)."
  (let [(all-ids (dedup-str-list (list-append dense-ids lexical-ids)))
        (k-val (if (<= k 0) 60 k))
        (scores (map (fn [(id Str)] -> RrfScore
                       (compute-single-rrf id dense-ids lexical-ids k-val))
                     all-ids))]
    (sort-rrf-scores scores)))

(df reciprocal-rank-fusion [(dense-ids (List Str)) (lexical-ids (List Str)) (k I64)] -> (List RrfScore)
  :d "Alias for fuse-ranks-rrf combining vector and lexical rankings."
  (fuse-ranks-rrf dense-ids lexical-ids k))

