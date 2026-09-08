(module asl-mem/rrf
  :doc "Reciprocal Rank Fusion (RRF) combining lexical BM25 and SimHash semantic recall."
  :x [RankedCandidate
      compute-rrf-score
      fuse-ranked-channels
      hybrid-code-search]
  :i [])

(dfs RankedCandidate
  (:f id Str "Candidate document or code snippet identifier")
  (:f score F64 "Fused or channel relevance score")
  (:f rank I64 "Ordinal rank position in channel (1-indexed)"))

(df compute-rrf-score [(rank I64) (k I64)] -> F64
  :doc "Computes standard Reciprocal Rank Fusion component score 1 / (k + rank)."
  (if (<= rank 0)
      0.0
      (let [(k-val (if (<= k 0) 60 k))
            (denom (+ (int64-to-float64 k-val) (int64-to-float64 rank)))]
        (/ 1.0 denom))))

(df accumulate-channel [(channel (List RankedCandidate)) (k I64) (acc-map (Map Str F64))] -> (Map Str F64)
  :doc "Accumulates RRF scores from candidate channel into map."
  (if (list-empty? channel)
      acc-map
      (let [(cand (option-or (list-head channel) (RankedCandidate :id "" :score 0.0 :rank 0)))
            (rest (option-or (list-tail channel) (list)))
            (id (.-id cand))
            (r (.-rank cand))
            (s (compute-rrf-score r k))
            (prev (mt (map-get acc-map id) ((some val) val) ((none) 0.0)))
            (next-map (map-set acc-map id (+ prev s)))]
        (accumulate-channel rest k next-map))))

(df insert-candidate [(item RankedCandidate) (sorted (List RankedCandidate))] -> (List RankedCandidate)
  :doc "Inserts RankedCandidate into sorted list in descending score order."
  (if (list-empty? sorted)
      (list item)
      (let [(head-c (option-or (list-head sorted) (RankedCandidate :id "" :score 0.0 :rank 0)))
            (tail-c (option-or (list-tail sorted) (list)))]
        (if (> (.-score item) (.-score head-c))
            (list-cons item sorted)
            (list-cons head-c (insert-candidate item tail-c))))))

(df sort-candidates-loop [(unsorted (List RankedCandidate)) (sorted (List RankedCandidate))] -> (List RankedCandidate)
  :doc "Sorts list of RankedCandidates descending by score."
  (if (list-empty? unsorted)
      sorted
      (let [(item (option-or (list-head unsorted) (RankedCandidate :id "" :score 0.0 :rank 0)))
            (rest (option-or (list-tail unsorted) (list)))]
        (sort-candidates-loop rest (insert-candidate item sorted)))))

(df sort-candidates [(candidates (List RankedCandidate))] -> (List RankedCandidate)
  :doc "Sorts candidate list descending by score."
  (sort-candidates-loop candidates (list)))

(df assign-ranks-loop [(candidates (List RankedCandidate)) (current-rank I64) (acc (List RankedCandidate))] -> (List RankedCandidate)
  :doc "Assigns 1-indexed rank order to sorted candidates."
  (if (list-empty? candidates)
      acc
      (let [(cand (option-or (list-head candidates) (RankedCandidate :id "" :score 0.0 :rank 0)))
            (rest (option-or (list-tail candidates) (list)))
            (updated (RankedCandidate :id (.-id cand) :score (.-score cand) :rank current-rank))]
        (assign-ranks-loop rest (+ current-rank 1) (list-append acc (list updated))))))

(df assign-ranks [(candidates (List RankedCandidate))] -> (List RankedCandidate)
  :doc "Assigns 1-indexed ranks to candidate list."
  (assign-ranks-loop candidates 1 (list)))

(df map-to-candidates [(keys (List Str)) (scores (Map Str F64))] -> (List RankedCandidate)
  :doc "Converts score map keys to candidate list."
  (if (list-empty? keys)
      (list)
      (let [(k (option-or (list-head keys) ""))
            (rest (option-or (list-tail keys) (list)))
            (s (mt (map-get scores k) ((some v) v) ((none) 0.0)))
            (cand (RankedCandidate :id k :score s :rank 0))]
        (list-cons cand (map-to-candidates rest scores)))))

(df fuse-ranked-channels [(channel-a (List RankedCandidate)) (channel-b (List RankedCandidate)) (k I64)] -> (List RankedCandidate)
  :doc "Fuses two ranked candidate channels using Reciprocal Rank Fusion."
  (let [(k-val (if (<= k 0) 60 k))
        (map-a (accumulate-channel channel-a k-val (map-empty)))
        (map-both (accumulate-channel channel-b k-val map-a))
        (keys (map-keys map-both))
        (unsorted (map-to-candidates keys map-both))
        (sorted (sort-candidates unsorted))]
    (assign-ranks sorted)))

(df hybrid-code-search [(bm25-results (List RankedCandidate)) (simhash-results (List RankedCandidate))] -> (List RankedCandidate)
  :doc "Executes unified hybrid code search fusing BM25 lexical and SimHash semantic candidate channels with standard k=60."
  (fuse-ranked-channels bm25-results simhash-results 60))
