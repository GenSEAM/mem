(module asl-mem/context-scoring
  :d "Multi-factor context utility scoring and anti-bait eviction governor"
  :x [ContextScoreRecord
      ActiveVolumeReport
      make-context-record
      calculate-context-score
      penalize-unreferenced-matches
      evict-records-below-threshold
      calculate-active-volume
      is-volume-bounded?
      report-active-volume]
  :i [(math :a m)])

(dfs ContextScoreRecord
  (:f id Str "Context item or invariant unique identifier")
  (:f is-invariant Bool "True if item is a pinned architectural invariant")
  (:f invariant-weight F64 "Weight multiplier protecting invariant rules")
  (:f retrieval-cost F64 "Cost to re-derive or re-fetch item")
  (:f reference-links I64 "Number of inbound references or active citations")
  (:f delta-turns I64 "Turns elapsed since last active utilization")
  (:f unreferenced-matches I64 "Number of times retrieved by search but unreferenced")
  (:f bait-penalty F64 "Computed anti-bait utility penalty")
  (:f utility-score F64 "Final composite context utility score"))

(df calculate-context-score [(record ContextScoreRecord) (lambda-decay F64) (bait-penalty-rate F64)] -> ContextScoreRecord
  :d "Calculates multi-factor utility score protecting invariants and penalizing unreferenced bait."
  (let [(inv-term (if (.-is-invariant record) (.-invariant-weight record) 0.0))
        (cost-term (.-retrieval-cost record))
        (links-term (int64-to-float64 (.-reference-links record)))
        (dt-float (int64-to-float64 (.-delta-turns record)))
        (dt-term (* lambda-decay (m/fast-ln (+ 1.0 dt-float))))
        (unref-float (int64-to-float64 (.-unreferenced-matches record)))
        (computed-bait (* unref-float bait-penalty-rate))
        (final-score (- (+ (+ inv-term cost-term) links-term) (+ dt-term computed-bait)))]
    (ContextScoreRecord
      :id (.-id record)
      :is-invariant (.-is-invariant record)
      :invariant-weight (.-invariant-weight record)
      :retrieval-cost cost-term
      :reference-links (.-reference-links record)
      :delta-turns (.-delta-turns record)
      :unreferenced-matches (.-unreferenced-matches record)
      :bait-penalty computed-bait
      :utility-score final-score)))

(df make-context-record [(id Str) (is-invariant Bool) (retrieval-cost F64) (reference-links I64) (delta-turns I64)] -> ContextScoreRecord
  :d "Constructs an initial context score record with standard default invariant weights."
  (let [(weight (if is-invariant 50.0 0.0))
        (base-rec (ContextScoreRecord
                    :id id
                    :is-invariant is-invariant
                    :invariant-weight weight
                    :retrieval-cost retrieval-cost
                    :reference-links reference-links
                    :delta-turns delta-turns
                    :unreferenced-matches 0
                    :bait-penalty 0.0
                    :utility-score 0.0))]
    (calculate-context-score base-rec 1.0 2.5)))

(df penalize-unreferenced-matches [(record ContextScoreRecord) (unreferenced-inc I64)] -> ContextScoreRecord
  :d "Increments unreferenced search match count and reapplies anti-bait penalty."
  (let [(new-unref (+ (.-unreferenced-matches record) unreferenced-inc))
        (updated-rec (ContextScoreRecord
                       :id (.-id record)
                       :is-invariant (.-is-invariant record)
                       :invariant-weight (.-invariant-weight record)
                       :retrieval-cost (.-retrieval-cost record)
                       :reference-links (.-reference-links record)
                       :delta-turns (.-delta-turns record)
                       :unreferenced-matches new-unref
                       :bait-penalty (.-bait-penalty record)
                       :utility-score (.-utility-score record)))]
    (calculate-context-score updated-rec 1.0 2.5)))

(df evict-records-below-threshold [(records (List ContextScoreRecord)) (threshold F64)] -> (List ContextScoreRecord)
  :d "Filters out context items scoring strictly below the eviction threshold unless pinned invariant."
  (fold (fn [(acc (List ContextScoreRecord)) (rec ContextScoreRecord)] -> (List ContextScoreRecord)
          (if (or (.-is-invariant rec) (>= (.-utility-score rec) threshold))
            (list-append acc (list rec))
            acc))
        (list)
        records))

(dfs ActiveVolumeReport
  (:f volume I64 "Current active volume of memory context")
  (:f ceiling I64 "Ceiling threshold for active volume")
  (:f bounded Bool "Whether active volume is within ceiling bound"))

(df calculate-active-volume [(records (List ContextScoreRecord))] -> I64
  :d "Calculates the total active volume from records."
  (list-length records))

(df is-volume-bounded? [(volume I64) (ceiling I64)] -> Bool
  :d "Checks if active volume is bounded within maximum volume ceiling."
  (<= volume ceiling))

(df report-active-volume [(records (List ContextScoreRecord)) (ceiling I64)] -> ActiveVolumeReport
  :d "Reports structured metrics for active context volume and bound enforcement."
  (let [(vol (calculate-active-volume records))
        (bounded (is-volume-bounded? vol ceiling))]
    (ActiveVolumeReport
      :volume vol
      :ceiling ceiling
      :bounded bounded)))
