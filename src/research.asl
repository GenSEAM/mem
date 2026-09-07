(module asl-mem/research
  :d "Research Memory Cluster & Attributed QA Knowledge Cache"
  :x [ResearchFact
      ResearchQueryOutcome
      make-research-fact
      query-research-memory
      store-research-fact]
  :i [])

(dfs ResearchFact
  (:f id Str "Fact identifier")
  (:f query Str "Original semantic query")
  (:f fact Str "Canonical synthesized knowledge fact")
  (:f source Str "Document citation or URL")
  (:f confidence F64 "Confidence rating [0.0 - 1.0]")
  (:f timestamp I64 "Epoch timestamp of extraction"))

(dfs ResearchQueryOutcome
  (:f is-direct-hit Bool "True if high-confidence cache hit (>=0.85)")
  (:f fact Str "Retrieved or proxied fact")
  (:f token-cost I64 "Token overhead incurred (0 on direct hit)")
  (:f delegated Bool "True if forwarded to research agent"))

(df make-research-fact [(id Str) (query Str) (fact Str) (source Str) (confidence F64)] -> ResearchFact
  :d "Constructs canonical research knowledge fact record."
  (ResearchFact
    :id id
    :query query
    :fact fact
    :source source
    :confidence confidence
    :timestamp 0))

(df store-research-fact [(fact ResearchFact) (index (List ResearchFact))] -> (List ResearchFact)
  :d "Appends or updates verified research fact in memory cache."
  (if (or (= index null) (list-empty? index))
      (list fact)
      (let [(filtered (filter (fn [(f ResearchFact)] -> Bool
                                (and (!= (.-id f) (.-id fact))
                                     (!= (.-query f) (.-query fact))))
                              index))]
        (list-cons fact filtered))))

(df find-matching-fact-loop [(q Str) (facts (List ResearchFact))] -> (Option ResearchFact)
  (if (list-empty? facts)
      (none)
      (let [(f (option-or (list-head facts) (ResearchFact :id "" :query "" :fact "" :source "" :confidence 0.0 :timestamp 0)))
            (rest (option-or (list-tail facts) (list)))
            (f-query (.-query f))]
        (if (or (= f-query q)
                (or (string-contains? f-query q)
                    (string-contains? q f-query)))
            (some f)
            (find-matching-fact-loop q rest)))))

(df find-matching-fact [(q Str) (facts (List ResearchFact))] -> (Option ResearchFact)
  :d "Searches facts index for exact or semantic query match."
  (find-matching-fact-loop q facts))

(df query-research-memory [(query Str) (index (List ResearchFact)) (threshold F64)] -> ResearchQueryOutcome
  :d "Evaluates direct cache hit vs low-confidence delegation."
  (let [(facts (if (= index null) (list) index))
        (min-conf (if (or (= threshold null) (<= threshold 0.0)) 0.85 threshold))
        (matched-opt (find-matching-fact query facts))]
    (mt matched-opt
      ((none)
       (ResearchQueryOutcome
         :is-direct-hit false
         :fact ""
         :token-cost 150
         :delegated true))
      ((some f)
       (if (>= (.-confidence f) min-conf)
           (ResearchQueryOutcome
             :is-direct-hit true
             :fact (.-fact f)
             :token-cost 0
             :delegated false)
           (ResearchQueryOutcome
             :is-direct-hit false
             :fact (.-fact f)
             :token-cost 150
             :delegated true))))))
