(module asl-mem/amnesia
  :d "Amnesia memory chunk DAG, millisecond timestamps, and confidence invariant enforcement in pure ASL."
  :x [Chunk
      ChunkDAG
      RetiredChunk
      validate-memory-chunk
      is-low-confidence?
      chunk-dag-empty
      chunk-dag-add
      chunk-dag-find
      detect-cycle?
      chunk-dag-ancestors
      evict-below-threshold
      transition-to-retired
      evaluate-retirement
      retire-chunk
      query-retired-chunks
      load-active-chunks]
  :i [(view_layer :a vl)
      (hydration :a h)])

(dfs Chunk
  (:f id Str "Unique memory chunk identifier")
  (:f ts I64 "Creation unix epoch timestamp in milliseconds")
  (:f conf F64 "Chunk factual confidence score between 0.0 and 1.0")
  (:f summary Str "Human and agent readable summary")
  (:f facts (List Str) "Factual scalar assertion list")
  (:f directives (List Str) "Operational constraints and behavioral directives")
  (:f refs (List Str) "Causal parent chunk identifiers in DAG"))

(dfs ChunkDAG
  (:f chunks (Map Str vl/MemoryChunk) "Memory chunks indexed by unique identifier")
  (:f roots (List Str) "Root chunk identifiers having zero parent references")
  (:f size I64 "Total number of committed chunks in DAG"))

(df validate-memory-chunk [(chunk vl/MemoryChunk)] -> Bool
  :d "Validates memory chunk structural boundaries timestamp positivity and confidence range."
  (and (> (string-length (.-id chunk)) 0)
       (and (> (.-ts chunk) 0)
            (and (>= (.-conf chunk) 0.0)
                 (<= (.-conf chunk) 1.0)))))

(df is-low-confidence? [(chunk vl/MemoryChunk) (threshold F64)] -> Bool
  :d "Predicate asserting whether chunk confidence falls below safety verification threshold."
  (< (.-conf chunk) threshold))

(df chunk-dag-empty [] -> ChunkDAG
  :d "Constructs an empty memory chunk directed acyclic graph."
  (ChunkDAG
    :chunks (map-empty)
    :roots (list)
    :size 0))

(df chunk-dag-find [(dag ChunkDAG) (id Str)] -> (Option vl/MemoryChunk)
  :d "Retrieves an indexed memory chunk from DAG by its unique identifier."
  (map-get (.-chunks dag) id))

(df collect-ancestors [(dag ChunkDAG) (id Str) (visited (List Str))] -> (List Str)
  :d "Internal helper recursively collecting transitive ancestor IDs in topological causal order."
  (mt (chunk-dag-find dag id)
    ((none) visited)
    ((some chunk)
     (let [(parents (.-refs chunk))]
       (fold (fn [(acc (List Str)) (p-id Str)] -> (List Str)
               (if (list-contains? acc p-id)
                   acc
                   (let [(acc-with-p (collect-ancestors dag p-id acc))]
                     (if (list-contains? acc-with-p p-id)
                         acc-with-p
                         (list-append acc-with-p (list p-id))))))
             visited
             parents)))))

(df chunk-dag-ancestors [(dag ChunkDAG) (chunk-id Str)] -> (List Str)
  :d "Traverses DAG topology to return all transitive ancestor chunk IDs in causal order."
  (collect-ancestors dag chunk-id (list)))

(df detect-cycle? [(dag ChunkDAG) (candidate vl/MemoryChunk)] -> Bool
  :d "Graph cycle detection preventing self-references and circular loops."
  (let [(cand-id (.-id candidate))
        (refs (.-refs candidate))]
    (if (list-contains? refs cand-id)
        true
        (fold (fn [(has-cycle Bool) (ref-id Str)] -> Bool
                (or has-cycle
                    (or (= ref-id cand-id)
                        (list-contains? (chunk-dag-ancestors dag ref-id) cand-id))))
              false
              refs))))

(df all-refs-exist? [(chunks (Map Str vl/MemoryChunk)) (refs (List Str))] -> Bool
  :d "Verifies that all parent reference IDs are present in the chunk map."
  (fold (fn [(ok-flag Bool) (r-id Str)] -> Bool
          (and ok-flag (map-has? chunks r-id)))
        true
        refs))

(df chunk-dag-add [(dag ChunkDAG) (chunk vl/MemoryChunk)] -> (Result ChunkDAG Str)
  :d "Inserts MemoryChunk into DAG while enforcing reference existence and acyclicity."
  (let [(c-id (.-id chunk))
        (refs (.-refs chunk))]
    (cond
      ((map-has? (.-chunks dag) c-id)
       (err "chunk ID already exists in DAG"))
      ((not (all-refs-exist? (.-chunks dag) refs))
       (err "missing parent reference in DAG"))
      ((detect-cycle? dag chunk)
       (err "cycle detected in chunk references"))
      (:else
       (let [(new-chunks (map-set (.-chunks dag) c-id chunk))
             (new-roots (if (list-empty? refs)
                            (list-append (.-roots dag) (list c-id))
                            (.-roots dag)))
             (new-size (+ (.-size dag) 1))]
         (ok (ChunkDAG :chunks new-chunks :roots new-roots :size new-size)))))))

(df evict-below-threshold [(chunks (List vl/MemoryChunk)) (threshold F64)] -> (List vl/MemoryChunk)
  :d "Evicts memory chunks whose confidence score falls strictly below safety threshold."
  (filter (fn [(c vl/MemoryChunk)] -> Bool
            (not (is-low-confidence? c threshold)))
          chunks))

(dfs RetiredChunk
  (:f chunk vl/MemoryChunk "Underlying memory chunk transitioned to retired state")
  (:f status Str "Lifecycle status: retired")
  (:f closure-condition Str "Closure condition expression that was satisfied")
  (:f retired-at I64 "Millisecond timestamp when retired"))

(df transition-to-retired [(chunk vl/MemoryChunk) (closure-condition Str) (now-ts I64)] -> RetiredChunk
  :d "Transitions an active memory chunk to retired state preserving full causal metadata."
  (RetiredChunk
    :chunk chunk
    :status "retired"
    :closure-condition closure-condition
    :retired-at now-ts))

(df evaluate-retirement [(chunk vl/MemoryChunk) (closure-condition Str) (is-satisfied Bool) (now-ts I64)] -> (Option RetiredChunk)
  :d "Evaluates closure condition for retirement; if satisfied, transitions chunk to retired state."
  (if is-satisfied
    (some (transition-to-retired chunk closure-condition now-ts))
    (none)))

(df retire-chunk [(dag ChunkDAG) (chunk-id Str) (closure-condition Str) (now-ts I64)] -> (Option RetiredChunk)
  :d "Retires a chunk from active DAG if present and transition criteria are met."
  (mt (chunk-dag-find dag chunk-id)
    ((none) (none))
    ((some c) (some (transition-to-retired c closure-condition now-ts)))))

(df query-retired-chunks [(retired-list (List RetiredChunk)) (query Str)] -> (List RetiredChunk)
  :d "Queries the retired chunk store without loading retired records into active working context."
  (filter (fn [(rc RetiredChunk)] -> Bool
            (let [(c (.-chunk rc))]
              (or (string-contains? (.-id c) query)
                  (or (string-contains? (.-summary c) query)
                      (string-contains? (.-closure-condition rc) query)))))
          retired-list))

(df load-active-chunks [(dag ChunkDAG) (retired-ids (List Str))] -> (List vl/MemoryChunk)
  :d "Loads only active, non-retired memory chunks into working context."
  (let [(all-chunks (map-values (.-chunks dag)))]
    (filter (fn [(c vl/MemoryChunk)] -> Bool
              (not (list-contains? retired-ids (.-id c))))
            all-chunks)))
