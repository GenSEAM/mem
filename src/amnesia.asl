(module asl-mem/amnesia
  :d "Amnesia memory chunk DAG, millisecond timestamps, and confidence invariant enforcement in pure ASL."
  :x [Chunk
      MemoryChunk
      ChunkDAG
      now-ms
      make-memory-chunk
      validate-memory-chunk
      is-low-confidence?
      chunk-dag-empty
      chunk-dag-add
      chunk-dag-find
      detect-cycle?
      chunk-dag-ancestors]
  :i [])

(dfs Chunk
  (:f id Str "Unique memory chunk identifier")
  (:f ts I64 "Creation unix epoch timestamp in milliseconds")
  (:f conf F64 "Chunk factual confidence score between 0.0 and 1.0")
  (:f summary Str "Human and agent readable summary")
  (:f facts (List Str) "Factual scalar assertion list")
  (:f directives (List Str) "Operational constraints and behavioral directives")
  (:f refs (List Str) "Causal parent chunk identifiers in DAG"))

(dfs MemoryChunk
  (:f id Str "Canonical memory chunk record")
  (:f ts I64 "Creation unix epoch timestamp in milliseconds")
  (:f conf F64 "Chunk factual confidence score between 0.0 and 1.0")
  (:f summary Str "Human and agent readable summary")
  (:f facts (List Str) "Factual scalar assertion list")
  (:f directives (List Str) "Operational constraints and behavioral directives")
  (:f refs (List Str) "Causal parent chunk identifiers in DAG"))

(dfs ChunkDAG
  (:f chunks (Map Str MemoryChunk) "Memory chunks indexed by unique identifier")
  (:f roots (List Str) "Root chunk identifiers having zero parent references")
  (:f size I64 "Total number of committed chunks in DAG"))

(df now-ms [(base I64) (delta I64)] -> I64
  :d "Calculates millisecond epoch timestamp."
  (+ base delta))

(df make-memory-chunk [(id Str) (ts I64) (conf F64) (summary Str) (facts (List Str)) (directives (List Str)) (refs (List Str))] -> MemoryChunk
  :d "Constructs a MemoryChunk record with millisecond timestamp confidence and DAG reference links."
  (MemoryChunk
    :id id
    :ts ts
    :conf conf
    :summary summary
    :facts facts
    :directives directives
    :refs refs))

(df validate-memory-chunk [(chunk MemoryChunk)] -> Bool
  :d "Validates memory chunk structural boundaries timestamp positivity and confidence range."
  (and (> (string-length (.-id chunk)) 0)
       (and (> (.-ts chunk) 0)
            (and (>= (.-conf chunk) 0.0)
                 (<= (.-conf chunk) 1.0)))))

(df is-low-confidence? [(chunk MemoryChunk) (threshold F64)] -> Bool
  :d "Predicate asserting whether chunk confidence falls below safety verification threshold."
  (< (.-conf chunk) threshold))

(df chunk-dag-empty [] -> ChunkDAG
  :d "Constructs an empty memory chunk directed acyclic graph."
  (ChunkDAG
    :chunks (map-empty)
    :roots (list)
    :size 0))

(df chunk-dag-find [(dag ChunkDAG) (id Str)] -> (Option MemoryChunk)
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

(df detect-cycle? [(dag ChunkDAG) (candidate MemoryChunk)] -> Bool
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

(df all-refs-exist? [(chunks (Map Str MemoryChunk)) (refs (List Str))] -> Bool
  :d "Verifies that all parent reference IDs are present in the chunk map."
  (fold (fn [(ok-flag Bool) (r-id Str)] -> Bool
          (and ok-flag (map-has? chunks r-id)))
        true
        refs))

(df chunk-dag-add [(dag ChunkDAG) (chunk MemoryChunk)] -> (Result ChunkDAG Str)
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
