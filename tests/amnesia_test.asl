(module asl-mem/tests/amnesia-test
  :d "Unit verification suite for Amnesia MemoryChunk, ChunkDAG, cycle detection, and millisecond timestamps."
  :x [run-tests
      test-chunk-creation
      test-chunk-validation
      test-dag-construction
      test-dag-ancestors
      test-cycle-prevention]
  :i [(amnesia :a a)
      (view_layer :a vl)
      (hydration :a h)])

(df test-chunk-creation [] -> Bool
  :d "Verifies construction and accessors of MemoryChunk with millisecond timestamp."
  (let [(c (vl/make-memory-chunk
             "chunk-001"
             (h/now-ms 1725769200000 123)
             0.95
             "User requested memory persistence"
             (list "fact-1: agent initialized" "fact-2: DAG enabled")
             (list "directive-1: enforce acyclicity")
             (list)))]
    (assert (= (.-id c) "chunk-001") "Chunk ID must match")
    (assert (= (.-ts c) 1725769200123) "Millisecond timestamp must match computed value")
    (assert (= (.-conf c) 0.95) "Chunk confidence score must match")
    (assert (= (list-length (.-facts c)) 2) "Chunk must contain two scalar facts")
    (assert (= (list-length (.-directives c)) 1) "Chunk must contain one directive")
    true))

(df test-chunk-validation [] -> Bool
  :d "Verifies boundary conditions for timestamps, confidence scores, and low-confidence flags."
  (let [(c-valid (vl/make-memory-chunk "c-v" 1725769200000 0.85 "valid" (list) (list) (list)))
        (c-zero-ts (vl/make-memory-chunk "c-zts" 0 0.85 "zero ts" (list) (list) (list)))
        (c-neg-conf (vl/make-memory-chunk "c-nc" 1000 -0.1 "neg conf" (list) (list) (list)))
        (c-high-conf (vl/make-memory-chunk "c-hc" 1000 1.2 "high conf" (list) (list) (list)))
        (c-low (vl/make-memory-chunk "c-low" 1000 0.45 "low conf" (list) (list) (list)))
        (c-high (vl/make-memory-chunk "c-high" 1000 0.92 "high conf" (list) (list) (list)))]
    (assert (a/validate-memory-chunk c-valid) "Valid chunk must pass validation")
    (assert (not (a/validate-memory-chunk c-zero-ts)) "Chunk with non-positive timestamp must fail validation")
    (assert (not (a/validate-memory-chunk c-neg-conf)) "Chunk with negative confidence must fail validation")
    (assert (not (a/validate-memory-chunk c-high-conf)) "Chunk with confidence exceeding 1.0 must fail validation")
    (assert (a/is-low-confidence? c-low 0.70) "Chunk with 0.45 confidence must be flagged as low confidence")
    (assert (not (a/is-low-confidence? c-high 0.70)) "Chunk with 0.92 confidence must not be flagged as low confidence")
    true))

(df test-dag-construction [] -> Bool
  :d "Verifies DAG initialization, root insertion, child insertion, and chunk retrieval."
  (let [(d0 (a/chunk-dag-empty))
        (r1 (vl/make-memory-chunk "root-1" 1000 0.90 "root chunk" (list) (list) (list)))
        (res-add-r1 (a/chunk-dag-add d0 r1))]
    (assert (is-ok? res-add-r1) "Adding root chunk to empty DAG must succeed")
    (let [(d1 (result-or res-add-r1 d0))
          (c1 (vl/make-memory-chunk "child-1" 2000 0.88 "child chunk" (list) (list) (list "root-1")))
          (res-add-c1 (a/chunk-dag-add d1 c1))]
      (assert (is-ok? res-add-c1) "Adding child chunk referencing existing root must succeed")
      (let [(d2 (result-or res-add-c1 d1))
            (lookup-root (a/chunk-dag-find d2 "root-1"))
            (lookup-child (a/chunk-dag-find d2 "child-1"))]
        (assert (= (.-size d2) 2) "DAG size after two insertions must equal 2")
        (assert (is-some? lookup-root) "Root chunk must be retrievable by ID")
        (assert (is-some? lookup-child) "Child chunk must be retrievable by ID")
        true))))

(df test-dag-ancestors [] -> Bool
  :d "Verifies transitive causal ancestor traversal across linear chains and diamond topologies."
  (let [(d0 (a/chunk-dag-empty))
        (na (vl/make-memory-chunk "A" 1000 0.95 "Node A" (list) (list) (list)))
        (d1 (result-or (a/chunk-dag-add d0 na) d0))
        (nb (vl/make-memory-chunk "B" 2000 0.95 "Node B" (list) (list) (list "A")))
        (d2 (result-or (a/chunk-dag-add d1 nb) d1))
        (nc (vl/make-memory-chunk "C" 3000 0.95 "Node C" (list) (list) (list "B")))
        (d3 (result-or (a/chunk-dag-add d2 nc) d2))]
    (assert (= (list-length (a/chunk-dag-ancestors d3 "A")) 0) "Root node must have zero ancestors")
    (assert (= (list-length (a/chunk-dag-ancestors d3 "C")) 2) "Linear chain child C must have 2 transitive ancestors")
    (assert (= (option-or (list-head (a/chunk-dag-ancestors d3 "C")) "") "A") "Oldest ancestor A must precede intermediate B in linear chain")
    (let [(nc-dia (vl/make-memory-chunk "C2" 2500 0.95 "Node C2" (list) (list) (list "A")))
          (d-dia1 (result-or (a/chunk-dag-add d2 nc-dia) d2))
          (nd (vl/make-memory-chunk "D" 4000 0.95 "Node D" (list) (list) (list "B" "C2")))
          (d-diamond (result-or (a/chunk-dag-add d-dia1 nd) d-dia1))]
      (assert (= (list-length (a/chunk-dag-ancestors d-diamond "D")) 3) "Diamond join node D must have 3 deduplicated ancestors")
      (assert (= (option-or (list-head (a/chunk-dag-ancestors d-diamond "D")) "") "A") "Root A must precede branch nodes B and C2 in diamond topological order")
      true)))

(df test-cycle-prevention [] -> Bool
  :d "Verifies cycle detection and rejection of self-loops, 2-node cycles, indirect cycles, and orphan chunks."
  (let [(d0 (a/chunk-dag-empty))
        (c-self (vl/make-memory-chunk "A" 1000 0.90 "self loop" (list) (list) (list "A")))]
    (assert (a/detect-cycle? d0 c-self) "Self-referential chunk A -> A must trigger cycle detection")
    (assert (is-err? (a/chunk-dag-add d0 c-self)) "Adding self-referential chunk must return error result")
    (let [(na (vl/make-memory-chunk "A" 1000 0.90 "Node A" (list) (list) (list)))
          (d1 (result-or (a/chunk-dag-add d0 na) d0))
          (nb (vl/make-memory-chunk "B" 2000 0.90 "Node B" (list) (list) (list "A")))
          (d2 (result-or (a/chunk-dag-add d1 nb) d1))
          (c-2node (vl/make-memory-chunk "A" 3000 0.90 "2-node loop" (list) (list) (list "B")))]
      (assert (a/detect-cycle? d2 c-2node) "Two-node cycle A -> B -> A must trigger cycle detection")
      (let [(nc (vl/make-memory-chunk "C" 3000 0.90 "Node C" (list) (list) (list "B")))
            (d3 (result-or (a/chunk-dag-add d2 nc) d2))
            (c-indirect (vl/make-memory-chunk "A" 4000 0.90 "indirect loop" (list) (list) (list "C")))
            (c-orphan (vl/make-memory-chunk "O" 5000 0.90 "orphan" (list) (list) (list "unknown-parent")))]
        (assert (a/detect-cycle? d3 c-indirect) "Indirect cycle A -> B -> C -> A must trigger cycle detection")
        (assert (is-err? (a/chunk-dag-add d3 c-orphan)) "Adding chunk with non-existent parent reference must reject with error")
        true))))

(df run-tests [] -> Bool
  :d "Executes comprehensive amnesia unit test suite with 26 strict assertions."
  (and (test-chunk-creation)
       (and (test-chunk-validation)
            (and (test-dag-construction)
                 (and (test-dag-ancestors)
                      (test-cycle-prevention))))))
