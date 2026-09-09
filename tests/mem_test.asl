(module asl-mem/test
  :d "Unit tests for in-memory vector database, graph intelligence, and compact encoding in ASL"
  :x [run-tests
      test-driver-cosine-similarity]
  :i [(store :a s)
      (graph :a g)
      (compact :a c)
      (driver :a d)])

(df test-vector-similarity [] -> Bool
  :d "Verifies cosine similarity calculation and vector normalization."
  (let [(v1 (list 1.0 0.0 0.0))
        (v2 (list 1.0 0.0 0.0))
        (v3 (list 0.0 1.0 0.0))
        (sim1 (s/cosine-similarity v1 v2))
        (sim2 (s/cosine-similarity v1 v3))]
    (assert (> sim1 0.99) "Identical vectors similarity must be > 0.99")
    (assert (< sim2 0.01) "Orthogonal vectors similarity must be < 0.01")
    true))

(df test-driver-cosine-similarity [] -> Bool
  :d "Verifies vector driver cosine similarity against orthogonal, identical, and opposite vectors."
  (let [(v1 (list 1.0 0.0))
        (v2 (list 0.0 1.0))
        (v3 (list -1.0 0.0))]
    (assert (= (d/cosine-similarity v1 v2) 0.0) "Orthogonal vectors must have similarity 0.0")
    (assert (= (d/cosine-similarity v1 v1) 1.0) "Identical vectors must have similarity 1.0")
    (assert (= (d/cosine-similarity v1 v3) -1.0) "Opposite vectors must have similarity -1.0")
    true))

(df test-knowledge-graph [] -> Bool
  :d "Verifies graph construction, edge retrieval, and neighbor queries."
  (let [(n1 (g/GraphNode :id "n1" :label "entity" :content "Concept A" :timestamp-epoch 1700000000 :confidence 0.95))
        (n2 (g/GraphNode :id "n2" :label "entity" :content "Concept B" :timestamp-epoch 1700001000 :confidence 0.90))
        (e1 (g/GraphEdge :source-id "n1" :target-id "n2" :relation "implements" :weight 1.0 :timestamp-epoch 1700001000))
        (empty-graph (g/KnowledgeGraph :nodes (list) :edges (list)))
        (g1 (g/add-node empty-graph n1))
        (g2 (g/add-node g1 n2))
        (g3 (g/add-edge g2 e1))
        (neighbors (g/find-neighbors g3 "n1"))
        (found-node (g/find-node g3 "n2"))]
    (assert (= (list-length neighbors) 1) "Neighbors count must be 1")
    (assert (mt found-node
              ((none) false)
              ((some n) (= (.-id n) "n2"))) "Node n2 must be found")
    true))

(df test-contradiction-resolution [] -> Bool
  :d "Verifies automatic conflict resolution favoring fresher authoritative facts."
  (let [(old-fact (g/GraphNode :id "f1" :label "price" :content "$10" :timestamp-epoch 1000 :confidence 0.85))
        (new-fact (g/GraphNode :id "f2" :label "price" :content "$12" :timestamp-epoch 2000 :confidence 0.95))
        (res (g/resolve-contradiction old-fact new-fact))]
    (assert (.-has-conflict res) "Must report conflict")
    (assert (= (.-winner-id res) "f2") "Fresher fact must win conflict")
    true))

(df test-compact-encoding [] -> Bool
  :d "Verifies ASN frame serialization and ledger compilation."
  (let [(rec (c/encode-vector-frame "vec-1" "query test" (list 0.5 0.25 0.125)))
        (ledger (c/format-compact-ledger (list rec)))]
    (assert (string-contains? (.-payload rec) "v:{vec-1|query test|[0.5,0.25,0.125]}") "Payload must contain vector frame")
    (assert (string-contains? ledger "v:{vec-1") "Ledger must contain vector frame")
    true))

(df test-graph-node-removal [] -> Bool
  :d "Verifies node deletion purges node and all incident relationship edges."
  (let [(n1 (g/GraphNode :id "a" :label "node" :content "A" :timestamp-epoch 1000 :confidence 1.0))
        (n2 (g/GraphNode :id "b" :label "node" :content "B" :timestamp-epoch 1000 :confidence 1.0))
        (e1 (g/GraphEdge :source-id "a" :target-id "b" :relation "links" :weight 1.0 :timestamp-epoch 1000))
        (g0 (g/KnowledgeGraph :nodes (list n1 n2) :edges (list e1)))
        (g-after (g/remove-node g0 "a"))]
    (assert (= (list-length (.-nodes g-after)) 1) "Remaining nodes count must be 1")
    (assert (= (list-length (.-edges g-after)) 0) "Remaining edges count must be 0")
    (assert (= (.-id (option-or (list-head (.-nodes g-after)) n1)) "b") "Remaining node must be b")
    true))

(df test-vector-dimension-check [] -> Bool
  :d "Verifies strict dimension matching on vector store ingestion."
  (let [(store (s/VectorStore :name "dim-test" :dimensions 3 :items (list)))
        (valid-item (s/VectorItem :id "v1" :text "good" :vector (list 1.0 2.0 3.0)))
        (invalid-item (s/VectorItem :id "v2" :text "bad" :vector (list 1.0 2.0)))
        (res-ok (s/safe-insert-item store valid-item))
        (res-bad (s/safe-insert-item store invalid-item))
        (is-ok (mt res-ok ((some _) true) ((none) false)))
        (is-rejected (mt res-bad ((some _) false) ((none) true)))]
    (assert is-ok "Valid item insertion must succeed")
    (assert is-rejected "Mismatched dimension item must be rejected")
    true))

(df test-vector-slab-operations [] -> Bool
  :d "Verifies contiguous vector slab allocation, slot offset, and insertion."
  (let [(slab0 (s/make-vector-slab 3 10))
        (off0 (s/slab-slot-offset 0 3))
        (off2 (s/slab-slot-offset 2 3))
        (res-ok (s/slab-insert slab0 "vec-0" (list 0.1 0.2 0.3)))
        (res-bad (s/slab-insert slab0 "bad" (list 0.1 0.2)))
        (is-ok (mt res-ok ((some s) (and (= (.-count s) 1) (= (list-length (.-data s)) 3))) ((none) false)))
        (is-bad (mt res-bad ((some _) false) ((none) true)))]
    (assert (= off0 0) "Offset 0 must be 0")
    (assert (= off2 6) "Offset 2 must be 6")
    (assert is-ok "Slab insert valid must succeed")
    (assert is-bad "Slab insert invalid must fail")
    true))

(df run-tests [] -> Bool
  :d "Runs all asl-mem unit tests."
  (fold (fn [(acc Bool) (p Bool)] -> Bool (and acc p))
        true
        (list (test-vector-similarity)
              (test-driver-cosine-similarity)
              (test-knowledge-graph)
              (test-contradiction-resolution)
              (test-compact-encoding)
              (test-graph-node-removal)
              (test-vector-dimension-check)
              (test-vector-slab-operations))))
