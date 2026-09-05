(module asl-mem/graph
  :d "High-density entity knowledge graph, edge traversal, and contradiction resolution in ASL Nano."
  :x [GraphNode GraphEdge KnowledgeGraph ContradictionResult KnowledgeGraphIndex
      find-node find-outgoing-edges find-neighbors is-fresh? resolve-contradiction
      add-node add-edge add-nodes-batch add-edges-batch remove-node
      build-graph-index find-node-fast find-neighbors-fast])

(dfs GraphNode
  (:f id Str "Unique entity node identifier")
  (:f label Str "Entity category or class name")
  (:f content Str "Entity description or factual assertion")
  (:f timestamp-epoch I64 "Creation or verification unix epoch timestamp")
  (:f confidence F64 "Fact confidence score between 0.0 and 1.0"))

(dfs GraphEdge
  (:f source-id Str "Origin node identifier")
  (:f target-id Str "Destination node identifier")
  (:f relation Str "Relationship label (e.g. depends_on, contradicts, derives_from)")
  (:f weight F64 "Connection relevance weight")
  (:f timestamp-epoch I64 "Relationship discovery unix epoch timestamp"))

(dfs KnowledgeGraph
  (:f nodes (List GraphNode) "Entity vertices")
  (:f edges (List GraphEdge) "Directed relationship edges"))

(dfs ContradictionResult
  (:f has-conflict Bool "True if contradictory claims exist between nodes")
  (:f winner-id Str "Identifier of the authoritative node after resolution")
  (:f explanation Str "Rationale describing resolution based on freshness and confidence"))

(df find-node [(graph KnowledgeGraph) (node-id Str)] -> (Option GraphNode)
  :d "Looks up a node by its identifier in the graph."
  (let [(matches (filter (fn [(n GraphNode)] -> Bool (= (.-id n) node-id)) (.-nodes graph)))]
    (list-head matches)))

(df find-outgoing-edges [(graph KnowledgeGraph) (source-id Str)] -> (List GraphEdge)
  :d "Finds all outgoing relation edges from a source entity."
  (filter (fn [(e GraphEdge)] -> Bool (= (.-source-id e) source-id)) (.-edges graph)))

(df find-neighbors [(graph KnowledgeGraph) (node-id Str)] -> (List Str)
  :d "Finds target neighbor node IDs reachable from the source."
  (map (fn [(e GraphEdge)] -> Str (.-target-id e)) (find-outgoing-edges graph node-id)))

(df is-fresh? [(node GraphNode) (current-epoch I64) (max-age-sec I64)] -> Bool
  :d "Evaluates whether an entity fact is within the acceptable freshness deadline."
  (< (- current-epoch (.-timestamp-epoch node)) max-age-sec))

(df resolve-contradiction [(fact-a GraphNode) (fact-b GraphNode)] -> ContradictionResult
  :d "Resolves conflicting facts by balancing timestamp recency and confidence scores."
  (let [(time-diff (- (.-timestamp-epoch fact-b) (.-timestamp-epoch fact-a)))
        (conf-diff (- (.-confidence fact-b) (.-confidence fact-a)))]
    (cond
      ((and (> time-diff 0) (>= conf-diff -0.1))
       (ContradictionResult
         :has-conflict true
         :winner-id (.-id fact-b)
         :explanation "Fact B accepted: newer timestamp with authoritative confidence"))
      ((and (< time-diff 0) (<= conf-diff 0.1))
       (ContradictionResult
         :has-conflict true
         :winner-id (.-id fact-a)
         :explanation "Fact A accepted: newer timestamp with authoritative confidence"))
      ((> (.-confidence fact-b) (.-confidence fact-a))
       (ContradictionResult
         :has-conflict true
         :winner-id (.-id fact-b)
         :explanation "Fact B accepted: significantly higher confidence score"))
      (:else
       (ContradictionResult
         :has-conflict true
         :winner-id (.-id fact-a)
         :explanation "Fact A retained: authoritative default")))))

(df add-node [(graph KnowledgeGraph) (node GraphNode)] -> KnowledgeGraph
  :d "Appends a new entity vertex to the graph."
  (KnowledgeGraph
    :nodes (list-append (.-nodes graph) (list node))
    :edges (.-edges graph)))

(df add-edge [(graph KnowledgeGraph) (edge GraphEdge)] -> KnowledgeGraph
  :d "Appends a new relationship edge to the graph."
  (KnowledgeGraph
    :nodes (.-nodes graph)
    :edges (list-append (.-edges graph) (list edge))))

(df add-nodes-batch [(graph KnowledgeGraph) (new-nodes (List GraphNode))] -> KnowledgeGraph
  :d "Batched insertion of entity vertices in a single linear pass."
  (KnowledgeGraph
    :nodes (list-append (.-nodes graph) new-nodes)
    :edges (.-edges graph)))

(df add-edges-batch [(graph KnowledgeGraph) (new-edges (List GraphEdge))] -> KnowledgeGraph
  :d "Batched insertion of directed edges in a single linear pass."
  (KnowledgeGraph
    :nodes (.-nodes graph)
    :edges (list-append (.-edges graph) new-edges)))

(dfs KnowledgeGraphIndex
  (:f node-map (Map Str GraphNode) "O(1) hash map of entity ID to GraphNode")
  (:f adjacency (Map Str (List Str)) "O(1) adjacency list mapping source ID to target IDs"))

(df build-graph-index [(graph KnowledgeGraph)] -> KnowledgeGraphIndex
  :d "Constructs fast O(1) hash index and adjacency tables from a KnowledgeGraph."
  (let [(n-map (fold (fn [(acc (Map Str GraphNode)) (n GraphNode)] -> (Map Str GraphNode)
                       (map-set acc (.-id n) n))
                     (map-empty)
                     (.-nodes graph)))
        (adj (fold (fn [(acc (Map Str (List Str))) (e GraphEdge)] -> (Map Str (List Str))
                     (let [(src (.-source-id e))
                           (tgt (.-target-id e))
                           (existing (option-or (map-get acc src) (list)))]
                       (map-set acc src (list-append existing (list tgt)))))
                   (map-empty)
                   (.-edges graph)))]
    (KnowledgeGraphIndex :node-map n-map :adjacency adj)))

(df find-node-fast [(idx KnowledgeGraphIndex) (node-id Str)] -> (Option GraphNode)
  :d "O(1) lookup of a node by identifier."
  (map-get (.-node-map idx) node-id))

(df find-neighbors-fast [(idx KnowledgeGraphIndex) (node-id Str)] -> (List Str)
  :d "O(1) lookup of outgoing neighbor node IDs."
  (option-or (map-get (.-adjacency idx) node-id) (list)))

(df remove-node [(graph KnowledgeGraph) (node-id Str)] -> KnowledgeGraph
  :d "Removes an entity vertex and purges all connected incident edges to prevent orphaned relations."
  (let [(remaining-nodes (filter (fn [(n GraphNode)] -> Bool (not (= (.-id n) node-id))) (.-nodes graph)))
        (remaining-edges (filter (fn [(e GraphEdge)] -> Bool
                                   (and (not (= (.-source-id e) node-id))
                                        (not (= (.-target-id e) node-id))))
                                 (.-edges graph)))]
    (KnowledgeGraph
      :nodes remaining-nodes
      :edges remaining-edges)))

