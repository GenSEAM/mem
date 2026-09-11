(module asl-mem/xref
  :d "Universal In-Memory Dependency and Provenance Graph connecting Tasks, Code Symbols, ADRs, Invariants, and Memory Chunks"
  :x [XRefEdge
      XRefGraph
      make-xref-graph
      list-add-unique
      xref-add-edge
      find-dependents
      find-prerequisites
      find-provenance
      xref-forward-edges
      xref-backward-edges
      xref-has-node?]
  :i [])

(dfs XRefEdge
  (:f source Str "Origin entity URI identifier")
  (:f target Str "Target entity URI identifier")
  (:f relation Str "Relationship type: depends-on, prerequisite, provenance, references"))

(dfs XRefGraph
  (:f nodes (List Str) "List of unique entity URI identifiers in the graph")
  (:f edges (List XRefEdge) "List of all directed relationship edges")
  (:f forward (Map Str (List Str)) "Forward adjacency mapping from source to target identifiers")
  (:f backward (Map Str (List Str)) "Reverse adjacency mapping from target to source identifiers")
  (:f provenance-map (Map Str (List Str)) "Direct provenance mapping from entity to originating justification URIs")
  (:f prereq-map (Map Str (List Str)) "Prerequisite mapping from entity to required dependency URIs")
  (:f dep-map (Map Str (List Str)) "Dependent mapping from entity to downstream consumer URIs"))

(df make-xref-graph [] -> XRefGraph
  :d "Constructs an empty in-memory universal cross-reference and dependency graph"
  (XRefGraph
    :nodes (list)
    :edges (list)
    :forward (map-empty)
    :backward (map-empty)
    :provenance-map (map-empty)
    :prereq-map (map-empty)
    :dep-map (map-empty)))

(df list-add-unique [(lst (List Str)) (item Str)] -> (List Str)
  :d "Appends item to list if not already present, preserving uniqueness"
  (if (list-contains? lst item)
      lst
      (list-append lst (list item))))

(df xref-has-node? [(g XRefGraph) (id Str)] -> Bool
  :d "Checks if a given URI node exists in the graph"
  (list-contains? (.-nodes g) id))

(df xref-forward-edges [(g XRefGraph) (node-id Str)] -> (List Str)
  :d "Returns list of direct outgoing target URIs from node-id"
  (option-or (map-get (.-forward g) node-id) (list)))

(df xref-backward-edges [(g XRefGraph) (node-id Str)] -> (List Str)
  :d "Returns list of direct incoming source URIs pointing to node-id"
  (option-or (map-get (.-backward g) node-id) (list)))

(df xref-add-edge [(g XRefGraph) (source Str) (target Str) (relation Str)] -> XRefGraph
  :d "Adds a directed relationship edge between two entity URIs with O(1) bipartite indexing"
  (let [(edge (XRefEdge :source source :target target :relation relation))
        (n1 (list-add-unique (.-nodes g) source))
        (new-nodes (list-add-unique n1 target))
        (new-edges (list-append (.-edges g) (list edge)))
        (fwd-list (option-or (map-get (.-forward g) source) (list)))
        (new-fwd (map-set (.-forward g) source (list-add-unique fwd-list target)))
        (bwd-list (option-or (map-get (.-backward g) target) (list)))
        (new-bwd (map-set (.-backward g) target (list-add-unique bwd-list source)))
        (is-dep (= relation "depends-on"))
        (is-prereq (or (= relation "prerequisite")
                       (= relation "requires")))
        (is-prov (or (= relation "provenance")
                     (or (= relation "derives-from")
                         (or (= relation "justified-by")
                             (and (or (= relation "ref") (= relation "references"))
                                  (or (string-starts-with? target "adr:")
                                      (or (string-starts-with? target "d:")
                                          (or (string-starts-with? target "c:")
                                              (string-starts-with? target "task:")))))))))
        (p-list (option-or (map-get (.-prereq-map g) source) (list)))
        (new-prereq (if is-prereq
                        (map-set (.-prereq-map g) source (list-add-unique p-list target))
                        (.-prereq-map g)))
        (d-list (option-or (map-get (.-dep-map g) target) (list)))
        (new-dep (if is-dep
                     (map-set (.-dep-map g) target (list-add-unique d-list source))
                     (.-dep-map g)))
        (prov-list (option-or (map-get (.-provenance-map g) source) (list)))
        (new-prov (if is-prov
                      (map-set (.-provenance-map g) source (list-add-unique prov-list target))
                      (.-provenance-map g)))]
    (XRefGraph
      :nodes new-nodes
      :edges new-edges
      :forward new-fwd
      :backward new-bwd
      :provenance-map new-prov
      :prereq-map new-prereq
      :dep-map new-dep)))

(df find-dependents [(g XRefGraph) (node-id Str)] -> (List Str)
  :d "Queries forward dependents that rely on the given entity URI"
  (option-or (map-get (.-dep-map g) node-id) (list)))

(df find-prerequisites [(g XRefGraph) (node-id Str)] -> (List Str)
  :d "Queries prerequisite dependency URIs required by the given entity URI"
  (option-or (map-get (.-prereq-map g) node-id) (list)))

(df find-provenance [(g XRefGraph) (node-id Str)] -> (List Str)
  :d "Queries provenance justification URIs (Tasks, ADRs, Invariants) for the given entity URI"
  (option-or (map-get (.-provenance-map g) node-id) (list)))
