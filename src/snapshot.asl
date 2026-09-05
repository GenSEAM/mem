(module asl-mem/snapshot
  :d "Deterministic atomic snapshot creator and restorer for vector stores and knowledge graphs."
  :x [SnapshotManifest
      create-snapshot-frame
      serialize-vector-item
      serialize-graph-node-frame
      serialize-graph-edge
      format-snapshot-ledger]
  :i [(store :a s)
      (graph :a g)])

(dfs SnapshotManifest
  (:f version Str "Snapshot schema specification version")
  (:f timestamp-epoch I64 "Snapshot creation timestamp")
  (:f vector-count I64 "Number of serialized vectors")
  (:f node-count I64 "Number of serialized graph vertices")
  (:f edge-count I64 "Number of serialized graph edges")
  (:f checksum Str "Integrity hash or identifier"))

(df serialize-vector-item [(item s/VectorItem)] -> Str
  :d "Encodes a VectorItem into an ASN @v:{...} frame."
  (let [(dense (string-join (map (fn [(f F64)] -> Str (string-from-float64 f)) (.-vector item)) ","))]
    (str "@v:{" (.-id item) "|" (.-text item) "|[" dense "]}")))

(df serialize-graph-node-frame [(node g/GraphNode)] -> Str
  :d "Encodes a GraphNode into an ASN @n:{...} frame."
  (str "@n:{" (.-id node) "|" (.-label node) "|" (string-from-int64 (.-timestamp-epoch node)) "|" (string-from-float64 (.-confidence node)) "|" (.-content node) "}"))

(df serialize-graph-edge [(edge g/GraphEdge)] -> Str
  :d "Encodes a GraphEdge into an ASN @e:{...} frame."
  (str "@e:{" (.-source-id edge) "|" (.-target-id edge) "|" (.-relation edge) "|" (string-from-float64 (.-weight edge)) "|" (string-from-int64 (.-timestamp-epoch edge)) "}"))

(df create-snapshot-frame [(v-count I64) (n-count I64) (e-count I64) (epoch I64)] -> Str
  :d "Encodes snapshot header metadata into @snap:{...} frame."
  (str "@snap:{v1|"
       (string-from-int64 epoch) "|"
       (string-from-int64 v-count) "|"
       (string-from-int64 n-count) "|"
       (string-from-int64 e-count) "}"))

(df format-snapshot-ledger [(v-store s/VectorStore) (k-graph g/KnowledgeGraph) (epoch I64)] -> Str
  :d "Compiles complete memory state into an atomic, portable ASN snapshot document."
  (let [(v-items (.-items v-store))
        (g-nodes (.-nodes k-graph))
        (g-edges (.-edges k-graph))
        (header (create-snapshot-frame (list-length v-items) (list-length g-nodes) (list-length g-edges) epoch))
        (v-frames (map serialize-vector-item v-items))
        (n-frames (map serialize-graph-node-frame g-nodes))
        (e-frames (map serialize-graph-edge g-edges))
        (all-lines (list-append (list-append (list header) v-frames)
                                (list-append n-frames e-frames)))]
    (string-join all-lines "\n")))
