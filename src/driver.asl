(module asl-mem/driver
  :d "Pure AgentScript memory matrix driver and vector store adapter."
  :x [VectorRecord
      create-record
      cosine-similarity]
  :i [(math :a m)])

(dfs VectorRecord
  (:f id Str "Unique memory entry ID")
  (:f content Str "Indexed semantic text")
  (:f embedding (List F64) "Dense vector embedding"))

(df create-record [(id Str) (text Str) (emb (List F64))] -> VectorRecord
  :d "Constructs vector record instance."
  (VectorRecord :id id :content text :embedding emb))

(df cosine-similarity [(v1 (List F64)) (v2 (List F64))] -> F64
  :d "Computes cosine similarity between two vectors via Euclidean L2 normalization."
  (m/vector-cosine-sim v1 v2))
