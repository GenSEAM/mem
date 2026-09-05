(module asl-mem/store
  :d "In-memory vector store: L2 norm, dimension validation, cosine similarity, and contiguous flat memory slab."
  :x [VectorItem VectorStore VectorSlab make-vector-slab slab-slot-offset slab-insert sqrt-approx dot vector-norm cosine-similarity normalize-vector dot-normalized validate-vector-dim safe-insert-item])

(dfs VectorItem
  (:f id Str "Stable identifier for the stored item")
  (:f text Str "Text payload the vector was derived from")
  (:f vector (List F64) "Embedding vector"))

(dfs VectorStore
  (:f name Str "Store name")
  (:f dimensions I64 "Expected vector length")
  (:f items (List VectorItem) "Stored items"))

(df newton-step [(guess F64) (x F64)] -> F64
  :d "One Newton-Raphson iteration towards the square root of x."
  (* 0.5 (+ guess (/ x guess))))

(df sqrt-approx [(x F64)] -> F64
  :d "Square root by Newton-Raphson. The vocabulary has no sqrt, and a fixed
      iteration count keeps this total and identical on every backend."
  (if (<= x 0.0)
    0.0
    (fold (fn [(g F64) (i I64)] -> F64 (newton-step g x))
          x
          (range 0 24))))

(df dot [(a (List F64)) (b (List F64))] -> F64
  :d "Sum of pairwise products, truncating to the shorter vector."
  (list-sum (map (fn [(p (Pair F64 F64))] -> F64 (* (.-first p) (.-second p)))
                 (zip a b))))

(df vector-norm [(v (List F64))] -> F64
  :d "Euclidean L2 norm."
  (sqrt-approx (dot v v)))

(df cosine-similarity [(a (List F64)) (b (List F64))] -> F64
  :d "Cosine of the angle between two vectors; 0.0 when either has no length."
  (let [(denom (* (vector-norm a) (vector-norm b)))]
    (if (= denom 0.0)
      0.0
      (/ (dot a b) denom))))

(df normalize-vector [(v (List F64))] -> (List F64)
  :d "Returns unit-length normalized vector for fast dot product cosine similarity."
  (let [(norm (vector-norm v))]
    (if (= norm 0.0)
      v
      (map (fn [(x F64)] -> F64 (/ x norm)) v))))

(df dot-normalized [(a (List F64)) (b (List F64))] -> F64
  :d "Direct cosine similarity between pre-normalized unit vectors without norm overhead."
  (dot a b))

(df validate-vector-dim [(store VectorStore) (v (List F64))] -> Bool
  :d "Asserts that embedding vector length matches the configured store dimension."
  (= (list-length v) (.-dimensions store)))

(df safe-insert-item [(store VectorStore) (item VectorItem)] -> (Option VectorStore)
  :d "Validates vector dimension before inserting into store, rejecting mismatches."
  (if (validate-vector-dim store (.-vector item))
    (some (VectorStore
            :name (.-name store)
            :dimensions (.-dimensions store)
            :items (list-append (.-items store) (list item))))
    (none)))

(dfs VectorSlab
  (:f dimensions I64 "Vector dimension")
  (:f capacity I64 "Maximum slot capacity")
  (:f count I64 "Number of stored vectors")
  (:f ids (List Str) "Identifier labels")
  (:f data (List F64) "Flat contiguous embedding slab"))

(df make-vector-slab [(dim I64) (cap I64)] -> VectorSlab
  :d "Allocates an empty contiguous vector slab."
  (VectorSlab
    :dimensions dim
    :capacity cap
    :count 0
    :ids (list)
    :data (list)))

(df slab-slot-offset [(slot I64) (dim I64)] -> I64
  :d "Calculates the linear start offset for a vector slot index."
  (* slot dim))

(df slab-insert [(slab VectorSlab) (id Str) (vec (List F64))] -> (Option VectorSlab)
  :d "Appends a vector to the contiguous flat slab if within capacity and matching dimension."
  (if (and (= (list-length vec) (.-dimensions slab))
           (< (.-count slab) (.-capacity slab)))
    (some (VectorSlab
            :dimensions (.-dimensions slab)
            :capacity (.-capacity slab)
            :count (+ (.-count slab) 1)
            :ids (list-append (.-ids slab) (list id))
            :data (list-append (.-data slab) vec)))
    (none)))
