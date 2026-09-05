(module asl-mem/math
  :d "High-Performance Pure AgentScript FastMath & Vector Primitives"
  :x [sqrt fast-inv-sqrt
      vector-dot vector-norm vector-cosine-sim
      vector-add vector-scale vector-relu vector-softmax])

(df sqrt [(x F64)] -> F64
  (:d "Newton-Raphson approximation for square root with fixed 10-step convergence")
  (if (<= x 0.0)
      0.0
      (let [(g0 (/ x 2.0))
            (g1 (/ (+ g0 (/ x g0)) 2.0))
            (g2 (/ (+ g1 (/ x g1)) 2.0))
            (g3 (/ (+ g2 (/ x g2)) 2.0))
            (g4 (/ (+ g3 (/ x g3)) 2.0))
            (g5 (/ (+ g4 (/ x g4)) 2.0))
            (g6 (/ (+ g5 (/ x g5)) 2.0))
            (g7 (/ (+ g6 (/ x g6)) 2.0))
            (g8 (/ (+ g7 (/ x g7)) 2.0))
            (g9 (/ (+ g8 (/ x g8)) 2.0))]
        (/ (+ g9 (/ x g9)) 2.0))))

(df fast-inv-sqrt [(x F64)] -> F64
  (:d "Reciprocal square root (1 / sqrt(x))")
  (if (<= x 0.0)
      0.0
      (/ 1.0 (sqrt x))))

(df vector-dot [(a (List F64)) (b (List F64))] -> F64
  (:d "Dot product of two Float64 vectors")
  (let [(pairs (list-zip a b))]
    (fold (fn [(acc F64) (p (Pair F64 F64))]
            (+ acc (* (fst p) (snd p))))
          0.0
          pairs)))

(df vector-norm [(v (List F64))] -> F64
  (:d "Euclidean L2 norm of a vector")
  (sqrt (vector-dot v v)))

(df vector-cosine-sim [(a (List F64)) (b (List F64))] -> F64
  (:d "Cosine similarity between two Float64 vectors in range [-1.0, 1.0]")
  (let [(norm-a (vector-norm a))
        (norm-b (vector-norm b))]
    (if (or (<= norm-a 0.0) (<= norm-b 0.0))
        0.0
        (/ (vector-dot a b) (* norm-a norm-b)))))

(df vector-add [(a (List F64)) (b (List F64))] -> (List F64)
  (:d "Element-wise vector addition")
  (list-map (fn [(p (Pair F64 F64))] (+ (fst p) (snd p))) (list-zip a b)))

(df vector-scale [(v (List F64)) (s F64)] -> (List F64)
  (:d "Scale vector by scalar factor")
  (list-map (fn [(x F64)] (* x s)) v))

(df vector-relu [(v (List F64))] -> (List F64)
  (:d "Element-wise Rectified Linear Unit activation")
  (list-map (fn [(x F64)] (if (> x 0.0) x 0.0)) v))

(df vector-softmax [(v (List F64))] -> (List F64)
  (:d "Softmax normalization (exp(x_i) / sum(exp(x_j)))")
  (let [(exps (list-map (fn [(x F64)]
                          (let [(exp-approx (+ 1.0 (+ x (/ (* x x) 2.0))))]
                            (if (> exp-approx 0.0) exp-approx 0.0001)))
                        v))
        (total (fold (fn [(acc F64) (x F64)] (+ acc x)) 0.0 exps))]
    (if (<= total 0.0)
        v
        (list-map (fn [(x F64)] (/ x total)) exps))))
