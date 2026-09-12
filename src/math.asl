(module asl-mem/math
  :d "High-Performance Pure AgentScript FastMath & Vector Primitives"
  :x [sqrt fast-inv-sqrt
      vector-dot vector-norm vector-cosine-sim cosine-similarity
      vector-add vector-scale vector-relu vector-softmax
      fast-ln])

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

(df vector-dot-helper [(a (List F64)) (b (List F64)) (acc F64)] -> F64
  (if (or (list-empty? a) (list-empty? b))
      acc
      (let [(h-a (option-or (list-head a) 0.0))
            (h-b (option-or (list-head b) 0.0))
            (t-a (option-or (list-tail a) (list)))
            (t-b (option-or (list-tail b) (list)))]
        (vector-dot-helper t-a t-b (+ acc (* h-a h-b))))))

(df vector-dot [(a (List F64)) (b (List F64))] -> F64
  (:d "Dot product of two Float64 vectors")
  (vector-dot-helper a b 0.0))

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

(df cosine-similarity [(a (List F64)) (b (List F64))] -> F64
  (:d "Cosine similarity between two Float64 vectors in range [-1.0, 1.0]")
  (vector-cosine-sim a b))

(df vector-add-helper [(a (List F64)) (b (List F64)) (acc (List F64))] -> (List F64)
  (if (or (list-empty? a) (list-empty? b))
      acc
      (let [(h-a (option-or (list-head a) 0.0))
            (h-b (option-or (list-head b) 0.0))
            (t-a (option-or (list-tail a) (list)))
            (t-b (option-or (list-tail b) (list)))]
        (vector-add-helper t-a t-b (list-append acc (list (+ h-a h-b)))))))

(df vector-add [(a (List F64)) (b (List F64))] -> (List F64)
  (:d "Element-wise vector addition")
  (vector-add-helper a b (list)))

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

(df fast-ln [(x F64)] -> F64
  (:d "Pure ASL natural logarithm approximation using hyperbolic series.")
  (if (<= x 0.0)
    -10.0
    (let [(z (/ (- x 1.0) (+ x 1.0)))
          (z2 (* z z))
          (term1 z)
          (term2 (* term1 z2))
          (term3 (* term2 z2))
          (term4 (* term3 z2))]
      (* 2.0 (+ term1 (+ (/ term2 3.0) (+ (/ term3 5.0) (/ term4 7.0))))))))
