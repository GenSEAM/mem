(module asl-mem/simhash
  :doc "Pure ASL 64-bit Locality Sensitive Hashing, feature tokenization, and Hamming distance engine."
  :x [fnv1a-64
      simhash-64
      popcount-64
      hamming-distance
      simhash-similarity]
  :i [])

(df char-to-code [(ch Str)] -> I64
  :doc "Maps single character to deterministic integer code."
  (let [(charset "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ !\"#$%&'()*+,-./:<=>?@[\\]^_`{|}~\t\n\r")]
    (mt (string-index-of charset ch)
      ((none) 127)
      ((some idx) (+ idx 32)))))

(df to-u64 [(x I64)] -> I64
  :doc "Normalizes integer to unsigned 64-bit range."
  (if (< x 0)
      (+ x 18446744073709551616)
      x))

(df bit-xor-loop [(a I64) (b I64) (bit I64) (acc I64)] -> I64
  :doc "Bitwise XOR recursive accumulator loop."
  (if (and (<= a 0) (<= b 0))
      acc
      (let [(bit-a (mod a 2))
            (bit-b (mod b 2))
            (bit-res (if (= bit-a bit-b) 0 1))]
        (bit-xor-loop (/ a 2) (/ b 2) (* bit 2) (+ acc (* bit-res bit))))))

(df bit-xor-64 [(a I64) (b I64)] -> I64
  :doc "Computes bitwise XOR of two 64-bit integers."
  (bit-xor-loop (to-u64 a) (to-u64 b) 1 0))

(df fnv-chars-loop [(chars (List Str)) (h I64)] -> I64
  :doc "Processes characters through FNV-1a 64-bit hash iteration."
  (if (list-empty? chars)
      h
      (let [(ch (option-or (list-head chars) ""))
            (rest (option-or (list-tail chars) (list)))
            (code (char-to-code ch))
            (xor-h (bit-xor-64 h code))
            (next-h (mod (* xor-h 1099511628211) 18446744073709551616))]
        (fnv-chars-loop rest next-h))))

(df fnv1a-64 [(s Str)] -> I64
  :doc "Computes 64-bit Fowler-Noll-Vo 1a hash for string token."
  (fnv-chars-loop (string-chars s) 14695981039346656037))

(df popcount-loop [(val I64) (count I64)] -> I64
  :doc "Counts number of set bits in integer."
  (if (<= val 0)
      count
      (let [(b (mod val 2))
            (next-v (/ val 2))]
        (popcount-loop next-v (+ count b)))))

(df popcount-64 [(x I64)] -> I64
  :doc "Calculates the number of set bits in a 64-bit integer."
  (popcount-loop (to-u64 x) 0))

(df hamming-distance [(h1 I64) (h2 I64)] -> I64
  :doc "Computes Hamming distance between two 64-bit integer fingerprints."
  (popcount-64 (bit-xor-64 h1 h2)))

(df simhash-similarity [(h1 I64) (h2 I64)] -> F64
  :doc "Computes normalized similarity between two SimHash fingerprints in range [0.0, 1.0]."
  (let [(dist (hamming-distance h1 h2))
        (dist-f (int64-to-float64 dist))]
    (- 1.0 (/ dist-f 64.0))))

(df strip-punct [(text Str)] -> Str
  :doc "Replaces punctuation and whitespace delimiters with single spaces."
  (let [(s1 (string-replace text "." " "))
        (s2 (string-replace s1 "," " "))
        (s3 (string-replace s2 ";" " "))
        (s4 (string-replace s3 ":" " "))
        (s5 (string-replace s4 "!" " "))
        (s6 (string-replace s5 "?" " "))
        (s7 (string-replace s6 "\"" " "))
        (s8 (string-replace s7 "'" " "))
        (s9 (string-replace s8 "(" " "))
        (s10 (string-replace s9 ")" " "))
        (s11 (string-replace s10 "[" " "))
        (s12 (string-replace s11 "]" " "))
        (s13 (string-replace s12 "{" " "))
        (s14 (string-replace s13 "}" " "))
        (s15 (string-replace s14 "-" " "))
        (s16 (string-replace s15 "_" " "))
        (s17 (string-replace s16 "/" " "))
        (s18 (string-replace s17 "\\" " "))
        (s19 (string-replace s18 "\n" " "))]
    (string-replace s19 "\t" " ")))

(df tokenize-words [(text Str)] -> (List Str)
  :doc "Splits text into cleaned lowercase tokens."
  (let [(cleaned (strip-punct text))
        (lowered (string-lower cleaned))
        (raw-tokens (string-split lowered " "))
        (trimmed (map (fn [(t Str)] -> Str (string-trim t)) raw-tokens))]
    (filter (fn [(t Str)] -> Bool (> (string-length t) 0)) trimmed)))

(df make-zero-acc-loop [(n I64) (acc (List I64))] -> (List I64)
  :doc "Builds list of n zeros."
  (if (<= n 0)
      acc
      (make-zero-acc-loop (- n 1) (list-cons 0 acc))))

(df make-zero-acc [] -> (List I64)
  :doc "Initializes 64-element projection accumulator vector."
  (make-zero-acc-loop 64 (list)))

(df update-acc-bits [(h I64) (acc (List I64)) (step I64)] -> (List I64)
  :doc "Updates accumulator bits based on token hash bits."
  (if (or (<= step 0) (list-empty? acc))
      (list)
      (let [(bit (mod h 2))
            (weight (if (= bit 1) 1 -1))
            (cur (option-or (list-head acc) 0))
            (tail-acc (option-or (list-tail acc) (list)))
            (next-acc (update-acc-bits (/ h 2) tail-acc (- step 1)))]
        (list-cons (+ cur weight) next-acc))))

(df accumulate-tokens [(tokens (List Str)) (acc (List I64))] -> (List I64)
  :doc "Iterates through tokens and updates projection accumulator."
  (if (list-empty? tokens)
      acc
      (let [(t (option-or (list-head tokens) ""))
            (rest (option-or (list-tail tokens) (list)))
            (h (fnv1a-64 t))
            (next-acc (update-acc-bits h acc 64))]
        (accumulate-tokens rest next-acc))))

(df acc-to-hash-loop [(acc (List I64)) (power I64) (hash I64)] -> I64
  :doc "Thresholds accumulator into 64-bit integer fingerprint."
  (if (list-empty? acc)
      hash
      (let [(val (option-or (list-head acc) 0))
            (rest (option-or (list-tail acc) (list)))
            (bit (if (> val 0) 1 0))]
        (acc-to-hash-loop rest (* power 2) (+ hash (* bit power))))))

(df acc-to-hash [(acc (List I64))] -> I64
  :doc "Converts 64-element accumulator to 64-bit integer."
  (acc-to-hash-loop acc 1 0))

(df simhash-64 [(text Str)] -> I64
  :doc "Computes 64-bit Locality Sensitive Hash fingerprint for given text."
  (let [(tokens (tokenize-words text))]
    (if (list-empty? tokens)
        0
        (let [(init-acc (make-zero-acc))
              (final-acc (accumulate-tokens tokens init-acc))]
          (acc-to-hash final-acc)))))
