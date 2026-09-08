(module asl-mem/vfs
  :d "In-memory virtual file system and CAS buffer staging engine"
  :x [VFSBuffer
      VFSRegistry
      vfs-init
      vfs-read
      vfs-write
      vfs-cas-hash
      vfs-diff
      normalize-path]
  :i [])

(dfs VFSBuffer
  (:f path Str "Normalized virtual file path")
  (:f content Str "Buffer text payload")
  (:f base-content Str "Pristine or staged base content for diff computation")
  (:f cas-hash Str "Deterministic content-addressed hash of current content")
  (:f base-hash Str "CAS hash of pristine or staged base version")
  (:f revision I64 "Monotonic revision counter")
  (:f dirty Bool "Boolean flag indicating uncommitted edits relative to base")
  (:f loaded-at I64 "Millisecond timestamp when buffer was opened or created"))

(dfs VFSRegistry
  (:f buffers (Map Str VFSBuffer) "Mapping from canonical virtual path to VFSBuffer")
  (:f active-paths (List Str) "Ordered list of open buffer paths")
  (:f size I64 "Total buffer count"))

(df hex-nibble [(n I64)] -> Str
  :d "Converts integer in 0..15 to hex character."
  (let [(chars "0123456789abcdef")]
    (option-or (string-slice chars n (+ n 1)) "0")))

(df hex-u32 [(val I64)] -> Str
  :d "Formats 32-bit positive integer as 8-character lowercase hexadecimal string."
  (let [(d7 (hex-nibble (mod val 16)))
        (v6 (/ val 16))
        (d6 (hex-nibble (mod v6 16)))
        (v5 (/ v6 16))
        (d5 (hex-nibble (mod v5 16)))
        (v4 (/ v5 16))
        (d4 (hex-nibble (mod v4 16)))
        (v3 (/ v4 16))
        (d3 (hex-nibble (mod v3 16)))
        (v2 (/ v3 16))
        (d2 (hex-nibble (mod v2 16)))
        (v1 (/ v2 16))
        (d1 (hex-nibble (mod v1 16)))
        (v0 (/ v1 16))
        (d0 (hex-nibble (mod v0 16)))]
    (str d0 d1 d2 d3 d4 d5 d6 d7)))

(df char-to-code [(ch Str)] -> I64
  :d "Maps single character to deterministic integer code."
  (let [(charset "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ !\"#$%&'()*+,-./:<=>?@[\\]^_`{|}~\t\n\r")]
    (mt (string-index-of charset ch)
      ((none) 127)
      ((some idx) (+ idx 32)))))

(df vfs-cas-hash [(content Str)] -> Str
  :d "Computes deterministic content-addressed SHA256-like 64-character hex hash of buffer text payload."
  (let [(init-state (list 1779033703 996650630 1013904242 625997115 1359893119 453339277 528734635 1541459225))
        (chars (string-chars content))
        (final-state (fold (fn [(acc (List I64)) (ch Str)] -> (List I64)
                             (let [(code (char-to-code ch))
                                   (h0 (option-or (list-get acc 0) 0))
                                   (h1 (option-or (list-get acc 1) 0))
                                   (h2 (option-or (list-get acc 2) 0))
                                   (h3 (option-or (list-get acc 3) 0))
                                   (h4 (option-or (list-get acc 4) 0))
                                   (h5 (option-or (list-get acc 5) 0))
                                   (h6 (option-or (list-get acc 6) 0))
                                   (h7 (option-or (list-get acc 7) 0))
                                   (n0 (mod (+ (* h0 31) code) 2147483647))
                                   (n1 (mod (+ (* h1 37) code) 2147483629))
                                   (n2 (mod (+ (* h2 41) code) 2147483587))
                                   (n3 (mod (+ (* h3 43) code) 2147483579))
                                   (n4 (mod (+ (* h4 47) code) 2147483563))
                                   (n5 (mod (+ (* h5 53) code) 2147483543))
                                   (n6 (mod (+ (* h6 59) code) 2147483497))
                                   (n7 (mod (+ (* h7 61) code) 2147483489))]
                               (list n0 n1 n2 n3 n4 n5 n6 n7)))
                           init-state
                           chars))
        (len (string-length content))
        (f0 (mod (+ (* (option-or (list-get final-state 0) 0) 67) len) 2147483647))
        (f1 (mod (+ (* (option-or (list-get final-state 1) 0) 71) len) 2147483629))
        (f2 (mod (+ (* (option-or (list-get final-state 2) 0) 73) len) 2147483587))
        (f3 (mod (+ (* (option-or (list-get final-state 3) 0) 79) len) 2147483579))
        (f4 (mod (+ (* (option-or (list-get final-state 4) 0) 83) len) 2147483563))
        (f5 (mod (+ (* (option-or (list-get final-state 5) 0) 89) len) 2147483543))
        (f6 (mod (+ (* (option-or (list-get final-state 6) 0) 97) len) 2147483497))
        (f7 (mod (+ (* (option-or (list-get final-state 7) 0) 101) len) 2147483489))]
    (str (hex-u32 f0)
         (hex-u32 f1)
         (hex-u32 f2)
         (hex-u32 f3)
         (hex-u32 f4)
         (hex-u32 f5)
         (hex-u32 f6)
         (hex-u32 f7))))

(df normalize-path [(path Str)] -> Str
  :d "Normalizes virtual file path by trimming whitespace and stripping leading dot-slash or slash prefixes."
  (let [(trimmed (string-trim path))]
    (if (string-starts-with? trimmed "./")
      (option-or (string-slice trimmed 2 (string-length trimmed)) "")
      (if (string-starts-with? trimmed "/")
        (option-or (string-slice trimmed 1 (string-length trimmed)) "")
        trimmed))))

(df vfs-init [] -> VFSRegistry
  :d "Constructs an initial empty virtual file system registry."
  (VFSRegistry
    :buffers (map-empty)
    :active-paths (list)
    :size 0))

(df vfs-read [(registry VFSRegistry) (path Str)] -> (Option VFSBuffer)
  :d "Retrieves buffer record by normalized virtual path."
  (let [(norm (normalize-path path))]
    (map-get (.-buffers registry) norm)))

(df vfs-write [(registry VFSRegistry) (path Str) (new-content Str)] -> VFSRegistry
  :d "Stages new content into buffer, updates revision, marks dirty if content differs from base, and recalculates CAS hash."
  (let [(norm (normalize-path path))
        (new-hash (vfs-cas-hash new-content))]
    (mt (map-get (.-buffers registry) norm)
      ((none)
       (let [(new-buf (VFSBuffer
                        :path norm
                        :content new-content
                        :base-content new-content
                        :cas-hash new-hash
                        :base-hash new-hash
                        :revision 1
                        :dirty false
                        :loaded-at 0))]
         (VFSRegistry
           :buffers (map-set (.-buffers registry) norm new-buf)
           :active-paths (list-append (.-active-paths registry) (list norm))
           :size (+ (.-size registry) 1))))
      ((some prev)
       (let [(base-h (.-base-hash prev))
             (base-c (.-base-content prev))
             (is-dirty (!= new-hash base-h))
             (updated-buf (VFSBuffer
                            :path norm
                            :content new-content
                            :base-content base-c
                            :cas-hash new-hash
                            :base-hash base-h
                            :revision (+ (.-revision prev) 1)
                            :dirty is-dirty
                            :loaded-at (.-loaded-at prev)))]
         (VFSRegistry
           :buffers (map-set (.-buffers registry) norm updated-buf)
           :active-paths (.-active-paths registry)
           :size (.-size registry)))))))

(df split-lines [(text Str)] -> (List Str)
  :d "Splits text by newlines, returning empty list if text is empty."
  (if (string-empty? text)
    (list)
    (string-split text "\n")))

(df diff-lines-loop [(base (List Str)) (cur (List Str)) (i I64) (j I64) (acc (List Str))] -> (List Str)
  :d "Internal recursive diff engine matching lines and emitting unified diff entries."
  (let [(n (list-length base))
        (m (list-length cur))]
    (if (and (>= i n) (>= j m))
      acc
      (if (>= i n)
        (let [(line-c (option-or (list-get cur j) ""))
              (next-acc (list-append acc (list (str "+" line-c))))]
          (diff-lines-loop base cur i (+ j 1) next-acc))
        (if (>= j m)
          (let [(line-b (option-or (list-get base i) ""))
                (next-acc (list-append acc (list (str "-" line-b))))]
            (diff-lines-loop base cur (+ i 1) j next-acc))
          (let [(line-b (option-or (list-get base i) ""))
                (line-c (option-or (list-get cur j) ""))]
            (if (= line-b line-c)
              (let [(next-acc (list-append acc (list (str " " line-b))))]
                (diff-lines-loop base cur (+ i 1) (+ j 1) next-acc))
              (let [(sub-cur (option-or (list-slice cur j m) (list)))
                    (sub-base (option-or (list-slice base i n) (list)))]
                (if (list-contains? sub-cur line-b)
                  (let [(next-acc (list-append acc (list (str "+" line-c))))]
                    (diff-lines-loop base cur i (+ j 1) next-acc))
                  (if (list-contains? sub-base line-c)
                    (let [(next-acc (list-append acc (list (str "-" line-b))))]
                      (diff-lines-loop base cur (+ i 1) j next-acc))
                    (let [(next-acc (list-append acc (list (str "-" line-b) (str "+" line-c))))]
                      (diff-lines-loop base cur (+ i 1) (+ j 1) next-acc))))))))))))

(df vfs-diff [(buf VFSBuffer)] -> (List Str)
  :d "Computes line-by-line unified diff between base content and current content."
  (if (not (.-dirty buf))
    (list)
    (let [(base-lines (split-lines (.-base-content buf)))
          (cur-lines (split-lines (.-content buf)))]
      (diff-lines-loop base-lines cur-lines 0 0 (list)))))
