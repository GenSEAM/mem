(module asl-mem/spool-search
  :d "Pure ASL In-Memory Okapi BM25 Spool Search Engine for sub-millisecond terminal log navigation."
  :x [SpoolMatch
      spool-index-lines
      spool-query-bm25
      spool-search-index
      format-spool-matches]
  :i [(bm25 :a bm)])

(dfs SpoolMatch
  (:f line-no I64 "1-based line number in spool")
  (:f text Str "Matching line text")
  (:f score F64 "BM25 relevance score"))

(df make-line-doc [(line Str) (idx I64)] -> bm/Bm25Doc
  :d "Converts a spool line and zero-based index into an indexed BM25 document."
  (let [(line-no (+ idx 1))
        (terms (bm/tokenize-terms line))
        (t-len (list-length terms))
        (len (if (= t-len 0) 1 t-len))]
    (bm/Bm25Doc
      :id (string-from-int64 line-no)
      :terms terms
      :length len)))

(df index-lines-loop [(lines (List Str)) (idx I64) (acc (List bm/Bm25Doc))] -> (List bm/Bm25Doc)
  :d "Recursively converts lines into BM25 documents."
  (if (list-empty? lines)
      acc
      (let [(l (option-or (list-head lines) ""))
            (rest (option-or (list-tail lines) (list)))
            (doc (make-line-doc l idx))]
        (index-lines-loop rest (+ idx 1) (list-append acc (list doc))))))

(df spool-index-lines [(lines (List Str))] -> bm/Bm25Index
  :d "Builds an in-memory BM25 index from a list of terminal spool lines."
  (let [(docs (index-lines-loop lines 0 (list)))]
    (bm/make-bm25-index docs)))

(df build-matches-loop [(scores (List bm/Bm25Score)) (lines (List Str)) (acc (List SpoolMatch))] -> (List SpoolMatch)
  :d "Maps BM25 scores back to spool line numbers and text content."
  (if (list-empty? scores)
      acc
      (let [(s (option-or (list-head scores) (bm/Bm25Score :doc-id "" :score 0.0)))
            (rest (option-or (list-tail scores) (list)))
            (score-val (.-score s))]
        (if (<= score-val 0.0)
            (build-matches-loop rest lines acc)
            (let [(line-no (option-or (string-to-int64 (.-doc-id s)) 0))
                  (text (if (> line-no 0)
                            (option-or (list-get lines (- line-no 1)) "")
                            ""))
                  (m (SpoolMatch :line-no line-no :text text :score score-val))]
              (build-matches-loop rest lines (list-append acc (list m))))))))

(df spool-search-index [(index bm/Bm25Index) (lines (List Str)) (query Str) (top-k I64)] -> (List SpoolMatch)
  :d "Queries prebuilt BM25 index over spool lines and returns ranked matches."
  (let [(scores (bm/rank-bm25 index query top-k))]
    (build-matches-loop scores lines (list))))

(df spool-query-bm25 [(raw-text Str) (query Str) (top-k I64)] -> (List SpoolMatch)
  :d "Queries raw spool text directly using in-memory BM25 index and returns top-k matching lines."
  (if (string-empty? (string-trim raw-text))
      (list)
      (let [(norm (string-replace raw-text "\r\n" "\n"))
            (lines (string-split norm "\n"))
            (index (spool-index-lines lines))]
        (spool-search-index index lines query top-k))))

(df format-spool-matches [(matches (List SpoolMatch))] -> Str
  :d "Formats ranked spool matches into a compact high-SNR summary."
  (if (list-empty? matches)
      "Zero matches found."
      (let [(rows (map (fn [(m SpoolMatch)] -> Str
                         (str "  L" (string-from-int64 (.-line-no m)) ": " (.-text m)))
                       matches))]
        (string-join rows "\n"))))
