(module asl-mem/summary-search
  :d "Fast keyword token indexing and format-aware summary search in pure ASL."
  :x [SummaryIndex
      summary-index-empty
      index-chunk-summary
      search-chunks-by-summary]
  :i [(view_layer :a vl)
      (simhash :a sh)])

(dfs SummaryIndex
  (:f entries (List (Pair Str (List Str))) "List of indexed chunk ID and keyword token list pairs")
  (:f chunks (Map Str vl/MemoryChunk) "Mapping from chunk ID to canonical MemoryChunk record")
  (:f size I64 "Total number of indexed memory chunks"))

(df summary-index-empty [] -> SummaryIndex
  :d "Constructs an initial empty summary search index."
  (SummaryIndex
    :entries (list)
    :chunks (map-empty)
    :size 0))

(df render-card [(chunk vl/MemoryChunk)] -> Str
  :d "Renders compact dashboard summary card format."
  (str "[Chunk: " (.-id chunk) "] ("
       (string-from-float64 (.-conf chunk)) ") "
       (.-summary chunk) " ("
       (string-from-int64 (.-ts chunk)) " ms)"))

(df index-chunk-summary [(idx SummaryIndex) (chunk vl/MemoryChunk)] -> SummaryIndex
  :d "Indexes a memory chunk by tokenizing its summary and recording in chunk map."
  (let [(c-id (.-id chunk))
        (tokens (sh/tokenize-words (.-summary chunk)))
        (entry (pair c-id tokens))
        (new-entries (list-append (.-entries idx) (list entry)))
        (new-chunks (map-set (.-chunks idx) c-id chunk))
        (new-size (+ (.-size idx) 1))]
    (SummaryIndex
      :entries new-entries
      :chunks new-chunks
      :size new-size)))

(df match-any-token [(q-tokens (List Str)) (doc-tokens (List Str))] -> Bool
  :d "Checks if at least one query token matches a document token."
  (mt (list-head q-tokens)
    ((none) false)
    ((some head)
     (let [(tail (option-or (list-tail q-tokens) (list)))]
       (if (list-contains? doc-tokens head)
           true
           (match-any-token tail doc-tokens))))))

(df search-entries-step [(entries (List (Pair Str (List Str)))) (q-tokens (List Str)) (chunks (Map Str vl/MemoryChunk)) (format Str)] -> (List Str)
  :d "Recursively scans entries collecting formatted results for matching chunks."
  (mt (list-head entries)
    ((none) (list))
    ((some head-entry)
     (let [(tail-entries (option-or (list-tail entries) (list)))
           (c-id (.-first head-entry))
           (doc-tokens (.-second head-entry))
           (rest (search-entries-step tail-entries q-tokens chunks format))]
       (if (match-any-token q-tokens doc-tokens)
           (mt (map-get chunks c-id)
             ((none) rest)
             ((some chunk)
              (let [(formatted (if (= format "markdown")
                                   (render-card chunk)
                                   c-id))]
                (list-cons formatted rest))))
           rest)))))

(df search-chunks-by-summary [(idx SummaryIndex) (query Str) (format Str)] -> (List Str)
  :d "Searches indexed chunks by summary keywords returning matching chunk IDs or formatted Markdown."
  (let [(clean-query (string-trim query))]
    (if (or (string-empty? clean-query) (= (.-size idx) 0))
        (list)
        (let [(q-tokens (sh/tokenize-words clean-query))]
          (if (list-empty? q-tokens)
              (list)
              (search-entries-step (.-entries idx) q-tokens (.-chunks idx) format))))))
