(module asl-mem/view-layer
  :d "Asymmetric human hydration and Markdown projection of MemoryChunk DAG nodes in pure ASL."
  :x [MemoryChunk
      make-memory-chunk
      hydrate-to-markdown
      render-summary-card]
  :i [])

(dfs MemoryChunk
  (:f id Str "Canonical memory chunk record")
  (:f ts I64 "Creation unix epoch timestamp in milliseconds")
  (:f conf F64 "Chunk factual confidence score between 0.0 and 1.0")
  (:f summary Str "Human and agent readable summary")
  (:f facts (List Str) "Factual scalar assertion list")
  (:f directives (List Str) "Operational constraints and behavioral directives")
  (:f refs (List Str) "Causal parent chunk identifiers in DAG"))

(df make-memory-chunk [(id Str) (ts I64) (conf F64) (summary Str) (facts (List Str)) (directives (List Str)) (refs (List Str))] -> MemoryChunk
  :d "Constructs a MemoryChunk record with timestamp confidence and causal reference links."
  (MemoryChunk
    :id id
    :ts ts
    :conf conf
    :summary summary
    :facts facts
    :directives directives
    :refs refs))

(df format-facts-rows [(facts (List Str)) (idx I64)] -> Str
  :d "Formats scalar fact list into markdown table row string."
  (if (list-empty? facts)
      ""
      (let [(head (option-or (list-head facts) ""))
            (tail (option-or (list-tail facts) (list)))
            (row (str "| " (string-from-int64 idx) " | " head " |\n"))]
        (str row (format-facts-rows tail (+ idx 1))))))

(df format-facts-table [(facts (List Str))] -> Str
  :d "Renders markdown table of facts or empty placeholder."
  (if (list-empty? facts)
      "*No scalar facts recorded.*"
      (str "| Index | Fact |\n|---|---|\n" (format-facts-rows facts 1))))

(df format-directives-rows [(directives (List Str))] -> Str
  :d "Formats operational directive list into bulleted alert lines."
  (if (list-empty? directives)
      ""
      (let [(head (option-or (list-head directives) ""))
            (tail (option-or (list-tail directives) (list)))
            (line (str "> - " head "\n"))]
        (str line (format-directives-rows tail)))))

(df format-directives [(directives (List Str))] -> Str
  :d "Renders markdown alert box for operational directives or empty placeholder."
  (if (list-empty? directives)
      "*No operational directives.*"
      (str "> [!IMPORTANT]\n> **Directives**:\n" (format-directives-rows directives))))

(df format-refs-rows [(refs (List Str))] -> Str
  :d "Formats causal references list into bulleted items."
  (if (list-empty? refs)
      ""
      (let [(head (option-or (list-head refs) ""))
            (tail (option-or (list-tail refs) (list)))
            (line (str "- " head "\n"))]
        (str line (format-refs-rows tail)))))

(df format-causal-refs [(refs (List Str))] -> Str
  :d "Renders causal parent references or root node indicator."
  (if (list-empty? refs)
      "*Root node (zero parent references).*"
      (format-refs-rows refs)))

(df hydrate-to-markdown [(chunk MemoryChunk)] -> Str
  :d "Projects canonical MemoryChunk record into human-readable Markdown."
  (let [(header (str "# Memory Chunk: " (.-id chunk) "\n\n"))
        (meta (str "**Timestamp**: " (string-from-int64 (.-ts chunk))
                   " | **Confidence**: " (string-from-float64 (.-conf chunk)) "\n\n"))
        (summary (str "### Summary\n" (.-summary chunk) "\n\n"))
        (facts (str "### Facts\n" (format-facts-table (.-facts chunk)) "\n\n"))
        (directives (str "### Directives\n" (format-directives (.-directives chunk)) "\n\n"))
        (causality (str "### Causal References\n" (format-causal-refs (.-refs chunk))))]
    (str header meta summary facts directives causality)))

(df render-summary-card [(chunk MemoryChunk)] -> Str
  :d "Renders compact dashboard summary card format."
  (str "[Chunk: " (.-id chunk) "] ("
       (string-from-float64 (.-conf chunk)) ") "
       (.-summary chunk) " ("
       (string-from-int64 (.-ts chunk)) " ms)"))
