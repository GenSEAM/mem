(module asl-mem/subtree-cache
  :doc "Content-Addressable Storage subtree memoization for incremental AST re-parsing."
  :x [SubtreeEntry
      SubtreeCache
      index-subtrees
      lookup-subtree
      reconcile-incremental-diff]
  :i [(vfs :a v)])

(dfs SubtreeEntry
  (:f hash Str "Content-addressed hash of subtree form")
  (:f name Str "Symbol identifier declared by the form")
  (:f kind Str "Form keyword kind: df, dfs, dfe, module")
  (:f ast-node Str "Raw serialized text of subtree form"))

(dfs SubtreeCache
  (:f entries (Map Str SubtreeEntry) "Map of CAS hash to SubtreeEntry"))

(df compute-subtree-hash [(content Str)] -> Str
  :doc "Computes deterministic CAS hash of subtree form text."
  (let [(chars (string-chars content))
        (len (string-length content))
        (h (fold (fn [(acc I64) (ch Str)] -> I64
                   (mod (+ (* acc 31) (v/char-to-code ch)) 2147483647))
                 5381
                 chars))]
    (str "cas-" (string-from-int64 h) "-" (string-from-int64 len))))

(df count-parens-loop [(chars (List Str)) (depth I64)] -> I64
  :doc "Counts net open minus close parens in character list."
  (if (list-empty? chars)
      depth
      (let [(c (option-or (list-head chars) ""))
            (rest (option-or (list-tail chars) (list)))
            (delta (cond ((= c "(") 1) ((= c ")") -1) (:else 0)))]
        (count-parens-loop rest (+ depth delta)))))

(df net-parens [(line Str)] -> I64
  :doc "Computes net open minus close parens in line."
  (count-parens-loop (string-chars line) 0))

(df extract-kind-and-name [(form Str)] -> (List Str)
  :doc "Extracts kind and name from form text."
  (let [(trimmed (string-trim form))
        (after-p (if (string-starts-with? trimmed "(")
                     (option-or (string-slice trimmed 1 (string-length trimmed)) "")
                     trimmed))
        (words (string-split (string-trim after-p) " "))
        (clean-words (filter (fn [(w Str)] -> Bool (> (string-length w) 0)) words))
        (k (option-or (list-get clean-words 0) "unknown"))
        (n (option-or (list-get clean-words 1) "anonymous"))]
    (list k n)))

(df make-subtree-entry [(form-text Str)] -> SubtreeEntry
  :doc "Constructs SubtreeEntry from raw form text."
  (let [(kn (extract-kind-and-name form-text))
        (k (option-or (list-get kn 0) "unknown"))
        (n (option-or (list-get kn 1) "anonymous"))
        (h (compute-subtree-hash form-text))]
    (SubtreeEntry
      :hash h
      :name n
      :kind k
      :ast-node form-text)))

(df parse-forms-loop [(lines (List Str)) (cur-lines (List Str)) (cur-depth I64) (acc (List SubtreeEntry))] -> (List SubtreeEntry)
  :doc "Parses top-level balanced forms from lines."
  (if (list-empty? lines)
      (if (list-empty? cur-lines)
          acc
          (let [(entry (make-subtree-entry (string-join cur-lines "\n")))]
            (list-append acc (list entry))))
      (let [(line (option-or (list-head lines) ""))
            (rest (option-or (list-tail lines) (list)))
            (trimmed (string-trim line))]
        (if (list-empty? cur-lines)
            (if (string-starts-with? trimmed "(")
                (let [(np (net-parens trimmed))]
                  (if (<= np 0)
                      (let [(entry (make-subtree-entry trimmed))]
                        (parse-forms-loop rest (list) 0 (list-append acc (list entry))))
                      (parse-forms-loop rest (list line) np acc)))
                (parse-forms-loop rest (list) 0 acc))
            (let [(np (net-parens line))
                  (new-depth (+ cur-depth np))
                  (new-lines (list-append cur-lines (list line)))]
              (if (<= new-depth 0)
                  (let [(entry (make-subtree-entry (string-join new-lines "\n")))]
                    (parse-forms-loop rest (list) 0 (list-append acc (list entry))))
                  (parse-forms-loop rest new-lines new-depth acc)))))))

(df build-cache-map [(entries (List SubtreeEntry)) (acc (Map Str SubtreeEntry))] -> (Map Str SubtreeEntry)
  :doc "Builds map of hash to SubtreeEntry."
  (if (list-empty? entries)
      acc
      (let [(entry (option-or (list-head entries) (SubtreeEntry :hash "" :name "" :kind "" :ast-node "")))
            (rest (option-or (list-tail entries) (list)))
            (h (.-hash entry))
            (next-acc (map-set acc h entry))]
        (build-cache-map rest next-acc))))

(df index-subtrees [(source Str)] -> SubtreeCache
  :doc "Indexes top-level forms from source text into a Content-Addressable SubtreeCache."
  (let [(lines (string-split source "\n"))
        (entries (parse-forms-loop lines (list) 0 (list)))
        (entries-map (build-cache-map entries (map-empty)))]
    (SubtreeCache :entries entries-map)))

(df lookup-subtree [(cache SubtreeCache) (hash Str)] -> (Option SubtreeEntry)
  :doc "Retrieves cached subtree entry by its CAS content hash."
  (map-get (.-entries cache) hash))

(df find-unchanged-entries [(entries (List SubtreeEntry)) (old-cache SubtreeCache) (acc (List SubtreeEntry))] -> (List SubtreeEntry)
  :doc "Collects entries from new source that match old cache by hash."
  (if (list-empty? entries)
      acc
      (let [(entry (option-or (list-head entries) (SubtreeEntry :hash "" :name "" :kind "" :ast-node "")))
            (rest (option-or (list-tail entries) (list)))
            (h (.-hash entry))
            (cached (lookup-subtree old-cache h))]
        (mt cached
          ((some old-entry)
           (find-unchanged-entries rest old-cache (list-append acc (list old-entry))))
          ((none)
           (find-unchanged-entries rest old-cache acc))))))

(df reconcile-incremental-diff [(old-cache SubtreeCache) (new-source Str)] -> (Pair (List SubtreeEntry) SubtreeCache)
  :doc "Reconciles incremental AST diff returning unchanged memoized forms and the updated cache."
  (let [(new-cache (index-subtrees new-source))
        (all-entries (map-values (.-entries new-cache)))
        (unchanged (find-unchanged-entries all-entries old-cache (list)))]
    (pair unchanged new-cache)))
