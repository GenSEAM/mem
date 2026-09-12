(module asl-mem/vfs-merge
  :d "In-memory 3-way AST structural merge engine for conflict-free S-expression form integration"
  :x [AstMergeResult
      CausalMergeResult
      ast-3way-merge
      merge-disjoint-forms
      format-ast-conflict
      merge-causal-branch-intent]
  :i [(ast_merge :a am)])

(dfs AstMergeResult
  (:f merged-content Str "Merged S-expression content or conflict markers")
  (:f conflicts (List Str) "List of conflicting symbol identifiers")
  (:f clean Bool "True if merge completed with zero conflicts")
  (:f merged-forms-count I64 "Total number of forms in merged output"))

(dfs AstForm
  (:f symbol-id Str "Matched symbol identifier")
  (:f raw-form Str "Verbatim top-level form text"))

(df format-ast-conflict [(symbol-id Str) (staged-form Str) (disk-form Str)] -> Str
  :d "Formats structured AST conflict marker for conflicting form."
  (str "<<<<<<< STAGED (" symbol-id ")\n"
       staged-form "\n"
       "=======\n"
       disk-form "\n"
       ">>>>>>> DISK (" symbol-id ")"))

(dfs ParseAccumulator
  (:f chunks (List (List Str)) "Collected chunks")
  (:f depth I64 "Paren depth"))

(df count-parens [(line Str)] -> I64
  (let [(opens (- (list-length (string-split line "(")) 1))
        (closes (- (list-length (string-split line ")")) 1))]
    (- opens closes)))

(df parse-lines-to-forms [(lines (List Str))] -> (List AstForm)
  :d "Groups text lines into top-level S-expression form records."
  (let [(collected (fold (fn [(acc ParseAccumulator) (line Str)] -> ParseAccumulator
                           (let [(is-new-form (and (string-starts-with? line "(") (<= (.-depth acc) 0)))
                                 (delta (count-parens line))
                                 (new-depth (if is-new-form delta (+ (.-depth acc) delta)))
                                 (chunks (.-chunks acc))
                                 (n (list-length chunks))]
                             (if is-new-form
                               (ParseAccumulator :chunks (list-append chunks (list (list line))) :depth new-depth)
                               (if (= n 0)
                                 (if (string-empty? (string-trim line))
                                   acc
                                   (ParseAccumulator :chunks (list-append chunks (list (list line))) :depth new-depth))
                                 (let [(last-idx (- n 1))
                                       (prev-chunk (option-or (list-get chunks last-idx) (list)))
                                       (init-acc (option-or (list-slice chunks 0 last-idx) (list)))
                                       (updated-chunk (list-append prev-chunk (list line)))]
                                   (ParseAccumulator :chunks (list-append init-acc (list updated-chunk)) :depth new-depth))))))
                         (ParseAccumulator :chunks (list) :depth 0)
                         lines))]
    (fold (fn [(acc (List AstForm)) (form-lines (List Str))] -> (List AstForm)
            (let [(raw (string-join form-lines "
"))
                  (f-line (option-or (list-get form-lines 0) ""))
                  (sym-id (am/extract-form-id f-line))]
              (if (string-empty? (string-trim raw))
                acc
                (list-append acc (list (AstForm :symbol-id sym-id :raw-form raw))))))
          (list)
          (.-chunks collected))))

(df parse-ast-forms [(content Str)] -> (List AstForm)
  :d "Parses buffer content into top-level AST form records."
  (if (string-empty? (string-trim content))
    (list)
    (parse-lines-to-forms (string-split content "\n"))))

(df find-form [(forms (List AstForm)) (target-id Str)] -> (Option AstForm)
  :d "Finds first top-level form matching target symbol identifier."
  (fold (fn [(acc (Option AstForm)) (f AstForm)] -> (Option AstForm)
          (mt acc
            ((some found) (some found))
            ((none) (if (= (.-symbol-id f) target-id) (some f) (none)))))
        (none)
        forms))

(dfs FormMergeOutcome
  (:f text Str "Resolved form text or conflict block")
  (:f conflict-id (Option Str) "Conflicted symbol identifier if conflicting"))

(df resolve-3way-form [(sym Str) (s-raw Str) (b-opt (Option AstForm)) (d-opt (Option AstForm))] -> FormMergeOutcome
  :d "Applies 3-way structural merge rules to single staged form."
  (mt b-opt
    ((none)
     (mt d-opt
       ((none) (FormMergeOutcome :text s-raw :conflict-id (none)))
       ((some df)
        (let [(d-raw (.-raw-form df))]
          (if (= s-raw d-raw)
            (FormMergeOutcome :text s-raw :conflict-id (none))
            (FormMergeOutcome :text (format-ast-conflict sym s-raw d-raw) :conflict-id (some sym)))))))
    ((some bf)
     (let [(b-raw (.-raw-form bf))]
       (mt d-opt
         ((none)
          (if (= s-raw b-raw)
            (FormMergeOutcome :text "" :conflict-id (none))
            (FormMergeOutcome :text (format-ast-conflict sym s-raw "(deleted on disk)") :conflict-id (some sym))))
         ((some df)
          (let [(d-raw (.-raw-form df))]
            (if (= s-raw d-raw)
              (FormMergeOutcome :text s-raw :conflict-id (none))
              (if (= s-raw b-raw)
                (FormMergeOutcome :text d-raw :conflict-id (none))
                (if (= d-raw b-raw)
                  (FormMergeOutcome :text s-raw :conflict-id (none))
                  (FormMergeOutcome :text (format-ast-conflict sym s-raw d-raw) :conflict-id (some sym))))))))))))

(dfs MergeAccumulator
  (:f merged (List Str) "List of merged form texts")
  (:f conflicts (List Str) "List of conflicting symbol identifiers")
  (:f seen (List Str) "List of processed symbol identifiers"))

(df merge-disjoint-forms [(base-content Str) (staged-content Str) (disk-content Str)] -> AstMergeResult
  :d "Merges non-overlapping top-level AST forms across base staged and disk buffers."
  (let [(b-forms (parse-ast-forms base-content))
        (s-forms (parse-ast-forms staged-content))
        (d-forms (parse-ast-forms disk-content))
        (init (MergeAccumulator :merged (list) :conflicts (list) :seen (list)))
        (after-s (fold (fn [(acc MergeAccumulator) (sf AstForm)] -> MergeAccumulator
                         (let [(sym (.-symbol-id sf))
                               (s-raw (.-raw-form sf))
                               (b-opt (find-form b-forms sym))
                               (d-opt (find-form d-forms sym))
                               (outcome (resolve-3way-form sym s-raw b-opt d-opt))
                               (res-text (.-text outcome))
                               (cur-m (.-merged acc))
                               (cur-c (.-conflicts acc))
                               (cur-seen (list-append (.-seen acc) (list sym)))
                               (next-m (if (string-empty? res-text) cur-m (list-append cur-m (list res-text))))
                               (next-c (mt (.-conflict-id outcome)
                                         ((none) cur-c)
                                         ((some cid) (list-append cur-c (list cid)))))]
                           (MergeAccumulator :merged next-m :conflicts next-c :seen cur-seen)))
                       init
                       s-forms))
        (final-acc (fold (fn [(acc MergeAccumulator) (df AstForm)] -> MergeAccumulator
                           (let [(sym (.-symbol-id df))
                                 (seen (.-seen acc))]
                             (if (list-contains? seen sym)
                               acc
                               (let [(b-opt (find-form b-forms sym))
                                     (d-raw (.-raw-form df))
                                     (cur-m (.-merged acc))
                                     (cur-c (.-conflicts acc))
                                     (cur-seen (list-append seen (list sym)))]
                                 (mt b-opt
                                   ((none)
                                    (MergeAccumulator :merged (list-append cur-m (list d-raw)) :conflicts cur-c :seen cur-seen))
                                   ((some bf)
                                    (let [(b-raw (.-raw-form bf))]
                                      (if (= d-raw b-raw)
                                        (MergeAccumulator :merged cur-m :conflicts cur-c :seen cur-seen)
                                        (let [(c-block (format-ast-conflict sym "(deleted in staged)" d-raw))]
                                          (MergeAccumulator :merged (list-append cur-m (list c-block))
                                                            :conflicts (list-append cur-c (list sym))
                                                            :seen cur-seen))))))))))
                         after-s
                         d-forms))
        (merged-list (.-merged final-acc))
        (conflicts-list (.-conflicts final-acc))
        (merged-text (string-join merged-list "\n\n"))]
    (AstMergeResult
      :merged-content merged-text
      :conflicts conflicts-list
      :clean (= (list-length conflicts-list) 0)
      :merged-forms-count (list-length merged-list))))

(df ast-3way-merge [(base-content Str) (staged-content Str) (disk-content Str)] -> AstMergeResult
  :d "Performs 3-way AST structural merge between base disk and staged buffers."
  (merge-disjoint-forms base-content staged-content disk-content))

(dfs CausalMergeResult
  (:f merged-result AstMergeResult "AST 3-way merge outcome")
  (:f causal-id Str "Causal message ID associated with staged intent")
  (:f intent-conflict? Bool "True if structural merge produced unresolved conflicts"))

(df merge-causal-branch-intent [(base-content Str) (staged-content Str) (disk-content Str) (causal-id Str)] -> CausalMergeResult
  :d "Resolves causal branch intent against base and disk using AST 3-way merge and tags conflicts."
  (let [(m (ast-3way-merge base-content staged-content disk-content))]
    (CausalMergeResult
      :merged-result m
      :causal-id causal-id
      :intent-conflict? (not (.-clean m)))))
