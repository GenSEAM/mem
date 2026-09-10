(module asl-mem/ast-merge
  :exports [
    ASTCollisionRecord
    HomoiconicForm
    AstMergeOutcome
    AstMergeResult
    clean-form-id
    extract-form-kind
    extract-form-id
    parse-homoiconic-forms
    find-ast-form
    resolve-homoiconic-form
    format-ast-collision
    universal-ast-3way-merge
    merge-sexp-lists
  ])

(dfs ASTCollisionRecord
  (:f symbol-id Str "Conflicted symbol identifier")
  (:f base-form Str "Original baseline form text")
  (:f branch-a Str "Branch A mutation text")
  (:f branch-b Str "Branch B mutation text")
  (:f collision-type Str "Type of collision"))

(dfs HomoiconicForm
  (:f symbol-id Str "Unique form identifier")
  (:f kind Str "Construct kind")
  (:f raw-form Str "Full S-expression string")
  (:f hash Str "Deterministic structural hash"))

(dfs AstMergeOutcome
  (:f text Str "Resolved S-expression form or empty if deleted")
  (:f collision (Option ASTCollisionRecord) "Collision record if conflicting"))

(dfs AstMergeResult
  (:f merged-content Str "Full merged file content")
  (:f conflicts (List Str) "List of conflicting symbol identifiers")
  (:f collisions (List ASTCollisionRecord) "List of structured collision records")
  (:f clean Bool "True if zero conflicts")
  (:f merged-forms-count I64 "Number of successfully resolved forms"))

(df format-ast-collision [(c ASTCollisionRecord)] -> Str
  :d "Formats structured S-expression collision block preserving delimiter balance."
  (str "(:collision\n"
       "  :symbol \"" (.-symbol-id c) "\"\n"
       "  :type \"" (.-collision-type c) "\"\n"
       "  :branchA \n" (.-branch-a c) "\n"
       "  :branchB \n" (.-branch-b c) "\n"
       "  :base \n" (.-base-form c) ")"))

(df clean-form-id [(raw-token Str)] -> Str
  :d "Strips punctuation and delimiters from symbol identifier."
  (let [(t1 (string-replace raw-token "(" ""))
        (t2 (string-replace t1 ")" ""))
        (t3 (string-replace t2 "[" ""))
        (t4 (string-replace t3 "]" ""))
        (t5 (string-replace t4 "\"" ""))
        (t6 (string-replace t5 ":" ""))]
    (string-trim t6)))

(df extract-form-kind [(first-line Str)] -> Str
  :d "Extracts structural construct kind from form header."
  (let [(trimmed (string-trim first-line))]
    (if (string-starts-with? trimmed "(df ")
      "df"
      (if (string-starts-with? trimmed "(dfs ")
        "dfs"
        (if (string-starts-with? trimmed "(module ")
          "module"
          (if (string-starts-with? trimmed "(:task ")
            "task"
            (if (string-starts-with? trimmed "(:msg ")
              "msg"
              (if (string-starts-with? trimmed "(:adr ")
                "adr"
                "form"))))))))

(df extract-form-id [(first-line Str)] -> Str
  :d "Extracts symbol identifier from header line of form."
  (let [(trimmed (string-trim first-line))
        (words (string-split trimmed " "))
        (w-cnt (list-length words))]
    (if (<= w-cnt 1)
      (clean-form-id trimmed)
      (if (or (string-starts-with? trimmed "(:task")
              (string-starts-with? trimmed "(:msg")
              (string-starts-with? trimmed "(:adr"))
        (let [(id-opt (fold (fn [(acc (Option Str)) (w Str)] -> (Option Str)
                              (mt acc
                                ((some found) (some found))
                                ((none)
                                 (if (or (string-contains? w "\"") (string-starts-with? w "T") (string-starts-with? w "ADR-"))
                                   (some (clean-form-id w))
                                   (none)))))
                            (none)
                            words))]
          (option-or id-opt (clean-form-id (option-or (list-get words 1) "anonymous"))))
        (let [(w1 (option-or (list-get words 1) ""))]
          (if (or (string-empty? w1) (string-starts-with? w1 "["))
            (clean-form-id (option-or (list-get words 0) "form"))
            (clean-form-id w1)))))))

(df parse-homoiconic-forms [(content Str)] -> (List HomoiconicForm)
  :d "Parses buffer content into structured HomoiconicForm records."
  (if (string-empty? (string-trim content))
    (list)
    (let [(lines (string-split content "\n"))
          (collected (fold (fn [(acc (List (List Str))) (line Str)] -> (List (List Str))
                             (let [(is-new-form (or (string-starts-with? line "(") (string-starts-with? line "(:")))
                                   (n (list-length acc))]
                               (if is-new-form
                                 (list-append acc (list (list line)))
                                 (if (= n 0)
                                   (if (string-empty? (string-trim line))
                                     acc
                                     (list-append acc (list (list line))))
                                   (let [(last-idx (- n 1))
                                         (prev-chunk (option-or (list-get acc last-idx) (list)))
                                         (init-acc (option-or (list-slice acc 0 last-idx) (list)))
                                         (updated-chunk (list-append prev-chunk (list line)))]
                                     (list-append init-acc (list updated-chunk)))))))
                           (list)
                           lines))]
      (fold (fn [(acc (List HomoiconicForm)) (form-lines (List Str))] -> (List HomoiconicForm)
              (let [(raw (string-trim (string-join form-lines "\n")))
                    (f-line (option-or (list-get form-lines 0) ""))
                    (sym-id (extract-form-id f-line))
                    (kind (extract-form-kind f-line))]
                (if (string-empty? (string-trim raw))
                  acc
                  (list-append acc (list (HomoiconicForm
                                           :symbol-id sym-id
                                           :kind kind
                                           :raw-form raw
                                           :hash (str (string-length raw) ":" sym-id)))))))
            (list)
            collected))))

(df find-ast-form [(forms (List HomoiconicForm)) (target-id Str)] -> (Option HomoiconicForm)
  :d "Finds first top-level form matching target symbol identifier."
  (fold (fn [(acc (Option HomoiconicForm)) (f HomoiconicForm)] -> (Option HomoiconicForm)
          (mt acc
            ((some found) (some found))
            ((none) (if (= (.-symbol-id f) target-id) (some f) (none)))))
        (none)
        forms))

(df resolve-a-only [(sym Str) (a-raw Str) (base-opt (Option HomoiconicForm))] -> AstMergeOutcome
  :d "Resolves 3-way merge when form exists only in branch A."
  (mt base-opt
    ((none) (AstMergeOutcome :text a-raw :collision (none)))
    ((some base-f)
     (let [(base-raw (.-raw-form base-f))]
       (if (= a-raw base-raw)
         (AstMergeOutcome :text "" :collision (none))
         (let [(col (ASTCollisionRecord
                      :symbol-id sym
                      :base-form base-raw
                      :branch-a a-raw
                      :branch-b "(deleted in branch-b)"
                      :collision-type "modify-vs-delete"))]
           (AstMergeOutcome :text (format-ast-collision col) :collision (some col))))))))

(df resolve-b-only [(sym Str) (b-raw Str) (base-opt (Option HomoiconicForm))] -> AstMergeOutcome
  :d "Resolves 3-way merge when form exists only in branch B."
  (mt base-opt
    ((none) (AstMergeOutcome :text b-raw :collision (none)))
    ((some base-f)
     (let [(base-raw (.-raw-form base-f))]
       (if (= b-raw base-raw)
         (AstMergeOutcome :text "" :collision (none))
         (let [(col (ASTCollisionRecord
                      :symbol-id sym
                      :base-form base-raw
                      :branch-a "(deleted in branch-a)"
                      :branch-b b-raw
                      :collision-type "delete-vs-modify"))]
           (AstMergeOutcome :text (format-ast-collision col) :collision (some col))))))))

(df resolve-both-present [(sym Str) (a-raw Str) (b-raw Str) (base-opt (Option HomoiconicForm))] -> AstMergeOutcome
  :d "Resolves 3-way merge when form exists in both branch A and branch B."
  (if (= a-raw b-raw)
    (AstMergeOutcome :text a-raw :collision (none))
    (mt base-opt
      ((none)
       (let [(col (ASTCollisionRecord
                    :symbol-id sym
                    :base-form "(none)"
                    :branch-a a-raw
                    :branch-b b-raw
                    :collision-type "divergent-addition"))]
         (AstMergeOutcome :text (format-ast-collision col) :collision (some col))))
      ((some base-f)
       (let [(base-raw (.-raw-form base-f))]
         (if (= a-raw base-raw)
           (AstMergeOutcome :text b-raw :collision (none))
           (if (= b-raw base-raw)
             (AstMergeOutcome :text a-raw :collision (none))
             (let [(col (ASTCollisionRecord
                          :symbol-id sym
                          :base-form base-raw
                          :branch-a a-raw
                          :branch-b b-raw
                          :collision-type "divergent-modification"))]
               (AstMergeOutcome :text (format-ast-collision col) :collision (some col))))))))))

(df resolve-homoiconic-form [(sym Str)
                             (base-opt (Option HomoiconicForm))
                             (branch-a-opt (Option HomoiconicForm))
                             (branch-b-opt (Option HomoiconicForm))] -> AstMergeOutcome
  :d "Applies 3-way structural merge rules to single homoiconic form."
  (mt branch-a-opt
    ((none)
     (mt branch-b-opt
       ((none) (AstMergeOutcome :text "" :collision (none)))
       ((some fb) (resolve-b-only sym (.-raw-form fb) base-opt))))
    ((some fa)
     (mt branch-b-opt
       ((none) (resolve-a-only sym (.-raw-form fa) base-opt))
       ((some fb) (resolve-both-present sym (.-raw-form fa) (.-raw-form fb) base-opt))))))

(dfs AstMergeAccumulator
  (:f merged (List Str) "List of merged form texts")
  (:f conflicts (List Str) "List of conflicting symbol identifiers")
  (:f collisions (List ASTCollisionRecord) "List of structured collision records")
  (:f seen (List Str) "List of processed symbol identifiers"))

(df universal-ast-3way-merge [(base-src Str) (branch-a-src Str) (branch-b-src Str)] -> AstMergeResult
  :d "Performs recursive 3-way AST structural merge between base branch A and branch B."
  (let [(base-forms (parse-homoiconic-forms base-src))
        (a-forms (parse-homoiconic-forms branch-a-src))
        (b-forms (parse-homoiconic-forms branch-b-src))
        (init (AstMergeAccumulator :merged (list) :conflicts (list) :collisions (list) :seen (list)))
        (after-a (fold (fn [(acc AstMergeAccumulator) (fa HomoiconicForm)] -> AstMergeAccumulator
                         (let [(sym (.-symbol-id fa))
                               (base-opt (find-ast-form base-forms sym))
                               (b-opt (find-ast-form b-forms sym))
                               (outcome (resolve-homoiconic-form sym base-opt (some fa) b-opt))
                               (txt (.-text outcome))
                               (cur-m (.-merged acc))
                               (cur-c (.-conflicts acc))
                               (cur-col (.-collisions acc))
                               (cur-seen (list-append (.-seen acc) (list sym)))
                               (next-m (if (string-empty? txt) cur-m (list-append cur-m (list txt))))
                               (next-c (mt (.-collision outcome)
                                         ((none) cur-c)
                                         ((some _) (list-append cur-c (list sym)))))
                               (next-col (mt (.-collision outcome)
                                           ((none) cur-col)
                                           ((some c) (list-append cur-col (list c)))))]
                           (AstMergeAccumulator
                             :merged next-m
                             :conflicts next-c
                             :collisions next-col
                             :seen cur-seen)))
                       init
                       a-forms))
        (final-acc (fold (fn [(acc AstMergeAccumulator) (fb HomoiconicForm)] -> AstMergeAccumulator
                           (let [(sym (.-symbol-id fb))
                                 (seen (.-seen acc))]
                             (if (list-contains? seen sym)
                               acc
                               (let [(base-opt (find-ast-form base-forms sym))
                                     (outcome (resolve-homoiconic-form sym base-opt (none) (some fb)))
                                     (txt (.-text outcome))
                                     (cur-m (.-merged acc))
                                     (cur-c (.-conflicts acc))
                                     (cur-col (.-collisions acc))
                                     (cur-seen (list-append seen (list sym)))
                                     (next-m (if (string-empty? txt) cur-m (list-append cur-m (list txt))))
                                     (next-c (mt (.-collision outcome)
                                               ((none) cur-c)
                                               ((some _) (list-append cur-c (list sym)))))
                                     (next-col (mt (.-collision outcome)
                                                 ((none) cur-col)
                                                 ((some c) (list-append cur-col (list c)))))]
                                 (AstMergeAccumulator
                                   :merged next-m
                                   :conflicts next-c
                                   :collisions next-col
                                   :seen cur-seen)))))
                         after-a
                         b-forms))
        (merged-list (.-merged final-acc))
        (conflicts-list (.-conflicts final-acc))
        (collisions-list (.-collisions final-acc))
        (merged-text (string-join merged-list "\n\n"))]
    (AstMergeResult
      :merged-content merged-text
      :conflicts conflicts-list
      :collisions collisions-list
      :clean (= (list-length conflicts-list) 0)
      :merged-forms-count (list-length merged-list))))

(df merge-sexp-lists [(base (List Str)) (a (List Str)) (b (List Str))] -> (List Str)
  :d "Merges lists of S-expression items using set union for additions and deduplication."
  (let [(added-a (fold (fn [(acc (List Str)) (item Str)] -> (List Str)
                         (if (list-contains? base item)
                           acc
                           (if (list-contains? acc item) acc (list-append acc (list item)))))
                       (list)
                       a))
        (added-b (fold (fn [(acc (List Str)) (item Str)] -> (List Str)
                         (if (list-contains? base item)
                           acc
                           (if (or (list-contains? added-a item) (list-contains? acc item))
                             acc
                             (list-append acc (list item)))))
                       (list)
                       b))
        (retained-base (fold (fn [(acc (List Str)) (item Str)] -> (List Str)
                               (if (and (list-contains? a item) (list-contains? b item))
                                 (list-append acc (list item))
                                 acc))
                             (list)
                             base))]
    (list-concat retained-base (list-concat added-a added-b))))
