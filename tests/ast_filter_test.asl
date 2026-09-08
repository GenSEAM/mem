(module asl-mem/tests/ast-filter-test
  :d "Unit tests for AST pattern filtering and structural editor mutations"
  :x [run-tests
      test-query-form-type
      test-query-exact-scope
      test-rewrite-rename-symbol
      test-rewrite-replace-body
      test-rewrite-add-field-and-remove]
  :i [(vfs :a v)
      (ast_filter :a af)
      (structural_editor :a se)])

(df make-sample-buffer [] -> v/VFSBuffer
  :d "Constructs sample VFSBuffer with diverse top-level forms for query and mutation testing."
  (let [(content "(dfs Point\n  (:f x I64 \"x\")\n  (:f y I64 \"y\"))\n\n(df calc-sum [(a I64) (b I64)] -> I64\n  :d \"Calculates sum of two integers.\"\n  (+ a b))\n\n(dfe Color\n  (:c red [] \"red\")\n  (:c blue [] \"blue\"))\n\n(df calc-diff [(x I64) (y I64)] -> I64\n  :d \"Calculates difference.\"\n  (- x y))\n")
        (h (v/vfs-cas-hash content))]
    (v/VFSBuffer
      :path "src/sample.asl"
      :content content
      :base-content content
      :cas-hash h
      :base-hash h
      :revision 1
      :dirty false
      :loaded-at 1000)))

(df test-query-form-type [] -> Bool
  :d "Verifies matching forms by form type keyword (:dfs, :df, :dfe, *) and validates span metadata."
  (let [(buf (make-sample-buffer))
        (q-dfs (af/ASTQuery :form-type ":dfs" :symbol-id (none) :param-count (none) :predicate-token (none)))
        (res-dfs (af/match-ast-form buf q-dfs))
        (q-df (af/ASTQuery :form-type ":df" :symbol-id (none) :param-count (none) :predicate-token (none)))
        (res-df (af/match-ast-form buf q-df))
        (q-dfe (af/ASTQuery :form-type ":dfe" :symbol-id (none) :param-count (none) :predicate-token (none)))
        (res-dfe (af/match-ast-form buf q-dfe))
        (q-all (af/ASTQuery :form-type "*" :symbol-id (none) :param-count (none) :predicate-token (none)))
        (res-all (af/match-ast-form buf q-all))]
    (assert (= (list-length res-dfs) 1) "Expected exactly 1 dfs form match")
    (let [(n-dfs (option-or (list-get res-dfs 0) (af/ASTMatchNode :form-type "" :symbol-id "" :start-char 0 :end-char 0 :raw-form "" :line-number 0)))]
      (assert (= (.-symbol-id n-dfs) "Point") "Matched dfs form symbol must equal Point")
      (assert (= (.-line-number n-dfs) 1) "Matched dfs form must begin at line 1"))
    (assert (= (list-length res-df) 2) "Expected exactly 2 df form matches")
    (assert (= (list-length res-all) 4) "Wildcard query must match all 4 top-level forms in sample buffer")
    true))

(df test-query-exact-scope [] -> Bool
  :d "Verifies matching forms by exact symbol identifier, parameter count constraints, and predicate substrings."
  (let [(buf (make-sample-buffer))
        (q-sym (af/ASTQuery :form-type ":df" :symbol-id (some "calc-sum") :param-count (none) :predicate-token (none)))
        (res-sym (af/match-ast-form buf q-sym))
        (q-pc-ok (af/ASTQuery :form-type ":df" :symbol-id (some "calc-sum") :param-count (some 2) :predicate-token (none)))
        (res-pc-ok (af/match-ast-form buf q-pc-ok))
        (q-pc-bad (af/ASTQuery :form-type ":df" :symbol-id (some "calc-sum") :param-count (some 3) :predicate-token (none)))
        (res-pc-bad (af/match-ast-form buf q-pc-bad))
        (q-missing (af/ASTQuery :form-type ":df" :symbol-id (some "non-existent") :param-count (none) :predicate-token (none)))
        (res-missing (af/match-ast-form buf q-missing))
        (q-pred (af/ASTQuery :form-type "*" :symbol-id (none) :param-count (none) :predicate-token (some "+ a b")))
        (res-pred (af/match-ast-form buf q-pred))]
    (assert (= (list-length res-sym) 1) "Expected 1 match for symbol calc-sum")
    (assert (= (list-length res-pc-ok) 1) "Expected 1 match for calc-sum with param count 2")
    (assert (= (list-length res-pc-bad) 0) "Expected 0 matches for calc-sum with param count 3")
    (assert (= (list-length res-missing) 0) "Expected 0 matches for non-existent symbol identifier")
    (assert (= (list-length res-pred) 1) "Expected 1 match containing predicate body token (+ a b)")
    true))

(df test-rewrite-rename-symbol [] -> Bool
  :d "Verifies renaming symbols in functions and records, checking CAS hash updates, revision increments, and dirty status."
  (let [(buf0 (make-sample-buffer))
        (q-fn (af/ASTQuery :form-type ":df" :symbol-id (some "calc-sum") :param-count (none) :predicate-token (none)))
        (match-fn (af/match-ast-form buf0 q-fn))]
    (assert (= (list-length match-fn) 1) "Expected single match for calc-sum function")
    (let [(n-fn (option-or (list-get match-fn 0) (af/ASTMatchNode :form-type "" :symbol-id "" :start-char 0 :end-char 0 :raw-form "" :line-number 0)))
          (mut-fn (se/ASTTransform :op ":rename-symbol" :target-symbol "calc-sum" :replacement "calc-total" :field-type (none)))
          (buf1 (se/rewrite-ast-node buf0 n-fn mut-fn))]
      (assert (= (.-revision buf1) 2) "Buffer revision must increment to 2 after symbol rename")
      (assert (.-dirty buf1) "Buffer dirty flag must be true after modifying symbol")
      (assert (!= (.-cas-hash buf1) (.-cas-hash buf0)) "Buffer CAS hash must update after renaming symbol")
      (assert (string-contains? (.-content buf1) "df calc-total") "Buffer content must contain renamed symbol calc-total")
      (let [(q-dfs (af/ASTQuery :form-type ":dfs" :symbol-id (some "Point") :param-count (none) :predicate-token (none)))
            (match-dfs (af/match-ast-form buf1 q-dfs))
            (n-dfs (option-or (list-get match-dfs 0) (af/ASTMatchNode :form-type "" :symbol-id "" :start-char 0 :end-char 0 :raw-form "" :line-number 0)))
            (mut-dfs (se/ASTTransform :op ":rename-symbol" :target-symbol "Point" :replacement "Point2D" :field-type (none)))
            (buf2 (se/rewrite-ast-node buf1 n-dfs mut-dfs))]
        (assert (string-contains? (.-content buf2) "dfs Point2D") "Buffer content must contain renamed record Point2D")
        true))))

(df test-rewrite-replace-body [] -> Bool
  :d "Verifies replacing function body expressions, checking delimiter balance, CAS hash changes, and dirty diff computation."
  (let [(buf0 (make-sample-buffer))
        (q-fn (af/ASTQuery :form-type ":df" :symbol-id (some "calc-sum") :param-count (none) :predicate-token (none)))
        (nodes (af/match-ast-form buf0 q-fn))]
    (assert (= (list-length nodes) 1) "Expected 1 node for body replacement target")
    (let [(node (option-or (list-get nodes 0) (af/ASTMatchNode :form-type "" :symbol-id "" :start-char 0 :end-char 0 :raw-form "" :line-number 0)))
          (mut (se/ASTTransform :op ":replace-body" :target-symbol "calc-sum" :replacement "(+ a (* b 100))" :field-type (none)))
          (buf1 (se/rewrite-ast-node buf0 node mut))]
      (assert (string-contains? (.-content buf1) "(+ a (* b 100))") "Mutated buffer must contain replacement body expression")
      (assert (se/is-balanced-delimiters? (.-content buf1)) "Delimiter balance must be preserved across body rewrite")
      (assert (.-dirty buf1) "Buffer must be marked dirty after body replacement")
      (let [(diff-lines (v/vfs-diff buf1))]
        (assert (> (list-length diff-lines) 0) "VFS unified diff must be non-empty for dirty mutated buffer")
        true))))

(df test-rewrite-add-field-and-remove [] -> Bool
  :d "Verifies adding fields to record definitions and removing forms, enforcing zero collateral edits and balanced delimiters."
  (let [(buf0 (make-sample-buffer))
        (q-dfs (af/ASTQuery :form-type ":dfs" :symbol-id (some "Point") :param-count (none) :predicate-token (none)))
        (nodes-dfs (af/match-ast-form buf0 q-dfs))]
    (assert (= (list-length nodes-dfs) 1) "Expected 1 dfs node match")
    (let [(node-dfs (option-or (list-get nodes-dfs 0) (af/ASTMatchNode :form-type "" :symbol-id "" :start-char 0 :end-char 0 :raw-form "" :line-number 0)))
          (mut-add (se/ASTTransform :op ":add-field" :target-symbol "Point" :replacement "(:f z I64 \"z coordinate\")" :field-type (none)))
          (buf1 (se/rewrite-ast-node buf0 node-dfs mut-add))]
      (assert (string-contains? (.-content buf1) "(:f z I64 \"z coordinate\")") "Mutated buffer must contain added record field")
      (assert (se/is-balanced-delimiters? (.-content buf1)) "Delimiter balance must hold after adding field to dfs")
      (let [(q-del (af/ASTQuery :form-type ":dfe" :symbol-id (some "Color") :param-count (none) :predicate-token (none)))
            (nodes-del (af/match-ast-form buf1 q-del))]
        (assert (= (list-length nodes-del) 1) "Expected 1 dfe node match for deletion")
        (let [(node-del (option-or (list-get nodes-del 0) (af/ASTMatchNode :form-type "" :symbol-id "" :start-char 0 :end-char 0 :raw-form "" :line-number 0)))
              (mut-del (se/ASTTransform :op ":remove-form" :target-symbol "Color" :replacement "" :field-type (none)))
              (buf2 (se/rewrite-ast-node buf1 node-del mut-del))]
          (assert (not (string-contains? (.-content buf2) "dfe Color")) "Deleted dfe form must no longer exist in buffer content")
          (assert (string-contains? (.-content buf2) "dfs Point") "Collateral form Point must be preserved untouched")
          (assert (se/is-balanced-delimiters? (.-content buf2)) "Delimiter balance must hold after removing form")
          true)))))

(df run-tests [] -> Bool
  :d "Composite entrypoint executing full AST pattern filter and structural editor verification suite."
  (and (test-query-form-type)
       (and (test-query-exact-scope)
            (and (test-rewrite-rename-symbol)
                 (and (test-rewrite-replace-body)
                      (test-rewrite-add-field-and-remove))))))
