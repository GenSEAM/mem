(module asl-mem/spool-search-test
  :d "Unit tests for pure ASL in-memory BM25 spool search engine."
  :x [run-tests]
  :i [(spool_search :a ss)
      (bm25 :a bm)])

(df test-spool-indexing [] -> Bool
  :d "Verifies BM25 index construction over terminal lines."
  (let [(lines (list "building packages/asl-sh"
                     "compiling src/reducer.asl"
                     "compiling src/ansi.asl"
                     "all modules compiled cleanly"))
        (idx (ss/spool-index-lines lines))]
    (assert (= (.-doc-count idx) 4) "Index must contain 4 documents")
    (assert (> (.-avg-doc-length idx) 0.0) "Average document length must be positive")
    (assert (map-has? (.-term-doc-counts idx) "compiling") "Term doc counts must index compiling")
    (assert (= (option-or (map-get (.-term-doc-counts idx) "compiling") 0) 2) "Compiling appears in 2 documents")
    true))

(df test-spool-query-exact [] -> Bool
  :d "Verifies exact query matching and line mapping."
  (let [(lines (list "[INFO] Starting test suite"
                     "[DEBUG] Initializing sandbox"
                     "FAILED tests/unit_test.py::test_math - AssertionError: 1 != 2"
                     "[INFO] Summary: 1 failed"))
        (idx (ss/spool-index-lines lines))
        (matches (ss/spool-search-index idx lines "AssertionError" 5))]
    (assert (= (list-length matches) 1) "Exactly 1 line matches AssertionError")
    (let [(m (option-or (list-head matches) (ss/SpoolMatch :line-no 0 :text "" :score 0.0)))]
      (assert (= (.-line-no m) 3) "Matching line number must be 3")
      (assert (string-contains? (.-text m) "FAILED") "Matched text must contain FAILED")
      (assert (> (.-score m) 0.0) "Match score must be positive"))
    true))

(df test-spool-query-relevance [] -> Bool
  :d "Verifies multi-word query ranking by BM25 score."
  (let [(lines (list "[INFO] Compiling sources cleanly"
                     "src/main.rs:10:4 - error[E0308]: mismatched types"
                     "warning: unused variable `x`"
                     "error[E0382]: borrow of moved value `v`"
                     "[INFO] Build finished"))
        (idx (ss/spool-index-lines lines))
        (matches (ss/spool-search-index idx lines "error mismatched types" 2))]
    (assert (= (list-length matches) 2) "Two lines match error query")
    (let [(top (option-or (list-head matches) (ss/SpoolMatch :line-no 0 :text "" :score 0.0)))
          (second (option-or (list-get matches 1) (ss/SpoolMatch :line-no 0 :text "" :score 0.0)))]
      (assert (= (.-line-no top) 2) "Top match must be line 2 containing mismatched types")
      (assert (> (.-score top) (.-score second)) "Top match must outscore second match")
      (assert (string-contains? (.-text top) "mismatched") "Top match text must contain mismatched"))
    true))

(df test-spool-query-no-match [] -> Bool
  :d "Verifies zero matches when query terms are completely absent."
  (let [(lines (list "line one" "line two" "line three"))
        (idx (ss/spool-index-lines lines))
        (matches (ss/spool-search-index idx lines "nonexistentkeywordxyz" 5))]
    (assert (list-empty? matches) "Absent terms must produce zero matches")
    (assert (= (list-length matches) 0) "Match length must be 0")
    true))

(df test-spool-format-matches [] -> Bool
  :d "Verifies match formatting into concise lines."
  (let [(m1 (ss/SpoolMatch :line-no 12 :text "syntax error" :score 1.5))
        (m2 (ss/SpoolMatch :line-no 45 :text "fatal failure" :score 1.2))
        (formatted (ss/format-spool-matches (list m1 m2)))
        (empty-fmt (ss/format-spool-matches (list)))]
    (assert (string-contains? formatted "L12: syntax error") "Formatted output must contain L12")
    (assert (string-contains? formatted "L45: fatal failure") "Formatted output must contain L45")
    (assert (= empty-fmt "Zero matches found.") "Empty format must report zero matches")
    true))

(df test-spool-direct-query [] -> Bool
  :d "Verifies end-to-end direct spool text query."
  (let [(raw (str "Step 1: Init\n"
                  "Step 2: Processing batch\n"
                  "Step 3: Fatal panic during allocation\n"
                  "Step 4: Cleanup\n"))
        (matches (ss/spool-query-bm25 raw "panic allocation" 1))]
    (assert (= (list-length matches) 1) "Direct query must find 1 match")
    (let [(m (option-or (list-head matches) (ss/SpoolMatch :line-no 0 :text "" :score 0.0)))]
      (assert (= (.-line-no m) 3) "Matched line must be line 3")
      (assert (string-contains? (.-text m) "Fatal panic") "Matched text must contain Fatal panic")
      (assert (> (.-score m) 0.0) "Score must be positive"))
    true))

(df run-tests [] -> Bool
  :d "Runs all unit tests for in-memory BM25 spool search engine."
  (and (test-spool-indexing)
       (and (test-spool-query-exact)
            (and (test-spool-query-relevance)
                 (and (test-spool-query-no-match)
                      (and (test-spool-format-matches)
                           (test-spool-direct-query)))))))
