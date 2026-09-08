(module asl-mem/tests/token-regex-test
  :d "Comprehensive falsifiable unit test suite for zero-copy AST regex token scanner"
  :x [run-tests
      test-compile-token-pattern
      test-match-exact-sequence
      test-match-quantifiers
      test-greedy-vs-nongreedy
      test-capture-groups
      test-scan-token-stream
      test-find-token-subsequences]
  :i [(token_regex :a tr)])

(df test-compile-token-pattern [] -> Bool
  :d "Verifies pattern compilation into TokenMatcher records, kind resolution, quantifiers, and capture groups."
  (let [(m (tr/compile-token-pattern "def <fn-name> _* foo+ bar? any*? opt?? (?<rest>_)"))
        (pats (.-patterns m))
        (p0 (option-or (list-get pats 0) (tr/TokenPattern :kind "" :literal (none) :quantifier "" :capture-group (none))))
        (p1 (option-or (list-get pats 1) (tr/TokenPattern :kind "" :literal (none) :quantifier "" :capture-group (none))))
        (p2 (option-or (list-get pats 2) (tr/TokenPattern :kind "" :literal (none) :quantifier "" :capture-group (none))))
        (p5 (option-or (list-get pats 5) (tr/TokenPattern :kind "" :literal (none) :quantifier "" :capture-group (none))))
        (p7 (option-or (list-get pats 7) (tr/TokenPattern :kind "" :literal (none) :quantifier "" :capture-group (none))))]
    (assert (= (list-length pats) 8) "Matcher must compile exactly 8 pattern elements")
    (assert (and (= (.-kind p0) "literal") (and (= (.-literal p0) (some "def")) (= (.-quantifier p0) "1"))) "Element 0 must be literal def with quantifier 1")
    (assert (and (= (.-kind p1) "wildcard") (= (.-capture-group p1) (some "fn-name"))) "Element 1 must be wildcard with fn-name capture group")
    (assert (and (= (.-kind p2) "wildcard") (= (.-quantifier p2) "*")) "Element 2 must be wildcard with * quantifier")
    (assert (and (= (.-kind p5) "any") (= (.-quantifier p5) "*?")) "Element 5 must be any with *? non-greedy quantifier")
    (assert (and (= (.-capture-group p7) (some "rest")) (= (.-quantifier p7) "1")) "Element 7 must have rest capture group and quantifier 1")
    true))

(df test-match-exact-sequence [] -> Bool
  :d "Verifies exact token sequence matching from start-idx 0, non-zero offset, and rejection on mismatch."
  (let [(m (tr/compile-token-pattern "module foo :d"))
        (tokens0 (list "module" "foo" ":d" "body" "end"))
        (r0 (tr/match-token-stream m tokens0 0))
        (tokens1 (list "prefix" "module" "foo" ":d"))
        (r1 (tr/match-token-stream m tokens1 1))
        (tokens-bad (list "module" "bar" ":d"))
        (r-bad (tr/match-token-stream m tokens-bad 0))]
    (assert (.-matched r0) "Pattern must match exact token sequence at index 0")
    (assert (= (.-tokens-consumed r0) 3) "Pattern must consume exactly 3 tokens at index 0")
    (assert (.-matched r1) "Pattern must match exact token sequence at offset 1")
    (assert (= (.-tokens-consumed r1) 3) "Pattern must consume exactly 3 tokens at offset 1")
    (assert (and (not (.-matched r-bad)) (= (.-tokens-consumed r-bad) 0)) "Mismatched token must fail match with zero tokens consumed")
    true))

(df test-match-quantifiers [] -> Bool
  :d "Verifies quantifier behaviors for ? (0/1), * (0/1/many), and + (reject 0, accept 1/many)."
  (let [(m-opt (tr/compile-token-pattern "a b? c"))
        (r-opt0 (tr/match-token-stream m-opt (list "a" "c") 0))
        (r-opt1 (tr/match-token-stream m-opt (list "a" "b" "c") 0))
        (m-star (tr/compile-token-pattern "a b* c"))
        (r-star0 (tr/match-token-stream m-star (list "a" "c") 0))
        (r-star3 (tr/match-token-stream m-star (list "a" "b" "b" "b" "c") 0))
        (m-plus (tr/compile-token-pattern "a b+ c"))
        (r-plus0 (tr/match-token-stream m-plus (list "a" "c") 0))
        (r-plus1 (tr/match-token-stream m-plus (list "a" "b" "c") 0))]
    (assert (and (.-matched r-opt0) (= (.-tokens-consumed r-opt0) 2)) "? quantifier must match 0 occurrences consuming 2 tokens")
    (assert (and (.-matched r-opt1) (= (.-tokens-consumed r-opt1) 3)) "? quantifier must match 1 occurrence consuming 3 tokens")
    (assert (and (.-matched r-star0) (= (.-tokens-consumed r-star0) 2)) "* quantifier must match 0 occurrences consuming 2 tokens")
    (assert (and (.-matched r-star3) (= (.-tokens-consumed r-star3) 5)) "* quantifier must match 3 occurrences consuming 5 tokens")
    (assert (not (.-matched r-plus0)) "+ quantifier must reject 0 occurrences")
    (assert (and (.-matched r-plus1) (= (.-tokens-consumed r-plus1) 3)) "+ quantifier must match 1 occurrence consuming 3 tokens")
    true))

(df test-greedy-vs-nongreedy [] -> Bool
  :d "Verifies maximal consumption under greedy * and + versus minimal consumption under *? and +?."
  (let [(tokens (list "x" "a" "b" "a" "y"))
        (m-greedy (tr/compile-token-pattern "x <mid:_*> a y"))
        (r-greedy (tr/match-token-stream m-greedy tokens 0))
        (m-nongreedy (tr/compile-token-pattern "x <mid:_*?> a b a y"))
        (r-nongreedy (tr/match-token-stream m-nongreedy tokens 0))
        (rep-tokens (list "start" "t" "t" "t" "end"))
        (m-greedy-plus (tr/compile-token-pattern "start <rep:t+> t end"))
        (r-greedy-plus (tr/match-token-stream m-greedy-plus rep-tokens 0))
        (m-nongreedy-plus (tr/compile-token-pattern "start <rep:t+?> t t end"))
        (r-nongreedy-plus (tr/match-token-stream m-nongreedy-plus rep-tokens 0))]
    (assert (.-matched r-greedy) "Greedy * pattern must match sequence")
    (assert (and (= (.-tokens-consumed r-greedy) 5) (= (option-or (map-get (.-captures r-greedy) "mid") (list)) (list "a" "b"))) "Greedy * must consume maximal mid tokens [a b]")
    (assert (and (.-matched r-nongreedy) (= (list-length (option-or (map-get (.-captures r-nongreedy) "mid") (list))) 0)) "Non-greedy *? must consume minimal 0 tokens to allow downstream match")
    (assert (and (.-matched r-greedy-plus) (= (list-length (option-or (map-get (.-captures r-greedy-plus) "rep") (list))) 2)) "Greedy + must consume maximal tokens [t t]")
    (assert (and (.-matched r-nongreedy-plus) (= (list-length (option-or (map-get (.-captures r-nongreedy-plus) "rep") (list))) 1)) "Non-greedy +? must consume minimal tokens [t]")
    true))

(df test-capture-groups [] -> Bool
  :d "Verifies single, quantified, and multiple named capture groups across token streams."
  (let [(m-ast (tr/compile-token-pattern "( def <name:_> ( <args:_*> ) )"))
        (ast-tokens (list "(" "def" "compute" "(" "x" "y" "z" ")" ")"))
        (r-ast (tr/match-token-stream m-ast ast-tokens 0))
        (m-dual (tr/compile-token-pattern "( module (?<mod>foo) :v <ver:bar> )"))
        (dual-tokens (list "(" "module" "foo" ":v" "bar" ")"))
        (r-dual (tr/match-token-stream m-dual dual-tokens 0))]
    (assert (.-matched r-ast) "AST function pattern must match sequence")
    (assert (= (option-or (map-get (.-captures r-ast) "name") (list)) (list "compute")) "name capture group must contain function symbol compute")
    (assert (= (option-or (map-get (.-captures r-ast) "args") (list)) (list "x" "y" "z")) "args capture group must contain argument list [x y z]")
    (assert (= (option-or (map-get (.-captures r-dual) "mod") (list)) (list "foo")) "mod capture group must capture foo via (?<mod>...)")
    (assert (= (option-or (map-get (.-captures r-dual) "ver") (list)) (list "bar")) "ver capture group must capture bar via <ver:...>")
    true))

(df test-scan-token-stream [] -> Bool
  :d "Verifies whole-stream non-overlapping scanner collecting match results."
  (let [(m (tr/compile-token-pattern "fn _"))
        (stream (list "let" "x" "fn" "f1" "and" "fn" "f2" "end"))
        (results (tr/scan-token-stream m stream))
        (m0 (option-or (list-get results 0) (tr/TokenMatchResult :matched false :tokens-consumed 0 :captures (map-empty))))
        (m1 (option-or (list-get results 1) (tr/TokenMatchResult :matched false :tokens-consumed 0 :captures (map-empty))))
        (empty-res (tr/scan-token-stream m (list "no" "matching" "elements")))]
    (assert (= (list-length results) 2) "Stream must yield exactly 2 non-overlapping matches")
    (assert (= (.-tokens-consumed m0) 2) "First scan match must consume 2 tokens")
    (assert (= (.-tokens-consumed m1) 2) "Second scan match must consume 2 tokens")
    (assert (= (list-length empty-res) 0) "Non-matching stream must yield 0 results")
    true))

(df test-find-token-subsequences [] -> Bool
  :d "Verifies subsequence span indices (Pair start end) across stream."
  (let [(m (tr/compile-token-pattern "( :f _ _ )"))
        (stream (list "dfs" "Rec" "(" ":f" "a" "Str" ")" "mid" "(" ":f" "b" "I64" ")"))
        (spans (tr/find-token-subsequences m stream))
        (s0 (option-or (list-get spans 0) (pair 0 0)))
        (s1 (option-or (list-get spans 1) (pair 0 0)))]
    (assert (= (list-length spans) 2) "Subsequence search must find 2 spans")
    (assert (and (= (.-first s0) 2) (= (.-second s0) 7)) "First subsequence span must be [2, 7)")
    (assert (and (= (.-first s1) 8) (= (.-second s1) 13)) "Second subsequence span must be [8, 13)")
    (assert (= (- (.-second s0) (.-first s0)) 5) "Span width must equal consumed token length of 5")
    true))

(df run-tests [] -> Bool
  :d "Executes complete verification suite spanning 35 assertions across 7 test cases."
  (and (test-compile-token-pattern)
       (and (test-match-exact-sequence)
            (and (test-match-quantifiers)
                 (and (test-greedy-vs-nongreedy)
                      (and (test-capture-groups)
                           (and (test-scan-token-stream)
                                (test-find-token-subsequences))))))))
