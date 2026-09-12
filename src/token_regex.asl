(module asl-mem/token-regex
  :d "Zero-copy AST regex token scanner operating over lexed AST tokens without allocations"
  :x [TokenPattern
      TokenMatchResult
      TokenMatcher
      compile-token-pattern
      match-token-stream
      scan-token-stream
      find-token-subsequences
      tokenize-pattern]
  :i [])

(dfs TokenPattern
  (:f kind Str "Pattern element kind: literal, wildcard, any")
  (:f literal (Option Str) "Optional exact token literal string to match")
  (:f quantifier Str "Quantifier specifier: 1, ?, *, +, ??, *?, +?")
  (:f capture-group (Option Str) "Optional capture group identifier name"))

(dfs TokenMatchResult
  (:f matched Bool "Boolean flag indicating whether pattern matched token stream")
  (:f tokens-consumed I64 "Number of tokens consumed from token stream")
  (:f captures (Map Str (List Str)) "Map of capture group names to captured token sequence slices"))

(dfs TokenMatcher
  (:f patterns (List TokenPattern) "Ordered sequence of compiled token pattern elements")
  (:f case-sensitive Bool "Flag controlling case sensitivity of literal matching"))

(df slice-tokens-loop [(tokens (List Str)) (idx I64) (end-idx I64) (acc (List Str))] -> (List Str)
  :d "Accumulates tokens in range [idx, end-idx)."
  (if (>= idx end-idx)
    acc
    (let [(tok (option-or (list-get tokens idx) ""))]
      (slice-tokens-loop tokens (+ idx 1) end-idx (list-append acc (list tok))))))

(df tokenize-pattern [(s Str)] -> (List Str)
  :d "Tokenizes pattern string into tokens, separating S-expression parentheses while preserving regex groups."
  (let [(has-group (string-contains? s "(?<"))
        (res (fold (fn [(acc (Pair (List Str) Str)) (ch Str)] -> (Pair (List Str) Str)
                     (let [(tokens (.-first acc))
                           (curr (.-second acc))]
                       (if (or (= ch " ") (or (= ch "\t") (or (= ch "\n") (= ch "\r"))))
                         (if (= curr "")
                           (pair tokens "")
                           (pair (list-append tokens (list curr)) ""))
                         (if (and (not has-group) (or (= ch "(") (= ch ")")))
                           (let [(t1 (if (= curr "") tokens (list-append tokens (list curr))))]
                             (pair (list-append t1 (list ch)) ""))
                           (pair tokens (str curr ch))))))
                   (pair (list) "")
                   (string-split s "")))]
    (if (= (.-second res) "")
      (.-first res)
      (list-append (.-first res) (list (.-second res))))))

(df parse-named-group-paren [(tok Str)] -> (Option (Pair Str Str))
  :d "Parses (?<name>subpattern) into (pair name subpattern)."
  (if (string-starts-with? tok "(?<")
    (mt (string-index-of tok ">")
      ((some gt-pos)
       (let [(name (option-or (string-slice tok 3 gt-pos) ""))
             (sub-start (+ gt-pos 1))
             (tot-len (string-length tok))
             (sub-end (if (string-ends-with? tok ")") (- tot-len 1) tot-len))
             (sub-tok (if (> sub-end sub-start) (option-or (string-slice tok sub-start sub-end) "") ""))]
         (some (pair name (if (= sub-tok "") "_" sub-tok)))))
      ((none) (none)))
    (none)))

(df parse-named-group-angle [(tok Str)] -> (Option (Pair Str Str))
  :d "Parses <name:subpattern> or <name> into (pair name subpattern)."
  (let [(tot-len (string-length tok))]
    (if (and (string-starts-with? tok "<") (and (string-ends-with? tok ">") (> tot-len 2)))
      (let [(inner (option-or (string-slice tok 1 (- tot-len 1)) ""))]
        (mt (string-index-of inner ":")
          ((some col-pos)
           (let [(name (option-or (string-slice inner 0 col-pos) ""))
                 (sub-tok (option-or (string-slice inner (+ col-pos 1) (string-length inner)) ""))]
             (some (pair name (if (= sub-tok "") "_" sub-tok)))))
          ((none)
           (some (pair inner "_")))))
      (none))))

(df parse-token-capture [(tok Str)] -> (Pair (Option Str) Str)
  :d "Extracts capture group name and remaining sub-token pattern."
  (mt (parse-named-group-paren tok)
    ((some p) (pair (some (.-first p)) (.-second p)))
    ((none)
     (mt (parse-named-group-angle tok)
       ((some p2) (pair (some (.-first p2)) (.-second p2)))
       ((none) (pair (none) tok))))))

(df extract-quantifier [(s Str)] -> (Pair Str Str)
  :d "Extracts quantifier suffix and returns (pair base quantifier)."
  (let [(len (string-length s))]
    (cond
      ((and (>= len 3) (string-ends-with? s "*?"))
       (pair (option-or (string-slice s 0 (- len 2)) "") "*?"))
      ((and (>= len 3) (string-ends-with? s "+?"))
       (pair (option-or (string-slice s 0 (- len 2)) "") "+?"))
      ((and (>= len 3) (string-ends-with? s "??"))
       (pair (option-or (string-slice s 0 (- len 2)) "") "??"))
      ((and (>= len 2) (string-ends-with? s "*"))
       (pair (option-or (string-slice s 0 (- len 1)) "") "*"))
      ((and (>= len 2) (string-ends-with? s "+"))
       (pair (option-or (string-slice s 0 (- len 1)) "") "+"))
      ((and (>= len 2) (string-ends-with? s "?"))
       (pair (option-or (string-slice s 0 (- len 1)) "") "?"))
      ((= s "*") (pair "_" "*"))
      ((= s "+") (pair "_" "+"))
      ((= s "?") (pair "_" "?"))
      (:else (pair s "1")))))

(df determine-kind [(base Str)] -> Str
  :d "Determines pattern element kind from base token string."
  (cond
    ((or (= base "_") (= base "*")) "wildcard")
    ((= base "any") "any")
    (:else "literal")))

(df compile-single-token-pattern [(raw-tok Str)] -> TokenPattern
  :d "Compiles an individual pattern token string into a TokenPattern record."
  (let [(cap-pair (parse-token-capture raw-tok))
        (cap-grp (.-first cap-pair))
        (sub-tok (.-second cap-pair))
        (quant-pair (extract-quantifier sub-tok))
        (base (.-first quant-pair))
        (quant (.-second quant-pair))
        (kind (determine-kind base))
        (lit (if (= kind "literal") (some base) (none)))]
    (TokenPattern
      :kind kind
      :literal lit
      :quantifier quant
      :capture-group cap-grp)))

(df compile-token-pattern [(pattern-str Str)] -> TokenMatcher
  :d "Compiles token pattern string into TokenMatcher record representation"
  (let [(tokens (tokenize-pattern pattern-str))
        (patterns (map compile-single-token-pattern tokens))]
    (TokenMatcher
      :patterns patterns
      :case-sensitive true)))

(df token-matches-pattern? [(pat TokenPattern) (tok Str) (case-sensitive Bool)] -> Bool
  :d "Tests if a single token satisfies the pattern element constraint."
  (let [(kind (.-kind pat))]
    (if (or (= kind "wildcard") (= kind "any"))
      true
      (let [(lit (option-or (.-literal pat) ""))]
        (if case-sensitive
          (= tok lit)
          (= (string-lower tok) (string-lower lit)))))))

(df count-consecutive-matches [(pat TokenPattern) (tokens (List Str)) (tokens-len I64) (tok-idx I64) (case-sensitive Bool) (acc I64)] -> I64
  :d "Counts maximal consecutive tokens matching pattern element."
  (if (>= tok-idx tokens-len)
    acc
    (let [(tok (option-or (list-get tokens tok-idx) ""))]
      (if (token-matches-pattern? pat tok case-sensitive)
        (count-consecutive-matches pat tokens tokens-len (+ tok-idx 1) case-sensitive (+ acc 1))
        acc))))

(df record-capture [(captures (Map Str (List Str))) (group-name (Option Str)) (tokens (List Str)) (start-idx I64) (count I64)] -> (Map Str (List Str))
  :d "Records captured tokens under the capture group identifier if present."
  (mt group-name
    ((some name)
     (let [(slice (slice-tokens-loop tokens start-idx (+ start-idx count) (list)))
           (existing (option-or (map-get captures name) (list)))]
       (map-set captures name (list-append existing slice))))
    ((none) captures)))

(df try-greedy-k [(matcher TokenMatcher) (tokens (List Str)) (tokens-len I64) (start-idx I64) (pat-idx I64) (tok-idx I64) (pat TokenPattern) (grp (Option Str)) (captures (Map Str (List Str))) (k I64) (min-k I64)] -> (Option (Pair I64 (Map Str (List Str))))
  :d "Tries matching downstream pattern with k consumed tokens, decrementing from max-k to min-k."
  (if (< k min-k)
    (none)
    (let [(next-cap (record-capture captures grp tokens tok-idx k))
          (res (match-patterns-step matcher tokens tokens-len start-idx (+ pat-idx 1) (+ tok-idx k) next-cap))]
      (mt res
        ((some r) (some r))
        ((none) (try-greedy-k matcher tokens tokens-len start-idx pat-idx tok-idx pat grp captures (- k 1) min-k))))))

(df try-nongreedy-k [(matcher TokenMatcher) (tokens (List Str)) (tokens-len I64) (start-idx I64) (pat-idx I64) (tok-idx I64) (pat TokenPattern) (grp (Option Str)) (captures (Map Str (List Str))) (k I64) (max-k I64)] -> (Option (Pair I64 (Map Str (List Str))))
  :d "Tries matching downstream pattern with k consumed tokens, incrementing from min-k to max-k."
  (if (> k max-k)
    (none)
    (let [(next-cap (record-capture captures grp tokens tok-idx k))
          (res (match-patterns-step matcher tokens tokens-len start-idx (+ pat-idx 1) (+ tok-idx k) next-cap))]
      (mt res
        ((some r) (some r))
        ((none) (try-nongreedy-k matcher tokens tokens-len start-idx pat-idx tok-idx pat grp captures (+ k 1) max-k))))))

(df match-patterns-step [(matcher TokenMatcher) (tokens (List Str)) (tokens-len I64) (start-idx I64) (pat-idx I64) (tok-idx I64) (captures (Map Str (List Str)))] -> (Option (Pair I64 (Map Str (List Str))))
  :d "Executes recursive backtracking match across compiled pattern elements."
  (let [(patterns (.-patterns matcher))
        (pat-count (list-length patterns))]
    (if (>= pat-idx pat-count)
      (some (pair (- tok-idx start-idx) captures))
      (let [(pat (option-or (list-get patterns pat-idx) (TokenPattern :kind "" :literal (none) :quantifier "1" :capture-group (none))))
            (quant (.-quantifier pat))
            (grp (.-capture-group pat))
            (case-sens (.-case-sensitive matcher))]
        (cond
          ((= quant "1")
           (if (>= tok-idx tokens-len)
             (none)
             (let [(tok (option-or (list-get tokens tok-idx) ""))]
               (if (token-matches-pattern? pat tok case-sens)
                 (let [(next-cap (record-capture captures grp tokens tok-idx 1))]
                   (match-patterns-step matcher tokens tokens-len start-idx (+ pat-idx 1) (+ tok-idx 1) next-cap))
                 (none)))))
          ((= quant "?")
           (let [(res1 (if (< tok-idx tokens-len)
                         (let [(tok (option-or (list-get tokens tok-idx) ""))]
                           (if (token-matches-pattern? pat tok case-sens)
                             (let [(next-cap (record-capture captures grp tokens tok-idx 1))]
                               (match-patterns-step matcher tokens tokens-len start-idx (+ pat-idx 1) (+ tok-idx 1) next-cap))
                             (none)))
                         (none)))]
             (mt res1
               ((some r) (some r))
               ((none)
                (match-patterns-step matcher tokens tokens-len start-idx (+ pat-idx 1) tok-idx captures)))))
          ((= quant "??")
           (let [(res0 (match-patterns-step matcher tokens tokens-len start-idx (+ pat-idx 1) tok-idx captures))]
             (mt res0
               ((some r) (some r))
               ((none)
                (if (< tok-idx tokens-len)
                  (let [(tok (option-or (list-get tokens tok-idx) ""))]
                    (if (token-matches-pattern? pat tok case-sens)
                      (let [(next-cap (record-capture captures grp tokens tok-idx 1))]
                        (match-patterns-step matcher tokens tokens-len start-idx (+ pat-idx 1) (+ tok-idx 1) next-cap))
                      (none)))
                  (none))))))
          ((= quant "*")
           (let [(max-k (count-consecutive-matches pat tokens tokens-len tok-idx case-sens 0))]
             (try-greedy-k matcher tokens tokens-len start-idx pat-idx tok-idx pat grp captures max-k 0)))
          ((= quant "+")
           (let [(max-k (count-consecutive-matches pat tokens tokens-len tok-idx case-sens 0))]
             (if (< max-k 1)
               (none)
               (try-greedy-k matcher tokens tokens-len start-idx pat-idx tok-idx pat grp captures max-k 1))))
          ((= quant "*?")
           (let [(max-k (count-consecutive-matches pat tokens tokens-len tok-idx case-sens 0))]
             (try-nongreedy-k matcher tokens tokens-len start-idx pat-idx tok-idx pat grp captures 0 max-k)))
          ((= quant "+?")
           (let [(max-k (count-consecutive-matches pat tokens tokens-len tok-idx case-sens 0))]
             (if (< max-k 1)
               (none)
               (try-nongreedy-k matcher tokens tokens-len start-idx pat-idx tok-idx pat grp captures 1 max-k))))
          (:else
           (if (>= tok-idx tokens-len)
             (none)
             (let [(tok (option-or (list-get tokens tok-idx) ""))]
               (if (token-matches-pattern? pat tok case-sens)
                 (let [(next-cap (record-capture captures grp tokens tok-idx 1))]
                   (match-patterns-step matcher tokens tokens-len start-idx (+ pat-idx 1) (+ tok-idx 1) next-cap))
                 (none))))))))))

(df match-token-stream [(matcher TokenMatcher) (tokens (List Str)) (start-idx I64)] -> TokenMatchResult
  :d "Matches token sequence starting at index against TokenMatcher returning match result"
  (let [(tokens-len (list-length tokens))]
    (if (or (< start-idx 0) (> start-idx tokens-len))
      (TokenMatchResult :matched false :tokens-consumed 0 :captures (map-empty))
      (let [(res (match-patterns-step matcher tokens tokens-len start-idx 0 start-idx (map-empty)))]
        (mt res
          ((some pair-res)
           (TokenMatchResult :matched true :tokens-consumed (.-first pair-res) :captures (.-second pair-res)))
          ((none)
           (TokenMatchResult :matched false :tokens-consumed 0 :captures (map-empty))))))))

(df scan-tokens-loop [(matcher TokenMatcher) (tokens (List Str)) (tokens-len I64) (idx I64) (acc (List TokenMatchResult))] -> (List TokenMatchResult)
  :d "Iteratively scans token stream collecting match results."
  (if (>= idx tokens-len)
    acc
    (let [(res (match-token-stream matcher tokens idx))]
      (if (.-matched res)
        (let [(advance (if (> (.-tokens-consumed res) 0) (.-tokens-consumed res) 1))]
          (scan-tokens-loop matcher tokens tokens-len (+ idx advance) (list-append acc (list res))))
        (scan-tokens-loop matcher tokens tokens-len (+ idx 1) acc)))))

(df scan-token-stream [(matcher TokenMatcher) (tokens (List Str))] -> (List TokenMatchResult)
  :d "Scans entire token stream returning all non-overlapping token match results"
  (let [(tokens-len (list-length tokens))]
    (scan-tokens-loop matcher tokens tokens-len 0 (list))))

(df find-subseqs-loop [(matcher TokenMatcher) (tokens (List Str)) (tokens-len I64) (idx I64) (acc (List (Pair I64 I64)))] -> (List (Pair I64 I64))
  :d "Iteratively scans token stream finding matching subsequence index spans."
  (if (>= idx tokens-len)
    acc
    (let [(res (match-token-stream matcher tokens idx))]
      (if (.-matched res)
        (let [(consumed (.-tokens-consumed res))
              (advance (if (> consumed 0) consumed 1))
              (span (pair idx (+ idx consumed)))]
          (find-subseqs-loop matcher tokens tokens-len (+ idx advance) (list-append acc (list span))))
        (find-subseqs-loop matcher tokens tokens-len (+ idx 1) acc)))))

(df find-token-subsequences [(matcher TokenMatcher) (tokens (List Str))] -> (List (Pair I64 I64))
  :d "Finds start and end index pairs of matching token subsequences in stream"
  (let [(tokens-len (list-length tokens))]
    (find-subseqs-loop matcher tokens tokens-len 0 (list))))
