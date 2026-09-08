(module asl-mem/ast-filter
  :d "Structural AST pattern matching and S-expression filter engine"
  :x [ASTQuery
      ASTMatchNode
      VFSBuffer
      match-ast-form]
  :i [])

(dfs VFSBuffer
  (:f path Str "Normalized virtual file path")
  (:f content Str "Buffer text payload")
  (:f base-content Str "Pristine or staged base content for diff computation")
  (:f cas-hash Str "Deterministic content-addressed hash of current content")
  (:f base-hash Str "CAS hash of pristine or staged base version")
  (:f revision I64 "Monotonic revision counter")
  (:f dirty Bool "Boolean flag indicating uncommitted edits relative to base")
  (:f loaded-at I64 "Millisecond timestamp when buffer was opened or created"))

(dfs ASTQuery
  (:f form-type Str "Target form type keyword: dfs, df, dfe, module, import, or * for any")
  (:f symbol-id (Option Str) "Specific symbol identifier to filter or none for any")
  (:f param-count (Option I64) "Optional parameter count constraint for df/dfe")
  (:f predicate-token (Option Str) "Optional sub-token or body text substring match"))

(dfs ASTMatchNode
  (:f form-type Str "Matched form type keyword")
  (:f symbol-id Str "Matched symbol identifier")
  (:f start-char I64 "Start character offset in buffer content")
  (:f end-char I64 "End character offset in buffer content")
  (:f raw-form Str "Verbatim matched S-expression text slice")
  (:f line-number I64 "1-indexed line number where the form begins"))

(dfs MatchParsed
  (:f node ASTMatchNode "Parsed AST match node")
  (:f p-cnt (Option I64) "Optional parameter count"))

(dfs RawSpan
  (:f start-idx I64 "Start char offset")
  (:f end-idx I64 "End char offset")
  (:f line-num I64 "1-indexed line number"))

(dfs ScanAcc
  (:f idx I64 "Current character index")
  (:f line I64 "Current line number")
  (:f in-str Bool "Inside string literal")
  (:f esc Bool "Next character escaped")
  (:f in-cmt Bool "Inside comment line")
  (:f depth I64 "Delimiter nesting depth")
  (:f form-start I64 "Start offset of active form")
  (:f form-line I64 "Start line of active form")
  (:f spans (List RawSpan) "Accumulated completed top-level form spans"))

(df is-space-char [(ch Str)] -> Bool
  :d "Returns true if single-character string is whitespace."
  (cond
    ((= ch " ") true)
    ((= ch "\t") true)
    ((= ch "\n") true)
    ((= ch "\r") true)
    (:else false)))

(df scan-step-comment [(acc ScanAcc) (ch Str)] -> ScanAcc
  :d "Advances scanner within a comment line."
  (let [(idx (.-idx acc))
        (line (.-line acc))
        (depth (.-depth acc))
        (f-start (.-form-start acc))
        (f-line (.-form-line acc))
        (spans (.-spans acc))]
    (cond
      ((= ch "\n") (ScanAcc :idx (+ idx 1) :line (+ line 1) :in-str false :esc false :in-cmt false :depth depth :form-start f-start :form-line f-line :spans spans))
      (:else (ScanAcc :idx (+ idx 1) :line line :in-str false :esc false :in-cmt true :depth depth :form-start f-start :form-line f-line :spans spans)))))

(df scan-step-string [(acc ScanAcc) (ch Str)] -> ScanAcc
  :d "Advances scanner within a string literal."
  (let [(idx (.-idx acc))
        (line (.-line acc))
        (esc (.-esc acc))
        (depth (.-depth acc))
        (f-start (.-form-start acc))
        (f-line (.-form-line acc))
        (spans (.-spans acc))]
    (cond
      (esc (ScanAcc :idx (+ idx 1) :line line :in-str true :esc false :in-cmt false :depth depth :form-start f-start :form-line f-line :spans spans))
      ((= ch "\\") (ScanAcc :idx (+ idx 1) :line line :in-str true :esc true :in-cmt false :depth depth :form-start f-start :form-line f-line :spans spans))
      ((= ch "\"") (ScanAcc :idx (+ idx 1) :line line :in-str false :esc false :in-cmt false :depth depth :form-start f-start :form-line f-line :spans spans))
      (:else (ScanAcc :idx (+ idx 1) :line line :in-str true :esc false :in-cmt false :depth depth :form-start f-start :form-line f-line :spans spans)))))

(df scan-step-delim-open [(acc ScanAcc) (ch Str)] -> ScanAcc
  :d "Handles opening delimiter character."
  (let [(idx (.-idx acc))
        (line (.-line acc))
        (depth (.-depth acc))
        (f-start (.-form-start acc))
        (f-line (.-form-line acc))
        (spans (.-spans acc))
        (is-new (= depth 0))
        (next-start (if is-new idx f-start))
        (next-line (if is-new line f-line))]
    (ScanAcc :idx (+ idx 1) :line line :in-str false :esc false :in-cmt false :depth (+ depth 1) :form-start next-start :form-line next-line :spans spans)))

(df scan-step-delim-close [(acc ScanAcc) (ch Str)] -> ScanAcc
  :d "Handles closing delimiter character."
  (let [(idx (.-idx acc))
        (line (.-line acc))
        (depth (.-depth acc))
        (f-start (.-form-start acc))
        (f-line (.-form-line acc))
        (spans (.-spans acc))
        (next-d (if (> depth 0) (- depth 1) 0))
        (closed (and (= depth 1) (>= f-start 0)))
        (new-span (RawSpan :start-idx f-start :end-idx (+ idx 1) :line-num f-line))
        (next-s (if closed (list-append spans (list new-span)) spans))
        (next-start (if closed -1 f-start))]
    (ScanAcc :idx (+ idx 1) :line line :in-str false :esc false :in-cmt false :depth next-d :form-start next-start :form-line f-line :spans next-s)))

(df is-open-delim? [(ch Str)] -> Bool
  :d "Returns true if ch is an open delimiter."
  (cond
    ((= ch "(") true)
    ((= ch "[") true)
    ((= ch "{") true)
    (:else false)))

(df is-close-delim? [(ch Str)] -> Bool
  :d "Returns true if ch is a closing delimiter."
  (cond
    ((= ch ")") true)
    ((= ch "]") true)
    ((= ch "}") true)
    (:else false)))

(df scan-step-normal [(acc ScanAcc) (ch Str)] -> ScanAcc
  :d "Processes regular character in code stream."
  (let [(idx (.-idx acc))
        (line (.-line acc))
        (depth (.-depth acc))
        (f-start (.-form-start acc))
        (f-line (.-form-line acc))
        (spans (.-spans acc))]
    (cond
      ((= ch ";") (ScanAcc :idx (+ idx 1) :line line :in-str false :esc false :in-cmt true :depth depth :form-start f-start :form-line f-line :spans spans))
      ((= ch "\"") (ScanAcc :idx (+ idx 1) :line line :in-str true :esc false :in-cmt false :depth depth :form-start f-start :form-line f-line :spans spans))
      ((= ch "\n") (ScanAcc :idx (+ idx 1) :line (+ line 1) :in-str false :esc false :in-cmt false :depth depth :form-start f-start :form-line f-line :spans spans))
      ((is-open-delim? ch) (scan-step-delim-open acc ch))
      ((is-close-delim? ch) (scan-step-delim-close acc ch))
      (:else (ScanAcc :idx (+ idx 1) :line line :in-str false :esc false :in-cmt false :depth depth :form-start f-start :form-line f-line :spans spans)))))

(df scan-char-step [(acc ScanAcc) (ch Str)] -> ScanAcc
  :d "Processes single character updating scanner state and isolating top-level forms."
  (cond
    ((.-in-cmt acc) (scan-step-comment acc ch))
    ((.-in-str acc) (scan-step-string acc ch))
    (:else (scan-step-normal acc ch))))

(df scan-top-level-spans [(content Str)] -> (List RawSpan)
  :d "Scans buffer text isolating top-level S-expression character boundaries and line numbers."
  (let [(chars (string-chars content))
        (init-acc (ScanAcc
                    :idx 0
                    :line 1
                    :in-str false
                    :esc false
                    :in-cmt false
                    :depth 0
                    :form-start -1
                    :form-line 1
                    :spans (list)))
        (final-acc (fold (fn [(acc ScanAcc) (ch Str)] -> ScanAcc (scan-char-step acc ch)) init-acc chars))]
    (.-spans final-acc)))

(dfs HeaderAcc
  (:f idx I64 "Current char index")
  (:f cur-token Str "Active accumulated token")
  (:f tokens (List Str) "Extracted tokens")
  (:f max-tokens I64 "Maximum tokens to collect"))

(df header-char-step [(acc HeaderAcc) (ch Str)] -> HeaderAcc
  :d "Accumulates header tokens up to max limit."
  (let [(tokens (.-tokens acc))
        (max-tok (.-max-tokens acc))]
    (if (>= (list-length tokens) max-tok)
      acc
      (let [(idx (.-idx acc))
            (cur (.-cur-token acc))]
        (if (or (is-space-char ch) (or (= ch "(") (or (= ch ")") (or (= ch "[") (= ch "]")))))
          (if (string-empty? cur)
            (HeaderAcc :idx (+ idx 1) :cur-token "" :tokens tokens :max-tokens max-tok)
            (HeaderAcc :idx (+ idx 1) :cur-token "" :tokens (list-append tokens (list cur)) :max-tokens max-tok))
          (HeaderAcc :idx (+ idx 1) :cur-token (str cur ch) :tokens tokens :max-tokens max-tok))))))

(df extract-header-tokens [(raw Str) (max-tokens I64)] -> (List Str)
  :d "Extracts first N whitespace-separated tokens from raw form S-expression."
  (let [(chars (string-chars raw))
        (init (HeaderAcc :idx 0 :cur-token "" :tokens (list) :max-tokens max-tokens))
        (res (fold (fn [(acc HeaderAcc) (ch Str)] -> HeaderAcc (header-char-step acc ch)) init chars))
        (rem (.-cur-token res))
        (toks (.-tokens res))]
    (if (and (not (string-empty? rem)) (< (list-length toks) max-tokens))
      (list-append toks (list rem))
      toks)))

(dfs ParamCountAcc
  (:f depth I64 "Bracket nesting depth")
  (:f in-bracket Bool "Inside parameter list")
  (:f in-item Bool "Currently accumulating item")
  (:f count I64 "Total parameter count")
  (:f stopped Bool "Completed parameter vector"))

(df param-step-outside [(acc ParamCountAcc) (ch Str)] -> ParamCountAcc
  :d "Handles characters before parameter vector starts."
  (if (= ch "[")
    (ParamCountAcc :depth 1 :in-bracket true :in-item false :count 0 :stopped false)
    acc))

(df param-step-close-bracket [(acc ParamCountAcc)] -> ParamCountAcc
  :d "Handles closing square bracket."
  (let [(depth (.-depth acc))
        (cnt (.-count acc))]
    (if (<= depth 1)
      (ParamCountAcc :depth 0 :in-bracket false :in-item false :count cnt :stopped true)
      (ParamCountAcc :depth (- depth 1) :in-bracket true :in-item (.-in-item acc) :count cnt :stopped false))))

(df param-step-paren [(acc ParamCountAcc) (ch Str)] -> ParamCountAcc
  :d "Handles parenthesis delimiters inside parameter vector."
  (let [(depth (.-depth acc))
        (cnt (.-count acc))]
    (if (= ch "(")
      (let [(next-cnt (if (= depth 1) (+ cnt 1) cnt))]
        (ParamCountAcc :depth (+ depth 1) :in-bracket true :in-item false :count next-cnt :stopped false))
      (let [(next-d (if (> depth 1) (- depth 1) depth))]
        (ParamCountAcc :depth next-d :in-bracket true :in-item false :count cnt :stopped false)))))

(df param-step-item [(acc ParamCountAcc) (ch Str)] -> ParamCountAcc
  :d "Handles item tokens at depth 1."
  (let [(depth (.-depth acc))
        (in-item (.-in-item acc))
        (cnt (.-count acc))]
    (if (is-space-char ch)
      (ParamCountAcc :depth depth :in-bracket true :in-item false :count cnt :stopped false)
      (if in-item
        acc
        (ParamCountAcc :depth depth :in-bracket true :in-item true :count (+ cnt 1) :stopped false)))))

(df param-step-inside [(acc ParamCountAcc) (ch Str)] -> ParamCountAcc
  :d "Dispatches character processing inside parameter vector."
  (let [(depth (.-depth acc))]
    (cond
      ((= ch "[") (ParamCountAcc :depth (+ depth 1) :in-bracket true :in-item (.-in-item acc) :count (.-count acc) :stopped false))
      ((= ch "]") (param-step-close-bracket acc))
      ((or (= ch "(") (= ch ")")) (param-step-paren acc ch))
      ((= depth 1) (param-step-item acc ch))
      (:else acc))))

(df param-count-step [(acc ParamCountAcc) (ch Str)] -> ParamCountAcc
  :d "Scans parameter vector characters and tallies parameter items."
  (cond
    ((.-stopped acc) acc)
    ((not (.-in-bracket acc)) (param-step-outside acc ch))
    (:else (param-step-inside acc ch))))

(df count-params-in-form [(raw Str)] -> (Option I64)
  :d "Counts parameters declared inside the first bracket vector of a form."
  (let [(chars (string-chars raw))
        (init (ParamCountAcc :depth 0 :in-bracket false :in-item false :count 0 :stopped false))
        (res (fold (fn [(acc ParamCountAcc) (ch Str)] -> ParamCountAcc (param-count-step acc ch)) init chars))]
    (if (.-stopped res)
      (some (.-count res))
      (none))))

(df normalize-kw [(val Str)] -> Str
  :d "Strips leading colon from keyword string if present."
  (if (string-starts-with? val ":")
    (option-or (string-slice val 1 (string-length val)) "")
    val))

(df param-count-matches? [(query-pc (Option I64)) (actual-pc (Option I64))] -> Bool
  :d "Checks if query parameter count matches actual count."
  (mt query-pc
    ((none) true)
    ((some target)
     (mt actual-pc
       ((none) false)
       ((some actual) (= target actual))))))

(df form-matches-query? [(node ASTMatchNode) (p-count (Option I64)) (query ASTQuery)] -> Bool
  :d "Determines if matched node satisfies ASTQuery criteria."
  (let [(q-ft (normalize-kw (.-form-type query)))
        (n-ft (.-form-type node))
        (ft-ok (or (= q-ft "*") (= q-ft n-ft)))
        (sym-ok (mt (.-symbol-id query)
                  ((none) true)
                  ((some s) (= (normalize-kw s) (.-symbol-id node)))))
        (pc-ok (param-count-matches? (.-param-count query) p-count))
        (pred-ok (mt (.-predicate-token query)
                   ((none) true)
                   ((some tok) (string-contains? (.-raw-form node) tok))))]
    (and ft-ok (and sym-ok (and pc-ok pred-ok)))))

(df parse-match-from-span [(span RawSpan) (content Str)] -> MatchParsed
  :d "Extracts ASTMatchNode and parameter count from raw span."
  (let [(start-c (.-start-idx span))
        (end-c (.-end-idx span))
        (raw (option-or (string-slice content start-c end-c) ""))
        (toks (extract-header-tokens raw 5))
        (raw-t0 (option-or (list-get toks 0) ""))
        (t0 (normalize-kw raw-t0))
        (t1 (option-or (list-get toks 1) ""))
        (t2 (option-or (list-get toks 2) ""))
        (is-effectful (and (= t0 "df") (= t1 "!")))
        (sym-id (if is-effectful t2 t1))
        (p-cnt (if (or (= t0 "df") (= t0 "dfe"))
                 (count-params-in-form raw)
                 (none)))
        (node (ASTMatchNode
                :form-type t0
                :symbol-id sym-id
                :start-char start-c
                :end-char end-c
                :raw-form raw
                :line-number (.-line-num span)))]
    (MatchParsed :node node :p-cnt p-cnt)))

(df match-ast-form [(buffer VFSBuffer) (query ASTQuery)] -> (List ASTMatchNode)
  :d "Filters top-level S-expressions from buffer matching ASTQuery constraints."
  (let [(content (.-content buffer))
        (spans (scan-top-level-spans content))]
    (fold (fn [(acc (List ASTMatchNode)) (span RawSpan)] -> (List ASTMatchNode)
            (let [(parsed (parse-match-from-span span content))
                  (node (.-node parsed))
                  (p-cnt (.-p-cnt parsed))]
              (if (form-matches-query? node p-cnt query)
                (list-append acc (list node))
                acc)))
          (list)
          spans)))
