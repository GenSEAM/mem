(module asl-mem/structural-editor
  :d "Structural AST node transformer and balanced S-expression rewriting engine"
  :x [ASTTransform
      VFSBuffer
      ASTMatchNode
      rewrite-ast-node]
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

(dfs ASTMatchNode
  (:f form-type Str "Matched form type keyword")
  (:f symbol-id Str "Matched symbol identifier")
  (:f start-char I64 "Start character offset in buffer content")
  (:f end-char I64 "End character offset in buffer content")
  (:f raw-form Str "Verbatim matched S-expression text slice")
  (:f line-number I64 "1-indexed line number where the form begins"))

(dfs ASTTransform
  (:f op Str "Mutation operation: :rename-symbol, :replace-body, :add-field, :wrap-with-assert, :remove-form")
  (:f target-symbol Str "Identifier of target to modify or reference")
  (:f replacement Str "New symbol name, body expression, or field form text")
  (:f field-type (Option Str) "Field type annotation when adding a field to dfs"))

(dfs DelimAcc
  (:f p-depth I64 "Paren nesting depth")
  (:f b-depth I64 "Bracket nesting depth")
  (:f in-str Bool "Inside string literal")
  (:f esc Bool "Escape flag")
  (:f valid Bool "Balance validity flag"))

(df delim-step-string [(acc DelimAcc) (ch Str)] -> DelimAcc
  :d "Processes character inside string literal for delimiter check."
  (let [(p (.-p-depth acc))
        (b (.-b-depth acc))
        (esc (.-esc acc))]
    (cond
      (esc (DelimAcc :p-depth p :b-depth b :in-str true :esc false :valid true))
      ((= ch "\\") (DelimAcc :p-depth p :b-depth b :in-str true :esc true :valid true))
      ((= ch "\"") (DelimAcc :p-depth p :b-depth b :in-str false :esc false :valid true))
      (:else acc))))

(df delim-step-paren [(acc DelimAcc) (ch Str)] -> DelimAcc
  :d "Handles parenthesis delimiter."
  (let [(p (.-p-depth acc))
        (b (.-b-depth acc))]
    (cond
      ((= ch "(") (DelimAcc :p-depth (+ p 1) :b-depth b :in-str false :esc false :valid true))
      ((<= p 0) (DelimAcc :p-depth -1 :b-depth b :in-str false :esc false :valid false))
      (:else (DelimAcc :p-depth (- p 1) :b-depth b :in-str false :esc false :valid true)))))

(df delim-step-bracket [(acc DelimAcc) (ch Str)] -> DelimAcc
  :d "Handles bracket delimiter."
  (let [(p (.-p-depth acc))
        (b (.-b-depth acc))]
    (cond
      ((= ch "[") (DelimAcc :p-depth p :b-depth (+ b 1) :in-str false :esc false :valid true))
      ((<= b 0) (DelimAcc :p-depth p :b-depth -1 :in-str false :esc false :valid false))
      (:else (DelimAcc :p-depth p :b-depth (- b 1) :in-str false :esc false :valid true)))))

(df delim-step-code [(acc DelimAcc) (ch Str)] -> DelimAcc
  :d "Processes code character outside string."
  (cond
    ((= ch "\"") (DelimAcc :p-depth (.-p-depth acc) :b-depth (.-b-depth acc) :in-str true :esc false :valid true))
    ((or (= ch "(") (= ch ")")) (delim-step-paren acc ch))
    ((or (= ch "[") (= ch "]")) (delim-step-bracket acc ch))
    (:else acc)))

(df delim-step [(acc DelimAcc) (ch Str)] -> DelimAcc
  :d "Accumulates delimiter balance character by character."
  (cond
    ((not (.-valid acc)) acc)
    ((.-in-str acc) (delim-step-string acc ch))
    (:else (delim-step-code acc ch))))

(df is-balanced-delimiters? [(text Str)] -> Bool
  :d "Checks delimiter balance ensuring total open delimiters match closing delimiters."
  (let [(chars (string-chars text))
        (init (DelimAcc :p-depth 0 :b-depth 0 :in-str false :esc false :valid true))
        (res (fold delim-step init chars))]
    (and (.-valid res)
         (and (= (.-p-depth res) 0)
              (and (= (.-b-depth res) 0)
                   (not (.-in-str res)))))))

(df char-to-code [(ch Str)] -> I64
  :d "Maps single character to deterministic integer code."
  (let [(charset "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ !\"#$%&'()*+,-./:<=>?@[\\]^_`{|}~\t\n\r")]
    (mt (string-index-of charset ch)
      ((none) 127)
      ((some idx) (+ idx 32)))))

(df compute-cas-hash [(content Str)] -> Str
  :d "Computes deterministic content-addressed hash of buffer text payload."
  (let [(chars (string-chars content))
        (len (string-length content))
        (h (fold (fn [(acc I64) (ch Str)] -> I64
                   (mod (+ (* acc 31) (char-to-code ch)) 2147483647))
                 5381
                 chars))]
    (str "cas-" (string-from-int64 h) "-" (string-from-int64 len))))

(df string-replace-first [(text Str) (target Str) (replacement Str)] -> Str
  :d "Replaces first occurrence of target with replacement in text."
  (if (or (string-empty? target) (string-empty? text))
    text
    (mt (string-index-of text target)
      ((none) text)
      ((some idx)
       (let [(pre (option-or (string-slice text 0 idx) ""))
             (post-start (+ idx (string-length target)))
             (post (option-or (string-slice text post-start (string-length text)) ""))]
         (str pre replacement post))))))

(df find-last-paren-idx [(chars (List Str)) (idx I64) (best I64)] -> I64
  :d "Finds character index of last closing parenthesis in list."
  (if (>= idx (list-length chars))
    best
    (let [(c (option-or (list-get chars idx) ""))
          (next-best (if (= c ")") idx best))]
      (find-last-paren-idx chars (+ idx 1) next-best))))

(df mutate-rename-symbol [(raw Str) (node ASTMatchNode) (m ASTTransform)] -> Str
  :d "Renames symbol identifier in form."
  (let [(target (if (string-empty? (.-target-symbol m))
                  (.-symbol-id node)
                  (.-target-symbol m)))]
    (string-replace-first raw target (.-replacement m))))

(df find-docstring-end [(text Str) (start I64)] -> (Option I64)
  :d "Finds end quote of docstring starting at given index."
  (let [(chars (string-chars text))
        (len (list-length chars))]
    (mt (string-index-of (option-or (string-slice text start len) "") "\"")
      ((none) (none))
      ((some q1-rel)
       (let [(q1 (+ start (+ q1-rel 1)))]
         (mt (string-index-of (option-or (string-slice text q1 len) "") "\"")
           ((none) (none))
           ((some q2-rel) (some (+ q1 (+ q2-rel 1))))))))))

(df replace-body-fallback [(raw Str) (repl Str)] -> Str
  :d "Replaces body using last parenthesis boundary."
  (let [(chars (string-chars raw))
        (last-p (find-last-paren-idx chars 0 -1))]
    (if (> last-p 0)
      (str (option-or (string-slice raw 0 last-p) "") "\n  " repl "\n)")
      raw)))

(df replace-body-post-bracket [(raw Str) (br-idx I64) (repl Str)] -> Str
  :d "Replaces body following parameter bracket vector."
  (let [(post-br (option-or (string-slice raw (+ br-idx 1) (string-length raw)) ""))]
    (mt (string-index-of post-br "->")
      ((none)
       (str (option-or (string-slice raw 0 (+ br-idx 1)) "") "\n  " repl "\n)"))
      ((some arr-idx)
       (let [(after-arr (option-or (string-slice post-br (+ arr-idx 2) (string-length post-br)) ""))
             (words (string-split (string-trim after-arr) " "))
             (ret-type (option-or (list-get words 0) ""))
             (cut-len (+ (+ br-idx 1) (+ (+ arr-idx 2) (+ (string-length ret-type) 1))))]
         (str (option-or (string-slice raw 0 cut-len) "") "\n  " repl "\n)"))))))

(df replace-body-after-doc [(raw Str) (d-idx I64) (repl Str)] -> Str
  :d "Replaces body following docstring."
  (mt (find-docstring-end raw d-idx)
    ((some doc-end)
     (let [(prefix (option-or (string-slice raw 0 doc-end) ""))]
       (str prefix "\n  " repl "\n)")))
    ((none)
     (replace-body-fallback raw repl))))

(df replace-body-no-doc [(raw Str) (repl Str)] -> Str
  :d "Replaces body when no docstring is present."
  (mt (string-index-of raw "]")
    ((some br-idx) (replace-body-post-bracket raw br-idx repl))
    ((none) (replace-body-fallback raw repl))))

(df mutate-replace-body [(raw Str) (m ASTTransform)] -> Str
  :d "Replaces body of function or form."
  (let [(repl (.-replacement m))]
    (mt (string-index-of raw ":d ")
      ((some d-idx) (replace-body-after-doc raw d-idx repl))
      ((none) (replace-body-no-doc raw repl)))))

(df mutate-add-field [(raw Str) (m ASTTransform)] -> Str
  :d "Appends new field to record definition before closing parenthesis."
  (let [(chars (string-chars raw))
        (last-p (find-last-paren-idx chars 0 -1))
        (repl (.-replacement m))]
    (if (<= last-p 0)
      raw
      (let [(prefix (option-or (string-slice raw 0 last-p) ""))
            (field-str (if (or (string-starts-with? repl "(:f") (string-starts-with? repl "("))
                         (str "  " repl "\n")
                         (let [(ft (option-or (.-field-type m) "Str"))]
                           (str "  (:f " repl " " ft " \"" repl " field\")\n"))))]
        (str prefix field-str ")")))))

(df normalize-mutation-op [(op Str)] -> Str
  :d "Strips leading colon from mutation operation keyword."
  (if (string-starts-with? op ":")
    (option-or (string-slice op 1 (string-length op)) "")
    op))

(df apply-ast-mutation [(raw Str) (node ASTMatchNode) (m ASTTransform)] -> Str
  :d "Applies targeted structural AST mutation to raw form."
  (let [(op (normalize-mutation-op (.-op m)))]
    (cond
      ((= op "rename-symbol") (mutate-rename-symbol raw node m))
      ((= op "replace-body") (mutate-replace-body raw m))
      ((= op "add-field") (mutate-add-field raw m))
      ((= op "wrap-with-assert") (str "(assert " raw " \"" (.-replacement m) "\")"))
      ((= op "remove-form") "")
      (:else raw))))

(df rewrite-ast-node [(buffer VFSBuffer) (node ASTMatchNode) (mutation ASTTransform)] -> VFSBuffer
  :d "Rewrites matched AST node within buffer ensuring delimiter balance and CAS integrity."
  (let [(raw (.-raw-form node))
        (mutated (apply-ast-mutation raw node mutation))
        (content (.-content buffer))
        (c-len (string-length content))
        (start-c (.-start-char node))
        (end-c (.-end-char node))]
    (if (and (not (string-empty? mutated)) (not (is-balanced-delimiters? mutated)))
      buffer
      (let [(pre (option-or (string-slice content 0 start-c) ""))
            (post (option-or (string-slice content end-c c-len) ""))
            (new-content (str pre mutated post))
            (new-hash (compute-cas-hash new-content))
            (base-h (.-base-hash buffer))
            (is-dirty (!= new-hash base-h))]
        (VFSBuffer
          :path (.-path buffer)
          :content new-content
          :base-content (.-base-content buffer)
          :cas-hash new-hash
          :base-hash base-h
          :revision (+ (.-revision buffer) 1)
          :dirty is-dirty
          :loaded-at (.-loaded-at buffer))))))
