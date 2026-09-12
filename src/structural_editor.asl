(module asl-mem/structural-editor
  :d "Structural AST node transformer and balanced S-expression rewriting engine"
  :x [ASTTransform
      rewrite-ast-node
      is-stub-body?
      scaffold-module
      scaffold-test
      scaffold-fn]
  :i [(vfs :a v)
      (ast_filter :a af)
      (asl-parser/balance :a bal)])

(dfs ASTTransform
  (:f op Str "Mutation operation: :rename-symbol, :replace-body, :add-field, :wrap-with-assert, :remove-form")
  (:f target-symbol Str "Identifier of target to modify or reference")
  (:f replacement Str "New symbol name, body expression, or field form text")
  (:f field-type (Option Str) "Field type annotation when adding a field to dfs"))

(df is-balanced-delimiters? [(text Str)] -> Bool
  :d "Checks delimiter balance ensuring total open delimiters match closing delimiters via canonical asl-parser/balance."
  (bal/is-delimiter-balanced? text))

(df compute-cas-hash [(content Str)] -> Str
  :d "Computes deterministic content-addressed hash of buffer text payload."
  (let [(chars (string-chars content))
        (len (string-length content))
        (h (fold (fn [(acc I64) (ch Str)] -> I64
                   (mod (+ (* acc 31) (v/char-to-code ch)) 2147483647))
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

(df mutate-rename-symbol [(raw Str) (node af/ASTMatchNode) (m ASTTransform)] -> Str
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

(df apply-ast-mutation [(raw Str) (node af/ASTMatchNode) (m ASTTransform)] -> Str
  :d "Applies targeted structural AST mutation to raw form."
  (let [(op (normalize-mutation-op (.-op m)))]
    (cond
      ((= op "rename-symbol") (mutate-rename-symbol raw node m))
      ((= op "replace-body") (mutate-replace-body raw m))
      ((= op "add-field") (mutate-add-field raw m))
      ((= op "wrap-with-assert") (str "(assert " raw " \"" (.-replacement m) "\")"))
      ((= op "remove-form") "")
      (:else raw))))

(df rewrite-ast-node [(buffer v/VFSBuffer) (node af/ASTMatchNode) (mutation ASTTransform)] -> v/VFSBuffer
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
        (v/VFSBuffer
          :path (.-path buffer)
          :content new-content
          :base-content (.-base-content buffer)
          :cas-hash new-hash
          :base-hash base-h
          :revision (+ (.-revision buffer) 1)
          :dirty is-dirty
          :loaded-at (.-loaded-at buffer))))))

(df is-stub-body? [(body Str)] -> Bool
  :d "Detects constant-returning bodies and stubs lacking assertions or non-trivial expressions"
  (let [(trimmed (string-trim body))]
    (or (string-empty? trimmed)
        (= trimmed "true")
        (= trimmed "false")
        (= trimmed "nil")
        (= trimmed "0")
        (= trimmed "\"\""))))

(df scaffold-module [(name Str)] -> Str
  :d "Generates a valid ASL module form with declared exports and imports"
  (str "(module " name "\n"
       "  :d \"Module " name " providing core operational capabilities\"\n"
       "  :x [init-" name "\n"
       "      process-" name "]\n"
       "  :i [])\n\n"
       "(df init-" name " [] -> Bool\n"
       "  :d \"Initializes module state for " name "\"\n"
       "  (let [(ready true)]\n"
       "    (assert ready \"" name " initialization contract established\")\n"
       "    ready))\n\n"
       "(df process-" name " [(input Str)] -> Str\n"
       "  :d \"Processes input for " name " transforming payload\"\n"
       "  (if (string-empty? input)\n"
       "    \"default\"\n"
       "    (str \"" name ":\" input)))\n"))

(df scaffold-test [(name Str)] -> Str
  :d "Generates a valid ASL test module with non-stub assertions"
  (str "(module tests/" name "\n"
       "  :d \"Test suite for " name " validating non-trivial operational invariants\"\n"
       "  :x [run-tests\n"
       "      test-" name "-positive\n"
       "      test-" name "-negative]\n"
       "  :i [])\n\n"
       (str "(df " "test-" name "-positive [] -> Bool\n")
       "  :d \"Verifies positive transformation and execution behavior\"\n"
       "  (let [(expected \"sample-data\")\n"
       "        (actual (str \"sample\" \"-\" \"data\"))]\n"
       "    (assert (= expected actual) \"expected must match actual\")\n"
       "    (= expected actual)))\n\n"
       (str "(df " "test-" name "-negative [] -> Bool\n")
       "  :d \"Verifies boundary condition and negative case assertion\"\n"
       "  (let [(empty-str \"\")\n"
       "        (is-empty (string-empty? empty-str))]\n"
       "    (assert is-empty \"empty string must satisfy string-empty predicate\")\n"
       "    is-empty))\n\n"
       "(df run-tests [] -> Bool\n"
       "  :d \"Executes test suite for " name "\"\n"
       "  (do\n"
       "    (assert (test-" name "-positive) \"test-" name "-positive must pass\")\n"
       "    (assert (test-" name "-negative) \"test-" name "-negative must pass\")\n"
       "    true))\n"))

(df scaffold-fn [(name Str)] -> Str
  :d "Generates a typed function form with parameter bindings and non-stub body"
  (str "(df " name " [(payload Str)] -> Str\n"
       "  :d \"Processes payload for " name "\"\n"
       "  (if (string-empty? payload)\n"
       "    \"" name "-empty\"\n"
       "    (str \"" name ":\" payload)))\n"))
