(module asl-mem/pointer-orchestrator
  :d "Perceptual pointer orchestration, on-demand expansion, and minimal context boundary enforcement"
  :x [PointerSpec
      make-pointer-spec
      deref-pointer
      collapse-pointer
      orchestrate-context-pointers
      format-pointer-spec
      parse-pointer-spec
      total-active-tokens
      is-context-minimal?]
  :i [])

(dfs PointerSpec
  (:f id Str "Unique perceptual pointer identifier")
  (:f kind Str "Perceptual kind: sym, file, or chunk")
  (:f target Str "Target symbol identifier, file path, or memory chunk id")
  (:f tokens I64 "Compact pointer token footprint")
  (:f expanded-tokens I64 "Full dereferenced token count when expanded")
  (:f is-expanded Bool "Expansion status flag")
  (:f summary Str "Compact semantic summary for coordinator attention")
  (:f payload Str "Dereferenced full text content when expanded, or empty string"))

(df make-pointer-spec [(id Str) (kind Str) (target Str) (tokens I64) (summary Str)] -> PointerSpec
  :d "Constructs a compact perceptual pointer specification record"
  (PointerSpec
    :id id
    :kind kind
    :target target
    :tokens tokens
    :expanded-tokens 0
    :is-expanded false
    :summary summary
    :payload ""))

(df deref-pointer [(ptr PointerSpec) (full-content Str)] -> PointerSpec
  :d "Expands pointer on-demand with dereferenced payload and calculated token weight"
  (let [(len (string-length full-content))
        (toks (/ len 4))]
    (PointerSpec
      :id (.-id ptr)
      :kind (.-kind ptr)
      :target (.-target ptr)
      :tokens (.-tokens ptr)
      :expanded-tokens toks
      :is-expanded true
      :summary (.-summary ptr)
      :payload full-content)))

(df collapse-pointer [(ptr PointerSpec)] -> PointerSpec
  :d "Collapses pointer back to compact reference resetting active footprint to base tokens"
  (PointerSpec
    :id (.-id ptr)
    :kind (.-kind ptr)
    :target (.-target ptr)
    :tokens (.-tokens ptr)
    :expanded-tokens (.-expanded-tokens ptr)
    :is-expanded false
    :summary (.-summary ptr)
    :payload ""))

(df extract-between [(text Str) (prefix Str) (suffix Str)] -> (Option Str)
  :d "Extracts substring between prefix and first subsequent suffix"
  (mt (string-index-of text prefix)
    ((none) (none))
    ((some p-idx)
     (let [(start (+ p-idx (string-length prefix)))
           (sub (option-or (string-slice text start (string-length text)) ""))]
       (mt (string-index-of sub suffix)
         ((none) (none))
         ((some s-idx)
          (string-slice sub 0 s-idx)))))))

(df format-pointer-spec [(ptr PointerSpec)] -> Str
  :d "Formats PointerSpec into canonical compact S-expression pointer representation"
  (let [(tok-s (string-from-int64 (.-tokens ptr)))
        (exp-s (if (.-is-expanded ptr) "true" "false"))
        (p1 (str "(:ptr :kind \"" (.-kind ptr) "\" :id \"" (.-id ptr) "\" :target \"" (.-target ptr) "\""))
        (p2 (str " :tokens " tok-s " :expanded " exp-s " :summary \"" (.-summary ptr) "\")"))]
    (str p1 p2)))

(df parse-pointer-spec [(line Str)] -> (Option PointerSpec)
  :d "Parses compact S-expression pointer string into PointerSpec record"
  (let [(trimmed (string-trim line))]
    (if (and (string-starts-with? trimmed "(:ptr")
             (string-ends-with? trimmed ")"))
        (let [(kind-str (extract-between trimmed ":kind \"" "\""))
              (id-str (extract-between trimmed ":id \"" "\""))
              (target-str (extract-between trimmed ":target \"" "\""))
              (tokens-str (extract-between trimmed ":tokens " " "))
              (expanded-str (extract-between trimmed ":expanded " " "))
              (summary-str (extract-between trimmed ":summary \"" "\")"))]
          (mt kind-str
            ((none) (none))
            ((some k)
             (mt id-str
               ((none) (none))
               ((some i)
                (mt target-str
                  ((none) (none))
                  ((some targ)
                   (let [(tok-val (option-or (string-to-int64 (string-trim (option-or tokens-str "0"))) 0))
                         (is-exp (string-starts-with? (string-trim (option-or expanded-str "false")) "true"))
                         (summ (option-or summary-str ""))]
                     (some (PointerSpec
                             :id i
                             :kind k
                             :target targ
                             :tokens tok-val
                             :expanded-tokens 0
                             :is-expanded is-exp
                             :summary summ
                             :payload ""))))))))))
        (none))))

(df total-active-tokens-loop [(ptrs (List PointerSpec)) (sum I64)] -> I64
  :d "Accumulates active token footprint across perceptual pointers"
  (if (list-empty? ptrs)
      sum
      (let [(default-ptr (PointerSpec :id "" :kind "" :target "" :tokens 0 :expanded-tokens 0 :is-expanded false :summary "" :payload ""))
            (head-p (option-or (list-head ptrs) default-ptr))
            (tail-p (option-or (list-tail ptrs) (list)))
            (act-tok (if (.-is-expanded head-p) (.-expanded-tokens head-p) (.-tokens head-p)))]
        (total-active-tokens-loop tail-p (+ sum act-tok)))))

(df total-active-tokens [(pointers (List PointerSpec))] -> I64
  :d "Calculates total active token footprint across perceptual pointer set"
  (total-active-tokens-loop pointers 0))

(df is-context-minimal? [(pointers (List PointerSpec)) (ceiling I64)] -> Bool
  :d "Verifies active pointer context footprint remains strictly below minimal threshold"
  (< (total-active-tokens pointers) ceiling))

(df orchestrate-loop [(ptrs (List PointerSpec)) (budget I64) (accumulated I64) (acc (List PointerSpec))] -> (List PointerSpec)
  :d "Evaluates pointers in caller priority order expanding affordable pointers and collapsing remainder"
  (if (list-empty? ptrs)
      acc
      (let [(default-ptr (PointerSpec :id "" :kind "" :target "" :tokens 0 :expanded-tokens 0 :is-expanded false :summary "" :payload ""))
            (head-p (option-or (list-head ptrs) default-ptr))
            (tail-p (option-or (list-tail ptrs) (list)))
            (exp-tok (if (> (.-expanded-tokens head-p) 0) (.-expanded-tokens head-p) (.-tokens head-p)))]
        (if (<= (+ accumulated exp-tok) budget)
            (let [(expanded-ptr (PointerSpec
                                  :id (.-id head-p)
                                  :kind (.-kind head-p)
                                  :target (.-target head-p)
                                  :tokens (.-tokens head-p)
                                  :expanded-tokens exp-tok
                                  :is-expanded true
                                  :summary (.-summary head-p)
                                  :payload (.-payload head-p)))]
              (orchestrate-loop tail-p budget (+ accumulated exp-tok) (list-append acc (list expanded-ptr))))
            (let [(collapsed-ptr (PointerSpec
                                   :id (.-id head-p)
                                   :kind (.-kind head-p)
                                   :target (.-target head-p)
                                   :tokens (.-tokens head-p)
                                   :expanded-tokens (.-expanded-tokens head-p)
                                   :is-expanded false
                                   :summary (.-summary head-p)
                                   :payload ""))]
              (orchestrate-loop tail-p budget (+ accumulated (.-tokens head-p)) (list-append acc (list collapsed-ptr))))))))

(df orchestrate-context-pointers [(pointers (List PointerSpec)) (budget I64)] -> (List PointerSpec)
  :d "Orchestrates dynamic expansion of perceptual pointers within configured token budget ceiling"
  (orchestrate-loop pointers budget 0 (list)))
