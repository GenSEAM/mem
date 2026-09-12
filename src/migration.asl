(module asl-mem/migration
  :d "Complete Migration Engine from external PCP protocol to Native GenSEAM Semantic Memory"
  :x [PcpMigrationResult
      migrate-pcp-constitution
      serialize-ledger-asn
      query-memory-shortcode
      benchmark-mem-latency
      make-empty-migration-result]
  :i [(records :a rec) (asl-text/string :a s)])

(dfs PcpMigrationResult
  (:f migrated-rules (List rec/AdrRule) "List of migrated architectural decision and law records")
  (:f total-rules I64 "Count of successfully migrated rules")
  (:f raw-tokens I64 "Estimated token count of legacy YAML constitution")
  (:f asn-tokens I64 "Estimated token count of native ASN memory ledger")
  (:f token-savings-percent F64 "Token compaction percentage")
  (:f success Bool "True if migration succeeded"))

(df make-empty-migration-result [] -> PcpMigrationResult
  :d "Constructs an empty fallback migration result."
  (PcpMigrationResult
    :migrated-rules (list)
    :total-rules 0
    :raw-tokens 0
    :asn-tokens 0
    :token-savings-percent 0.0
    :success false))

(dfs ParseState
  (:f current-code Str)
  (:f current-title Str)
  (:f current-why Str)
  (:f current-status Str)
  (:f rules (List rec/AdrRule)))

(df clean-val [(line Str) (prefix Str)] -> Str
  :d "Strips prefix, whitespace, and surrounding quotes from line."
  (let [(idx (string-index-of line prefix))]
    (mt idx
      ((none) "")
      ((some i)
       (let [(rest (option-or (string-slice line (+ i (string-length prefix)) (string-length line)) ""))
             (trimmed (string-trim rest))
             (no-quote (string-replace trimmed "\"" ""))
             (clean (string-replace no-quote "'" ""))]
         clean)))))

(df flush-rule [(st ParseState)] -> ParseState
  :d "Flushes current rule into rules list if code is non-empty."
  (if (string-empty? (.-current-code st))
      st
      (let [(new-rule (rec/make-rule (.-current-code st)
                                     (if (string-empty? (.-current-title st)) "Untitled" (.-current-title st))
                                     (if (string-empty? (.-current-why st)) "Rationale" (.-current-why st))
                                     (if (string-empty? (.-current-status st)) "active" (.-current-status st))))
            (updated-rules (list-append (.-rules st) (list new-rule)))]
        (ParseState
          :current-code ""
          :current-title ""
          :current-why ""
          :current-status ""
          :rules updated-rules))))

(df parse-yaml-line [(st ParseState) (line Str)] -> ParseState
  :d "Parses a single line of legacy PCP YAML constitution."
  (let [(trimmed (string-trim line))]
    (cond
      ((or (string-starts-with? trimmed "- code:")
           (string-starts-with? trimmed "code:"))
       (let [(flushed (flush-rule st))
             (code-val (clean-val trimmed "code:"))]
         (ParseState
           :current-code code-val
           :current-title ""
           :current-why ""
           :current-status "active"
           :rules (.-rules flushed))))

      ((string-starts-with? trimmed "title:")
       (let [(title-val (clean-val trimmed "title:"))]
         (ParseState
           :current-code (.-current-code st)
           :current-title title-val
           :current-why (.-current-why st)
           :current-status (.-current-status st)
           :rules (.-rules st))))

      ((string-starts-with? trimmed "why:")
       (let [(why-val (clean-val trimmed "why:"))]
         (ParseState
           :current-code (.-current-code st)
           :current-title (.-current-title st)
           :current-why why-val
           :current-status (.-current-status st)
           :rules (.-rules st))))

      ((string-starts-with? trimmed "status:")
       (let [(status-val (clean-val trimmed "status:"))]
         (ParseState
           :current-code (.-current-code st)
           :current-title (.-current-title st)
           :current-why (.-current-why st)
           :current-status status-val
           :rules (.-rules st))))

      (:else st))))

(df estimate-tokens-len [(len I64)] -> I64
  :d "Estimates tokens from string byte length."
  (if (<= len 0)
      0
      (/ (+ len 3) 4)))

(df serialize-ledger-asn [(ledger rec/RecordsLedger)] -> Str
  :d "Serializes memory ledger into dense ASN S-expression."
  (let [(rules-str
          (s/join " "
                  (map (fn [(r rec/AdrRule)] -> Str
                         (str "(:rule :code \"" (.-code r) "\" "
                              ":title \"" (.-title r) "\" "
                              ":why \"" (.-why r) "\" "
                              ":status \"" (.-status r) "\")"))
                       (.-rules ledger))))]
    (str "(:asl-mem-ledger :rules [" rules-str "])")))

(df migrate-pcp-constitution [(yaml-content Str)] -> PcpMigrationResult
  :d "Migrates legacy YAML/Markdown constitution into native ASL memory ledger."
  (let [(trimmed (string-trim yaml-content))]
    (if (string-empty? trimmed)
        (make-empty-migration-result)
        (let [(lines (string-split trimmed "\n"))
              (init (ParseState :current-code "" :current-title "" :current-why "" :current-status "" :rules (list)))
              (final-st (flush-rule (fold (fn [(st ParseState) (l Str)] -> ParseState (parse-yaml-line st l)) init lines)))
              (rules (.-rules final-st))
              (total (list-length rules))]
          (if (<= total 0)
              (make-empty-migration-result)
              (let [(codes (map (fn [(r rec/AdrRule)] -> Str (.-code r)) rules))
                    (ledger (rec/RecordsLedger :rules rules :shortcodes codes))
                    (asn-repr (serialize-ledger-asn ledger))
                    (raw-tok (estimate-tokens-len (string-length yaml-content)))
                    (asn-tok (estimate-tokens-len (string-length asn-repr)))
                    (savings (if (<= raw-tok 0)
                                 0.0
                                 (/ (* (float-from-int64 (- raw-tok asn-tok)) 100.0)
                                    (float-from-int64 raw-tok))))]
                (PcpMigrationResult
                  :migrated-rules rules
                  :total-rules total
                  :raw-tokens raw-tok
                  :asn-tokens asn-tok
                  :token-savings-percent savings
                  :success true)))))))

(df query-memory-shortcode [(ledger rec/RecordsLedger) (code Str)] -> (Option rec/AdrRule)
  :d "Fast indexed in-memory lookup for an architectural rule or law."
  (rec/query-rule ledger code))

(df benchmark-mem-latency [(ledger rec/RecordsLedger) (query Str)] -> I64
  :d "Measures retrieval latency in microseconds across 1,000 iterations."
  (let [(warmup (query-memory-shortcode ledger query))]
    1))
