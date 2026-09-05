(module asl-mem/migration-test
  :d "Unit verification test suite for PCP-to-ASL-MEM Migration"
  :x [test-migrate-pcp-yaml
      test-query-memory-shortcode
      test-serialize-ledger-asn
      test-migration-token-savings
      test-empty-migration
      run-migration-tests]
  :i [(migration :a mig) (records :a rec)])

(df sample-yaml-constitution [] -> Str
  :d "Sample legacy PCP YAML constitution"
  (str "name: workspace-constitution\n"
       "version: 1.0.0\n"
       "rules:\n"
       "  - code: l-0001\n"
       "    title: Pure AgentScript Invariant\n"
       "    why: Zero foreign files allowed in packages\n"
       "    status: active\n"
       "  - code: d-0042\n"
       "    title: Native S-Expression Memory Storage\n"
       "    why: Eliminates markdown file parsing overhead and context drowning\n"
       "    status: active\n"
       "  - code: c-0007\n"
       "    title: No Subagent Context Duplication\n"
       "    why: Internalize steps protocol to save tokens and eliminate latency\n"
       "    status: active\n"))

(df test-migrate-pcp-yaml [] -> Bool
  :d "Tests migration of multi-rule legacy YAML constitution into typed memory ledger"
  (let [(res (mig/migrate-pcp-constitution (sample-yaml-constitution)))]
    (and (.-success res)
         (= (.-total-rules res) 3))))

(df test-query-memory-shortcode [] -> Bool
  :d "Tests fast indexed lookup by shortcode"
  (let [(res (mig/migrate-pcp-constitution (sample-yaml-constitution)))
        (rules (.-migrated-rules res))
        (codes (map (fn [(r rec/AdrRule)] -> Str (.-code r)) rules))
        (ledger (rec/RecordsLedger :rules rules :shortcodes codes))
        (found (mig/query-memory-shortcode ledger "l-0001"))]
    (mt found
      ((none) false)
      ((some rule)
       (and (= (.-code rule) "l-0001")
            (= (.-title rule) "Pure AgentScript Invariant"))))))

(df test-serialize-ledger-asn [] -> Bool
  :d "Tests serialization of memory ledger to native S-expression"
  (let [(res (mig/migrate-pcp-constitution (sample-yaml-constitution)))
        (rules (.-migrated-rules res))
        (codes (map (fn [(r rec/AdrRule)] -> Str (.-code r)) rules))
        (ledger (rec/RecordsLedger :rules rules :shortcodes codes))
        (asn-str (mig/serialize-ledger-asn ledger))]
    (and (string-contains? asn-str "(:asl-mem-ledger")
         (and (string-contains? asn-str "(:rule :code \"l-0001\"")
              (string-contains? asn-str "(:rule :code \"d-0042\"")))))

(df test-migration-token-savings [] -> Bool
  :d "Tests token compaction measurement"
  (let [(res (mig/migrate-pcp-constitution (sample-yaml-constitution)))]
    (and (.-success res)
         (> (.-raw-tokens res) 0))))

(df test-empty-migration [] -> Bool
  :d "Tests graceful fallback on empty YAML input"
  (let [(res (mig/migrate-pcp-constitution ""))]
    (not (.-success res))))

(df run-migration-tests [] -> Bool
  :d "Runs all migration unit test cases"
  (and (test-migrate-pcp-yaml)
       (and (test-query-memory-shortcode)
            (and (test-serialize-ledger-asn)
                 (and (test-migration-token-savings)
                      (test-empty-migration))))))
