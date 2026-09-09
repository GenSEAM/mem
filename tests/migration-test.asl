(module asl-mem/migration-test
  :d "Unit verification test suite for PCP-to-ASL-MEM Migration"
  :x [test-migrate-pcp-yaml
      test-query-memory-shortcode
      test-serialize-ledger-asn
      test-migration-token-savings
      test-empty-migration
      run-migration-tests
      run-tests]
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
    (assert (.-success res) "migration succeeds")
    (assert (= (.-total-rules res) 3) "total rules is 3")
    true))

(df test-query-memory-shortcode [] -> Bool
  :d "Tests fast indexed lookup by shortcode"
  (let [(res (mig/migrate-pcp-constitution (sample-yaml-constitution)))
        (rules (.-migrated-rules res))
        (codes (map (fn [(r rec/AdrRule)] -> Str (.-code r)) rules))
        (ledger (rec/RecordsLedger :rules rules :shortcodes codes))
        (found (mig/query-memory-shortcode ledger "l-0001"))]
    (mt found
      ((none) (do (assert false "rule not found") false))
      ((some rule)
       (do
         (assert (= (.-code rule) "l-0001") "code matches")
         (assert (= (.-title rule) "Pure AgentScript Invariant") "title matches")
         true)))))

(df test-serialize-ledger-asn [] -> Bool
  :d "Tests serialization of memory ledger to native S-expression"
  (let [(res (mig/migrate-pcp-constitution (sample-yaml-constitution)))
        (rules (.-migrated-rules res))
        (codes (map (fn [(r rec/AdrRule)] -> Str (.-code r)) rules))
        (ledger (rec/RecordsLedger :rules rules :shortcodes codes))
        (asn-str (mig/serialize-ledger-asn ledger))]
    (assert (string-contains? asn-str "(:asl-mem-ledger") "contains ledger tag")
    (assert (string-contains? asn-str "(:rule :code \"l-0001\"") "contains rule l-0001")
    (assert (string-contains? asn-str "(:rule :code \"d-0042\"") "contains rule d-0042")
    true))

(df test-migration-token-savings [] -> Bool
  :d "Tests token compaction measurement"
  (let [(res (mig/migrate-pcp-constitution (sample-yaml-constitution)))]
    (assert (.-success res) "success true")
    (assert (> (.-raw-tokens res) 0) "raw tokens > 0")
    true))

(df test-empty-migration [] -> Bool
  :d "Tests graceful fallback on empty YAML input"
  (let [(res (mig/migrate-pcp-constitution ""))]
    (assert (not (.-success res)) "empty input fails")
    (assert (= (.-total-rules res) 0) "empty input total rules is 0")
    true))

(df run-migration-tests [] -> Bool
  :d "Runs all migration unit test cases"
  (and (test-migrate-pcp-yaml)
       (and (test-query-memory-shortcode)
            (and (test-serialize-ledger-asn)
                 (and (test-migration-token-savings)
                      (test-empty-migration))))))

(df run-tests [] -> Bool
  (run-migration-tests))
