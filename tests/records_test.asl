(module asl-mem/test
  :d "Unit tests for asl-mem/records: ADR rules, shortcode scanning, and ledger verification."
  :x [test-shortcode-creation test-rule-and-ledger test-scan-and-verify run-tests main]
  :i [(records :a r)])

(df test-shortcode-creation [] -> Bool
  :d "Verifies ADR shortcode creation and kind mapping."
  (let [(sc (r/make-shortcode (r/p-dec) "d-1eed"))]
    (assert (= (.-code sc) "d-1eed") "Shortcode code must be d-1eed")
    (assert (= (r/shortcode-type-to-string (.-kind sc)) "d") "Shortcode kind must be d")
    true))

(df test-rule-and-ledger [] -> Bool
  :d "Verifies rule ledger construction and shortcode query lookup."
  (let [(rule1 (r/make-rule "d-1eed" "Standard Format" "Compact token ceiling" "active"))
        (rule2 (r/make-rule "l-a250" "Notes Invariant" "Comments are notes" "active"))
        (ledger (r/make-ledger (list rule1 rule2) (list "d-1eed" "l-a250")))
        (q1 (r/query-rule ledger "d-1eed"))
        (q2 (r/query-rule ledger "@rule:l-a250"))]
    (assert (mt q1 ((some _) true) ((none) false)) "Query for d-1eed must find rule")
    (assert (mt q2 ((some _) true) ((none) false)) "Query for @rule:l-a250 must find rule")
    true))

(df test-scan-and-verify [] -> Bool
  :d "Verifies shortcode scanning from source text and rule ledger verification."
  (let [(sample "This module respects @adr:d-1eed and invariant @rule:l-a250")
        (scs (r/scan-shortcodes sample))
        (rule1 (r/make-rule "d-1eed" "Standard Format" "Compact token ceiling" "active"))
        (rule2 (r/make-rule "l-a250" "Notes Invariant" "Comments are notes" "active"))
        (ledger (r/make-ledger (list rule1 rule2) (list "d-1eed" "l-a250")))
        (res (r/verify-module ledger "test-mod" sample))]
    (assert (= (list-length scs) 2) "Should find exactly 2 shortcodes in sample")
    (assert (list-empty? (.-missing res)) "No missing rules should be detected")
    true))

(df run-tests [] -> Bool
  :d "Runs all asl-mem records unit tests."
  (let [(_t1 (test-shortcode-creation))
        (_t2 (test-rule-and-ledger))
        (_t3 (test-scan-and-verify))]
    true))

(df ! main [(args (List Str))] -> (Result Unit IoError)
  :d "Runs unit tests for asl-mem records."
  (let [(_ (run-tests))]
    (println "asl-mem records tests passed cleanly")
    (ok ())))
