(module asl-mem/tests/consistency-audit-test
  :d "Comprehensive falsifiable test suite for pure AgentScript 5D consistency audit per D52."
  :x [run-tests
      test-shortcode-bidirectional-match
      test-d52-completeness-check
      test-roadmap-gate-verification
      test-clean-report-succeeds
      test-broken-report-fails]
  :i [(consistency-audit :a ca)])

(df test-shortcode-bidirectional-match [] -> Bool
  :d "Verifies bidirectional shortcode matching with positive and negative cases."
  (let [(code1 "D52")
        (code2 "d-0052")
        (code3 "D999")]
    (assert (ca/verify-shortcode-bidirectional? code1 code2) "D52 and d-0052 must match under normalization")
    (assert (not (ca/verify-shortcode-bidirectional? code1 code3)) "D52 and D999 must not match")
    true))

(df test-d52-completeness-check [] -> Bool
  :d "Verifies D52 field completeness detection with full and partial field lists."
  (let [(full-fields (list ":motivation" ":purpose" ":context" ":outcomes" ":owns" ":invariants" ":variations" ":failureModes" ":adr" ":decision" ":gate" ":actionDag"))
        (partial-fields (list ":motivation" ":purpose" ":context" ":outcomes"))]
    (assert (ca/check-d52-completeness? full-fields) "Full fields list must pass D52 check")
    (assert (not (ca/check-d52-completeness? partial-fields)) "Partial fields list must fail D52 check")
    true))

(df test-roadmap-gate-verification [] -> Bool
  :d "Verifies validation of non-empty executable roadmap gate commands."
  (let [(good-gate "asl test --strict-falsify mem/tests/vfs_test.asl && asl gate")
        (empty-gate "")
        (whitespace-gate "   ")
        (invalid-gate "echo ok")]
    (assert (ca/verify-roadmap-gate? good-gate) "Standard asl gate command must pass validation")
    (assert (not (ca/verify-roadmap-gate? empty-gate)) "Empty gate string must fail validation")
    (assert (not (ca/verify-roadmap-gate? whitespace-gate)) "Whitespace-only gate must fail validation")
    true))

(df test-clean-report-succeeds [] -> Bool
  :d "Verifies clean 5D consistency report attributes and passing status."
  (let [(rep (ca/make-clean-report 55 272 175 58 89 39))]
    (assert (.-all-passed rep) "Clean report must have all-passed true")
    (assert (= (.-broken-links rep) 0) "Clean report must have zero broken links")
    (assert (= (.-desync-intents rep) 0) "Clean report must have zero desync intents")
    (assert (= (.-context-voids rep) 0) "Clean report must have zero context voids")
    (assert (= (.-delimiter-errors rep) 0) "Clean report must have zero delimiter errors")
    (assert (= (.-emoji-violations rep) 0) "Clean report must have zero emoji violations")
    true))

(df test-broken-report-fails [] -> Bool
  :d "Verifies that reports with anomalies in any dimension fail overall verification."
  (let [(rep-broken (ca/audit-consistency-state 55 272 1 0 0 0 0 0))
        (rep-desync (ca/audit-consistency-state 55 272 0 1 0 0 0 0))
        (rep-voids (ca/audit-consistency-state 55 272 0 0 1 0 0 0))
        (rep-delim (ca/audit-consistency-state 55 272 0 0 0 0 1 0))
        (rep-emoji (ca/audit-consistency-state 55 272 0 0 0 0 0 1))]
    (assert (not (.-all-passed rep-broken)) "Report with broken link must fail")
    (assert (not (.-all-passed rep-desync)) "Report with desync intent must fail")
    (assert (not (.-all-passed rep-voids)) "Report with context void must fail")
    (assert (not (.-all-passed rep-delim)) "Report with delimiter error must fail")
    (assert (not (.-all-passed rep-emoji)) "Report with emoji violation must fail")
    true))

(df run-tests [] -> Bool
  :d "Runs all test cases in the consistency audit test suite."
  (do
    (test-shortcode-bidirectional-match)
    (test-d52-completeness-check)
    (test-roadmap-gate-verification)
    (test-clean-report-succeeds)
    (test-broken-report-fails)
    true))
