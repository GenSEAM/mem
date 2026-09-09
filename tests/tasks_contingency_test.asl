(module asl-mem/tasks-contingency-test
  :d "Unit and falsifiable verification suite for task tolerance corridors, anticipated risks, and escalation triggers."
  :x [test-anticipated-risk-construction
      test-task-tolerance-construction
      test-leeway-and-strict-invariant-validation
      test-escalation-trigger-matching
      test-asn-serialization
      run-tests]
  :i [(tasks :a t)])

(df test-anticipated-risk-construction [] -> Bool
  :d "Verifies construction and field integrity of AnticipatedRisk."
  (let [(r (t/make-anticipated-risk
             "Name collision in imported module"
             "Compilation failure duplicate symbol"
             "Use explicit module alias (:i [(mod :a m)])"
             "asl check target.asl"))]
    (do
      (assert (= (.-condition r) "Name collision in imported module") "Condition must match")
      (assert (= (.-impact r) "Compilation failure duplicate symbol") "Impact must match")
      (assert (= (.-prescribed-mitigation r) "Use explicit module alias (:i [(mod :a m)])") "Mitigation must match")
      (assert (= (.-contingency-gate r) "asl check target.asl") "Contingency gate must match")
      true)))

(df test-task-tolerance-construction [] -> Bool
  :d "Verifies construction of TaskTolerance with invariants, leeway, risks, and triggers."
  (let [(r (t/make-anticipated-risk "AST limit" "Over-token" "Factor helper" "asl check"))
        (tol (t/make-task-tolerance
               (list "c-0001" "c-0003" "wire-protocol")
               (list "internal-helper-decomposition" "extra-tests")
               (list r)
               (list "edits-outside-owns" "broken-api")))]
    (do
      (assert (= (list-length (.-strict-invariants tol)) 3) "Strict invariants count must be 3")
      (assert (= (list-length (.-variance-leeway tol)) 2) "Variance leeway count must be 2")
      (assert (= (list-length (.-anticipated-risks tol)) 1) "Anticipated risks count must be 1")
      (assert (= (list-length (.-escalation-triggers tol)) 2) "Escalation triggers count must be 2")
      true)))

(df test-leeway-and-strict-invariant-validation [] -> Bool
  :d "Verifies that proposed actions inside leeway are accepted and invariant violations are rejected."
  (let [(r (t/make-anticipated-risk "AST limit" "Over-token" "Factor helper" "asl check"))
        (tol (t/make-task-tolerance
               (list "c-0001" "wire-protocol")
               (list "internal-helper-decomposition" "extra-tests")
               (list r)
               (list "edits-outside-owns")))]
    (do
      (assert (t/task-is-within-leeway? tol "internal-helper-decomposition") "Permitted leeway action must pass")
      (assert (t/task-is-within-leeway? tol "extra-tests") "Extra tests action must pass")
      (assert (not (t/task-is-within-leeway? tol "c-0001")) "Strict invariant must be rejected")
      (assert (not (t/task-is-within-leeway? tol "wire-protocol")) "Wire protocol violation must be rejected")
      (assert (not (t/task-is-within-leeway? tol "unlisted-arbitrary-action")) "Unlisted action must be rejected")
      true)))

(df test-escalation-trigger-matching [] -> Bool
  :d "Verifies detection of escalation events and negative cases."
  (let [(tol (t/make-task-tolerance
               (list "c-0001")
               (list "extra-tests")
               (list)
               (list "edits-outside-owns" "broken-api-signature")))]
    (do
      (assert (t/task-has-escalation-trigger? tol "edits-outside-owns") "Known trigger must be detected")
      (assert (t/task-has-escalation-trigger? tol "broken-api-signature") "Known trigger must be detected")
      (assert (not (t/task-has-escalation-trigger? tol "normal-local-edit")) "Benign event must not trigger escalation")
      (assert (not (t/task-has-escalation-trigger? tol "extra-tests")) "Permitted action must not trigger escalation")
      true)))

(df test-asn-serialization [] -> Bool
  :d "Verifies canonical ASN formatting of AnticipatedRisk and TaskTolerance."
  (let [(r (t/make-anticipated-risk "Name collision" "Compile error" "Use alias" "asl check"))
        (tol (t/make-task-tolerance
               (list "c-0001" "c-0003")
               (list "helper-decomposition")
               (list r)
               (list "edits-outside-owns")))
        (s-risk (t/format-anticipated-risk-asn r))
        (s-tol (t/format-task-tolerance-asn tol))]
    (do
      (assert (string-contains? s-risk ":risk") "Risk ASN must contain :risk tag")
      (assert (string-contains? s-risk ":condition \"Name collision\"") "Risk ASN must contain condition")
      (assert (string-contains? s-risk ":mitigation \"Use alias\"") "Risk ASN must contain mitigation")
      (assert (string-contains? s-tol ":tolerance") "Tolerance ASN must contain :tolerance tag")
      (assert (string-contains? s-tol ":strict [\"c-0001\" \"c-0003\"]") "Tolerance ASN must contain strict list")
      (assert (string-contains? s-tol ":leeway [\"helper-decomposition\"]") "Tolerance ASN must contain leeway list")
      (assert (string-contains? s-tol ":escalate [\"edits-outside-owns\"]") "Tolerance ASN must contain escalate list")
      true)))

(df run-tests [] -> Bool
  :d "Executes all task tolerance and contingency assertions."
  (do
    (assert (test-anticipated-risk-construction))
    (assert (test-task-tolerance-construction))
    (assert (test-leeway-and-strict-invariant-validation))
    (assert (test-escalation-trigger-matching))
    (assert (test-asn-serialization))
    true))
