(module asl-mem/tests/roadmap-test
  :d "Unit verification suite for native ASL-Mem Roadmap engine, dual projection, and atomic CAS buffer versioning."
  :x [run-tests
      test-item-and-phase-records
      test-lease-and-roadmap-records
      test-defrecord-serialization
      test-phase-lifecycle-and-claim
      test-dual-projection-engine]
  :i [(roadmap :a rm)
      (projector :a prj)])

(df test-item-and-phase-records [] -> Bool
  :d "Verifies work item and phase record construction and accessors."
  (let [(it1 (rm/make-item "296.1" "Roadmap Grammar" "pending" "asl check mem/src/roadmap.asl"))
        (it2 (rm/make-item "296.2" "CAS Versioning" "pending" "asl test mem/tests/roadmap_test.asl"))
        (p (rm/make-phase "phase-296" "Native ASL Roadmap" "in-progress" "Wave E" "asl gate" (list it1 it2)))]
    (assert (= (.-id it1) "296.1") "Item 1 ID must match")
    (assert (= (.-name it1) "Roadmap Grammar") "Item 1 name must match")
    (assert (= (.-status it1) "pending") "Item 1 status must be pending")
    (assert (= (.-id p) "phase-296") "Phase ID must match")
    (assert (= (.-wave p) "Wave E") "Phase wave must match")
    (assert (= (.-status p) "in-progress") "Phase status must match")
    (assert (= (list-length (.-items p)) 2) "Phase must contain 2 items")
    true))

(df test-lease-and-roadmap-records [] -> Bool
  :d "Verifies lease creation, TTL validity checking, and roadmap initialization."
  (let [(now 1000)
        (ttl 30000)
        (l (rm/make-lease "phase-296" "agent-alpha" now ttl))
        (p (rm/make-phase "phase-296" "Native ASL Roadmap" "pending" "Wave E" "asl gate" (list)))
        (r (rm/make-roadmap "iter-2026" (list p)))]
    (assert (= (.-phase-id l) "phase-296") "Lease phase-id must match")
    (assert (= (.-agent-id l) "agent-alpha") "Lease agent-id must match")
    (assert (rm/is-lease-active? l 1500) "Lease must be active within TTL window")
    (assert (not (rm/is-lease-active? l 32000)) "Lease must expire after acquired plus TTL")
    (assert (= (.-iteration r) "iter-2026") "Roadmap iteration must match")
    (assert (= (.-version r) 1) "Initial roadmap version must be 1")
    (assert (= (list-length (.-phases r)) 1) "Roadmap must have 1 phase")
    true))

(df test-defrecord-serialization [] -> Bool
  :d "Verifies canonical ASN Defrecord serialization eliminating key repetition."
  (let [(p1 (rm/make-phase "phase-283" "WASM AOT" "done" "Wave A" "asl gate" (list)))
        (p2 (rm/make-phase "phase-284" "HM Type Checker" "done" "Wave A" "asl gate" (list)))
        (r (rm/make-roadmap "iter-test" (list p1 p2)))
        (asn (rm/format-roadmap-asn r))]
    (assert (string-contains? asn "(:roadmap") "Serialization must include :roadmap root")
    (assert (string-contains? asn ":iteration \"iter-test\"") "Iteration must match in ASN")
    (assert (string-contains? asn ":schema (id name status wave gate)") "Defrecord schema header must be present")
    (assert (string-contains? asn "\"phase-283\" \"WASM AOT\" \"done\" \"Wave A\"") "Phase 283 tuple must match")
    (assert (string-contains? asn "\"phase-284\" \"HM Type Checker\" \"done\" \"Wave A\"") "Phase 284 tuple must match")
    true))

(df test-phase-lifecycle-and-claim [] -> Bool
  :d "Verifies phase lifecycle operations: register, get, claim, complete."
  (let [(it1 (rm/make-item "296.1" "Grammar" "pending" "gate1"))
        (p1 (rm/make-phase "phase-296" "Engine" "pending" "Wave E" "gate" (list it1)))
        (r0 (rm/make-roadmap "iter-test" (list p1)))
        (p-found (rm/phase-get r0 "phase-296"))
        (p-miss (rm/phase-get r0 "phase-nonexistent"))]
    (assert (is-some? p-found) "Existing phase must be found")
    (assert (is-none? p-miss) "Nonexistent phase must return none")
    (let [(p2 (rm/make-phase "phase-297" "Harness" "pending" "Wave E" "gate" (list)))
          (r1 (rm/phase-register r0 p2))]
      (assert (= (.-version r1) 2) "Version must increment after register")
      (assert (= (list-length (.-phases r1)) 2) "Roadmap must now have 2 phases")
      (let [(claim-res (rm/phase-claim r1 "phase-296" "worker-1" 1000 30000))]
        (assert (.-success claim-res) "Phase claim must succeed on unleased phase")
        (assert (is-some? (.-lease claim-res)) "Claim result must include lease")
        (let [(r2 (.-roadmap claim-res))
              (r3 (rm/item-complete r2 "phase-296" "296.1"))
              (p-upd (option-or (rm/phase-get r3 "phase-296") p1))
              (first-it (option-or (list-head (.-items p-upd)) it1))]
          (assert (= (.-status first-it) "completed") "Work item status must be updated to completed")
          (let [(r4 (rm/phase-complete r3 "phase-296"))
                (p-done (option-or (rm/phase-get r4 "phase-296") p1))]
            (assert (= (.-status p-done) "completed") "Phase status must be updated to completed")
            (assert (= (list-length (.-leases r4)) 0) "Completed phase must release its active leases")
            true))))))

(df test-dual-projection-engine [] -> Bool
  :d "Verifies dual-projection of in-memory state to STATUS.md and phase PLAN.md."
  (let [(row (prj/format-phase-status-row "phase-296" "in-progress" "asl gate"))
        (s-md (prj/project-status-md "iter-2026" row))
        (p-item (prj/format-plan-item "296.1" "Defrecord Schema" "done" "asl check"))
        (p-md (prj/project-plan-md "phase-296" "Roadmap Engine" "Execute roadmap engine" "asl gate" p-item))
        (dual (prj/project-dual-manifest "iter-2026" "phase-296" "Roadmap Engine" "in-progress" "Wave E" "asl gate"))]
    (assert (string-contains? row "| `phase-296` | in-progress | `asl gate` | PASS |") "Table row must match")
    (assert (string-contains? s-md "# Current Status: `iter-2026`") "Status markdown must include title")
    (assert (string-contains? s-md "## Active Roadmap Waves") "Status markdown must include waves header")
    (assert (string-contains? p-md "# Plan: Roadmap Engine") "Plan markdown must include phase plan title")
    (assert (string-contains? p-md "### Item 296.1: Defrecord Schema") "Plan markdown must include item header")
    (assert (string-contains? p-md "asl gate") "Plan markdown must include acceptance gate")
    (assert (= (.-target-plan-path dual) ".plans/phase-296/PLAN.md") "Dual projection path must be canonical")
    (assert (string-contains? (.-status-md dual) "phase-296") "Dual projection status must contain phase ID")
    true))



(df run-tests [] -> Bool
  :d "Executes complete roadmap verification test suite with >=20 strict assertions."
  (and (test-item-and-phase-records)
       (and (test-lease-and-roadmap-records)
            (and (test-defrecord-serialization)
                 (and (test-phase-lifecycle-and-claim)
                      (test-dual-projection-engine))))))
