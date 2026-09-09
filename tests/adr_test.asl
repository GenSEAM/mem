(module asl-mem/tests/adr-test
  :d "Unit tests for AdrRecord, ASN serialization, and Dual-Projection markdown generation."
  :x [main]
  :i [(projector :a p)])

(df test-adr-construction [] -> Bool
  (let [(pos (p/AdrConsequence :kind "pos" :text "Reduces tokens by 75%"))
        (neg (p/AdrConsequence :kind "neg" :text "Requires on-demand projection"))
        (adr (p/make-adr "ADR-0044"
                         "d-0044"
                         "Machine-Native ASN Decisions"
                         "active"
                         "2026-09-09"
                         "architecture/governance"
                         (list "architect" "planner")
                         "Markdown ADRs impose token tax"
                         (list "Context preservation" "AST queries")
                         "Store ADRs in ASN format"
                         (list "c-0001" "c-0003")
                         (list pos neg)))]
    (assert (= (.-id adr) "ADR-0044") "ADR ID must match")
    (assert (= (.-shortcode adr) "d-0044") "Shortcode must match")
    (assert (= (.-status adr) "active") "Status must be active")
    (assert (= (.-cluster adr) "architecture/governance") "Cluster must match")
    (assert (= (list-length (.-deciders adr)) 2) "Deciders count must be 2")
    (assert (= (list-length (.-consequences adr)) 2) "Consequences count must be 2")
    true))

(df test-adr-asn-serialization [] -> Bool
  (let [(pos (p/AdrConsequence :kind "pos" :text "High lexical density"))
        (adr (p/make-adr "ADR-0044"
                         "d-0044"
                         "Machine-Native Decisions"
                         "active"
                         "2026-09-09"
                         "architecture/governance"
                         (list "auditor")
                         "Problem desc"
                         (list "Driver 1")
                         "Decision outcome"
                         (list "c-0001")
                         (list pos)))
        (asn-str (p/format-adr-asn adr))]
    (assert (string-contains? asn-str "(:adr") "Must contain :adr head")
    (assert (string-contains? asn-str ":id \"ADR-0044\"") "Must serialize ID")
    (assert (string-contains? asn-str ":shortcode \"d-0044\"") "Must serialize shortcode")
    (assert (string-contains? asn-str ":status :active") "Must serialize status keyword")
    (assert (string-contains? asn-str ":pos \"High lexical density\"") "Must serialize positive consequence")
    true))

(df test-adr-markdown-projection [] -> Bool
  (let [(pos (p/AdrConsequence :kind "pos" :text "Deterministic AST queries"))
        (neg (p/AdrConsequence :kind "neg" :text "Transient human rendering step"))
        (adr (p/make-adr "ADR-0044"
                         "d-0044"
                         "Dual-Projection Governance"
                         "active"
                         "2026-09-09"
                         "architecture/governance"
                         (list "system-architect")
                         "Markdown wastes agent attention"
                         (list "Token economy" "Homoiconicity")
                         "Adopt canonical ASN storage"
                         (list "c-0001" "c-0003")
                         (list pos neg)))
        (md-str (p/project-adr-md adr))]
    (assert (string-contains? md-str "# ADR-0044: Dual-Projection Governance") "Must contain H1 markdown title")
    (assert (string-contains? md-str "- **Shortcode**: `d-0044`") "Must contain formatted shortcode")
    (assert (string-contains? md-str "## Problem Statement") "Must contain Problem Statement section")
    (assert (string-contains? md-str "## Decision Drivers") "Must contain Decision Drivers section")
    (assert (string-contains? md-str "## Decision") "Must contain Decision section")
    (assert (string-contains? md-str "## Consequences") "Must contain Consequences section")
    (assert (string-contains? md-str "### Positive") "Must contain Positive section")
    (assert (string-contains? md-str "- Deterministic AST queries") "Must list positive consequence bullet")
    (assert (string-contains? md-str "### Negative") "Must contain Negative section")
    (assert (string-contains? md-str "- Transient human rendering step") "Must list negative consequence bullet")
    true))

(df ! main [(args (List Str))] -> (Result Unit IoError)
  :d "Executes pure ASL ADR and Dual-Projection test suite."
  (if (and (test-adr-construction)
           (and (test-adr-asn-serialization)
                (test-adr-markdown-projection)))
    (let [(u (println "ADR Dual-Projection test suite passed cleanly"))]
      (Ok Unit))
    (let [(err (println "ADR Dual-Projection test suite FAILED"))]
      (Err (io/make-error 1 "Assertion failed")))))
