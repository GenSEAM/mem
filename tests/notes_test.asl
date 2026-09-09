(module mem-notes-test
  :d "Unit tests for pure AgentScript machine-native notes ledger engine."
  :i [(asl-mem/notes :a n)])

(df test-notes-lifecycle [] -> Bool
  :d "Verifies NoteRecord construction, filtering, dual-projection, and promotion lifecycle."
  (let [(note1 (n/make-note
                 "n1"
                 "token-economy"
                 "auditor"
                 "sess-01"
                 "2026-09-09"
                 "active"
                 (list "asl/asl" "harness/paradigm.asn")
                 (list "tokens" "density" "prefixes")
                 "BPE tokenizers penalize hyphenated shortcodes; prefix plus number is 2 tokens."
                 "Empirical tiktoken run on cl100k and o200k."
                 "Adopt 2-token compact shortcode convention."))
        (note2 (n/make-note
                 "n2"
                 "session-recovery"
                 "implementer"
                 "sess-02"
                 "2026-09-09"
                 "archived"
                 (list "mem/src/tasks.asl")
                 (list "tasks" "sessions")
                 "In-flight tasks persist in in_flight.asn with step-index."
                 "Verified by tests/test_task_protocol_e2e.sh."
                 "Enable seamless crash recovery."))
        (asn-str (n/format-note-asn note1))
        (md-str (n/project-note-md note1))
        (promoted-task (n/note-promote-to-task note1 "t388-1"))
        (promoted-adr (n/note-promote-to-adr note1 "d45"))]
    (assert (= (.-id note1) "n1"))
    (assert (= (.-topic note1) "token-economy"))
    (assert (n/note-is-active? note1))
    (assert (not (n/note-is-active? note2)))
    (assert (n/note-has-tag? note1 "tokens"))
    (assert (n/note-has-tag? note1 "density"))
    (assert (not (n/note-has-tag? note1 "nonexistent")))
    (assert (n/note-scope-matches? note1 "asl/asl"))
    (assert (n/note-scope-matches? note1 "harness/paradigm.asn"))
    (assert (not (n/note-scope-matches? note1 "foreign/path.py")))
    (assert (n/note-matches-query? note1 "token"))
    (assert (n/note-matches-query? note1 "n1"))
    (assert (n/note-matches-query? note1 "BPE"))
    (assert (not (n/note-matches-query? note1 "unrelated-query")))
    (assert (string-contains? asn-str "(:note"))
    (assert (string-contains? asn-str ":id \"n1\""))
    (assert (string-contains? asn-str ":tags [:tokens :density :prefixes]"))
    (assert (string-contains? md-str "# Note [n1]: token-economy"))
    (assert (string-contains? md-str "## Empirical Fact"))
    (assert (string-contains? md-str "## Grounded Evidence"))
    (assert (string-contains? md-str "## Architectural Consequence"))
    (assert (= (.-status promoted-task) "promoted-to-task"))
    (assert (string-contains? (.-consequence promoted-task) "t388-1"))
    (assert (= (.-status promoted-adr) "promoted-to-adr"))
    (assert (string-contains? (.-consequence promoted-adr) "d45"))
    true))
