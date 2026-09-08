(module asl-mem/tests/hydration-test
  :d "Comprehensive unit test suite for managed amnesia cascade compression, markdown hydration view, and summary search."
  :x [run-tests
      test-compression-threshold
      test-cascade-compression
      test-cascade-compression-empty-buffer
      test-raw-context-eviction
      test-markdown-hydration-and-card
      test-summary-search-and-autoloading
      test-hydrate-intent-summary
      test-modern-intent-ledger-hydration]
  :i [(amnesia :a a)
      (hydration :a h)
      (view_layer :a vl)
      (summary_search :a ss)])

(df test-compression-threshold [] -> Bool
  :d "Verifies threshold watcher behavior across below-threshold exact-boundary above-threshold and zero."
  (assert (not (h/check-compression-threshold 1500 3000)) "1500 tokens must not trigger 3000 token threshold")
  (assert (h/check-compression-threshold 3000 3000) "3000 tokens must trigger exact 3000 token threshold")
  (assert (h/check-compression-threshold 4500 3000) "4500 tokens must trigger 3000 token threshold")
  (assert (not (h/check-compression-threshold 0 3000)) "0 tokens must not trigger 3000 token threshold")
  true)

(df test-cascade-compression [] -> Bool
  :d "Verifies cascade compression folds messages into MemoryChunk and resets working RAM."
  (let [(buf (h/make-context-buffer
               "buf-001"
               3500
               (list "fact-1: agent initialized"
                     "directive-1: enforce acyclicity"
                     "fact-2: DAG enabled"
                     "Backlog fold snapshot")
               (list)))
        (res (h/cascade-compress buf 3000 (list "parent-root") 1725769200000))
        (chunk (.-first res))
        (new-buf (.-second res))]
    (assert (a/validate-memory-chunk chunk) "Compressed chunk must satisfy structural validation")
    (assert (> (string-length (.-id chunk)) 0) "Generated chunk ID must be non-empty")
    (assert (> (.-ts chunk) 0) "Generated chunk timestamp must be positive epoch millisecond")
    (assert (= (.-conf chunk) 0.95) "Default confidence score must equal 0.95")
    (assert (= (list-length (.-facts chunk)) 2) "Chunk must contain exactly 2 scalar facts")
    (assert (= (list-length (.-directives chunk)) 1) "Chunk must contain exactly 1 directive")
    (assert (list-contains? (.-refs chunk) "parent-root") "Causal parent references must be linked")
    (assert (and (= (.-active-tokens new-buf) 0) (list-empty? (.-messages new-buf))) "Context buffer messages must be evicted and active tokens reset to 0")
    true))

(df test-cascade-compression-empty-buffer [] -> Bool
  :d "Verifies cascade compression edge case with zero active messages."
  (let [(buf (h/make-context-buffer "buf-empty" 0 (list) (list)))
        (res (h/cascade-compress buf 3000 (list) 1725769200000))
        (chunk (.-first res))
        (new-buf (.-second res))]
    (assert (a/validate-memory-chunk chunk) "Empty buffer compression must produce valid chunk")
    (assert (and (list-empty? (.-facts chunk)) (list-empty? (.-directives chunk))) "Empty buffer chunk must have empty facts and empty directives")
    (assert (= (.-active-tokens new-buf) 0) "Returned context buffer must remain at 0 tokens")
    true))

(df test-raw-context-eviction [] -> Bool
  :d "Verifies operational working RAM wipe while preserving compressed chunk stubs."
  (let [(c1 (a/make-memory-chunk "c-pre-1" 1000 0.90 "pre-existing" (list) (list) (list)))
        (buf (h/make-context-buffer
               "buf-evict"
               2400
               (list "raw msg 1" "raw msg 2")
               (list c1)))
        (evicted (h/evict-raw-context buf))]
    (assert (list-empty? (.-messages evicted)) "Evicted buffer must have zero raw operational messages")
    (assert (= (.-active-tokens evicted) 0) "Evicted buffer must have 0 active tokens")
    (assert (= (list-length (.-chunks evicted)) 1) "Evicted buffer must retain pre-existing compressed chunk stubs")
    (assert (= (.-id (option-or (list-head (.-chunks evicted)) c1)) "c-pre-1") "Pre-existing chunk ID must match retained stub")
    true))

(df test-markdown-hydration-and-card [] -> Bool
  :d "Verifies on-demand Markdown projection and compact summary card formatting."
  (let [(c (a/make-memory-chunk
             "chunk-md-1"
             1725769200123
             0.95
             "User requested memory persistence"
             (list "agent initialized" "DAG enabled")
             (list "enforce acyclicity")
             (list "parent-000")))
        (md (vl/hydrate-to-markdown c))
        (c-empty (a/make-memory-chunk "chunk-empty" 1000 0.90 "empty" (list) (list) (list)))
        (md-empty (vl/hydrate-to-markdown c-empty))
        (card (vl/render-summary-card c))]
    (assert (string-contains? md "# Memory Chunk: chunk-md-1") "Markdown output must contain title header with chunk ID")
    (assert (and (string-contains? md "**Timestamp**: 1725769200123") (string-contains? md "**Confidence**: 0.95")) "Markdown output must contain formatted timestamp and confidence")
    (assert (string-contains? md "| Index | Fact |") "Markdown output must contain facts table syntax")
    (assert (string-contains? md "> [!IMPORTANT]") "Markdown output must contain GitHub alert box for directives")
    (assert (string-contains? md "### Causal References") "Markdown output must contain causal references section")
    (assert (and (string-contains? md-empty "*No scalar facts recorded.*") (string-contains? md-empty "*No operational directives.*")) "Empty chunk must display valid markdown placeholder text")
    (assert (and (string-contains? card "[Chunk: chunk-md-1]") (and (string-contains? card "0.95") (string-contains? card "1725769200123 ms"))) "Summary card must format compact ID confidence and millisecond timestamp")
    true))

(df test-summary-search-and-autoloading [] -> Bool
  :d "Verifies keyword indexing and dual-format summary search retrieval."
  (let [(idx0 (ss/summary-index-empty))
        (c1 (a/make-memory-chunk "c-s-1" 1000 0.95 "Memory persistence and cascade amnesia" (list) (list) (list)))
        (c2 (a/make-memory-chunk "c-s-2" 2000 0.90 "Human hydration view layer projection" (list) (list) (list)))
        (idx1 (ss/index-chunk-summary idx0 c1))
        (idx2 (ss/index-chunk-summary idx1 c2))]
    (assert (= (.-size idx0) 0) "Empty summary index must have size 0")
    (assert (= (.-size idx2) 2) "Summary index after two insertions must have size 2")
    (let [(res-csexpr (ss/search-chunks-by-summary idx2 "persistence" "cs-expr"))]
      (assert (and (= (list-length res-csexpr) 1) (= (option-or (list-head res-csexpr) "") "c-s-1")) "Keyword query with cs-expr format must return matching chunk ID"))
    (let [(res-md (ss/search-chunks-by-summary idx2 "hydration" "markdown"))]
      (assert (and (= (list-length res-md) 1) (string-contains? (option-or (list-head res-md) "") "[Chunk: c-s-2]")) "Keyword query with markdown format must return rendered summary card"))
    (let [(res-none (ss/search-chunks-by-summary idx2 "nonexistent-term" "cs-expr"))]
      (assert (list-empty? res-none) "Search with non-existent keyword must return empty list"))
    (let [(res-empty-idx (ss/search-chunks-by-summary idx0 "persistence" "cs-expr"))]
      (assert (list-empty? res-empty-idx) "Search on empty index must return empty list"))
    true))

(df test-hydrate-intent-summary [] -> Bool
  :d "Verifies intent summary hydration and core axiom formatting without disk scanning."
  (let [(axioms (h/default-core-axioms))
        (summary (h/hydrate-intent-summary axioms))
        (empty-summary (h/hydrate-intent-summary (list)))
        (syn (h/synthesize-intent-summary "(:id \"d-0015\" :title \"Axiom of Token Arbitrage\")"))
        (syn-fail (h/synthesize-intent-summary "empty"))]
    (assert (= (list-length axioms) 4) "Default core axioms must contain exactly 4 entries")
    (assert (string-contains? summary "d-0015: Token Arbitrage") "Summary must include Axiom 1 (d-0015)")
    (assert (string-contains? summary "d-0016: Agent-Native Autonomy") "Summary must include Axiom 2 (d-0016)")
    (assert (string-contains? summary "d-0017: Falsifiable Observability") "Summary must include Axiom 3 (d-0017)")
    (assert (string-contains? summary "d-0018: In-Memory State Surgery") "Summary must include Axiom 4 (d-0018)")
    (assert (string-contains? empty-summary "Token Arbitrage") "Empty axioms list must fall back to canonical axiom names")
    (assert (string-contains? syn "d-0015") "Synthesized summary must contain d-0015 on matching ledger")
    (assert (= syn-fail "Axioms: Ungrounded") "Synthesized summary must flag ungrounded on unmatched input")
    true))

(df test-modern-intent-ledger-hydration [] -> Bool
  :d "Verifies deterministic hydration of modern phase decisions d-0019 through d-0029 and liquidation of legacy PCP hashes."
  (let [(d19 (h/hydrate-modern-intent "d-0019"))
        (d20 (h/hydrate-modern-intent "d-0020"))
        (d21 (h/hydrate-modern-intent "d-0021"))
        (d22 (h/hydrate-modern-intent "d-0022"))
        (d23 (h/hydrate-modern-intent "d-0023"))
        (d24 (h/hydrate-modern-intent "d-0024"))
        (d25 (h/hydrate-modern-intent "d-0025"))
        (d26 (h/hydrate-modern-intent "d-0026"))
        (d27 (h/hydrate-modern-intent "d-0027"))
        (d28 (h/hydrate-modern-intent "d-0028"))
        (d29 (h/hydrate-modern-intent "d-0029"))
        (all-decisions (h/default-modern-decisions))]
    (assert (= (list-length all-decisions) 11) "Default modern decisions must contain exactly 11 entries")
    (assert (string-starts-with? d19 "d-0019") "d-0019 must begin with identifier prefix")
    (assert (string-contains? d19 "Differential Test Runner") "d-0019 must hydrate differential test runner decision")
    (assert (string-starts-with? d20 "d-0020") "d-0020 must begin with identifier prefix")
    (assert (string-contains? d20 "Token Regex Engine") "d-0020 must hydrate token regex engine decision")
    (assert (string-starts-with? d21 "d-0021") "d-0021 must begin with identifier prefix")
    (assert (string-contains? d21 "Equivalence E-Graphs") "d-0021 must hydrate AST equivalence e-graphs decision")
    (assert (string-starts-with? d22 "d-0022") "d-0022 must begin with identifier prefix")
    (assert (string-contains? d22 "Speculative VFS Branching") "d-0022 must hydrate speculative VFS branching decision")
    (assert (string-starts-with? d23 "d-0023") "d-0023 must begin with identifier prefix")
    (assert (string-contains? d23 "Grammar-Trie Constrained") "d-0023 must hydrate grammar-trie decoding decision")
    (assert (string-starts-with? d24 "d-0024") "d-0024 must begin with identifier prefix")
    (assert (string-contains? d24 "Memory TaskStore") "d-0024 must hydrate Memory taskstore decision")
    (assert (string-starts-with? d25 "d-0025") "d-0025 must begin with identifier prefix")
    (assert (string-contains? d25 "Supervisor & Dual-Temperature") "d-0025 must hydrate supervisor speculative racing decision")
    (assert (string-starts-with? d26 "d-0026") "d-0026 must begin with identifier prefix")
    (assert (string-contains? d26 "WASI Lowering") "d-0026 must hydrate pure ASL WASI lowering decision")
    (assert (string-starts-with? d27 "d-0027") "d-0027 must begin with identifier prefix")
    (assert (string-contains? d27 "Dense Tabular Pyramid") "d-0027 must hydrate compact tabular pyramid decision")
    (assert (string-starts-with? d28 "d-0028") "d-0028 must begin with identifier prefix")
    (assert (string-contains? d28 "Cross-Reference URIs") "d-0028 must hydrate universal cross-reference URIs decision")
    (assert (string-starts-with? d29 "d-0029") "d-0029 must begin with identifier prefix")
    (assert (string-contains? d29 "Decoupled Amnesia") "d-0029 must hydrate decoupled amnesia decision")
    (assert (= (h/hydrate-modern-intent "c-055e") "") "Liquidated legacy hash c-055e must return empty string")
    (assert (= (h/hydrate-modern-intent "d-043b") "") "Liquidated legacy hash d-043b must return empty string")
    (assert (= (h/hydrate-modern-intent "l-298e") "") "Liquidated legacy hash l-298e must return empty string")
    (assert (= (h/hydrate-modern-intent "r-ea8c") "") "Liquidated legacy hash r-ea8c must return empty string")
    (assert (= (h/hydrate-modern-intent "unknown-hash") "") "Unknown hash must return empty string without failure")
    (assert (not (string-empty? d19)) "Hydrated d-0019 must not be empty")
    (assert (not (string-empty? d29)) "Hydrated d-0029 must not be empty")
    true))

(df run-tests [] -> Bool
  :d "Executes comprehensive hydration unit test suite with 70 strict assertions."
  (and (test-compression-threshold)
       (and (test-cascade-compression)
            (and (test-cascade-compression-empty-buffer)
                 (and (test-raw-context-eviction)
                      (and (test-markdown-hydration-and-card)
                           (and (test-summary-search-and-autoloading)
                                (and (test-hydrate-intent-summary)
                                     (test-modern-intent-ledger-hydration)))))))))
