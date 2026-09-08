(module asl-mem/tests/vfs-sync-test
  :d "Master falsifiable verification test suite for Phase 337 VFS OCC sync, 3-way AST merge, context scoring, and boxart"
  :x [run-tests
      test-vfs-sync-occ
      test-vfs-merge-clean
      test-vfs-merge-conflict
      test-context-scoring
      test-asn-boxart
      test-slm-profile]
  :i [(vfs_sync :a vs)
      (vfs_merge :a vm)
      (context_scoring :a cs)
      (asn_boxart :a ba)
      (slm_profile :a slm)])

(df test-vfs-sync-occ [] -> Bool
  :d "Verifies optimistic concurrency control base-hash checking, drift detection, and flush safety validation."
  (let [(state0 (vs/make-sync-state "src/main.asl" "hash-base-123" 1725800000000))
        (state1 (vs/update-staged-hash state0 "hash-staged-456"))
        (safe-flush (vs/validate-flush-safety state0 "hash-base-123"))
        (conflict-flush (vs/validate-flush-safety state0 "hash-divergent-789"))
        (drift-detected (vs/detect-disk-drift state0 "hash-divergent-789" 1725800001000))
        (drift-identical (vs/detect-disk-drift state0 "hash-base-123" 1725800000000))
        (synced-state (vs/record-flush-success state1 "hash-staged-456" 1725800002000))]
    (assert (= (.-path state0) "src/main.asl") "Sync state path must normalize correctly")
    (assert (= (.-status state0) "clean") "Initial sync state status must be clean")
    (assert (= (.-status state1) "staged") "Updated sync state status must be staged")
    (assert safe-flush "Flush safety must validate when current disk hash matches pristine base hash")
    (assert (not conflict-flush) "Flush safety must reject flush when disk content has drifted from base")
    (assert drift-detected "Drift must be detected when disk hash or mtime diverged")
    (assert (not drift-identical) "Drift must not be detected when disk hash and mtime remain identical")
    (assert (= (.-status synced-state) "synced") "Recorded flush success must update status to synced")
    true))

(df test-vfs-merge-clean [] -> Bool
  :d "Verifies 3-way AST structural merge cleanly resolves disjoint non-overlapping form changes."
  (let [(base-code (str "(module test-app :x [f1 f2])\n\n"
                        "(df f1 [] -> I64 10)\n\n"
                        "(df f2 [] -> I64 20)"))
        (staged-code (str "(module test-app :x [f1 f2])\n\n"
                          "(df f1 [] -> I64 999)\n\n"
                          "(df f2 [] -> I64 20)"))
        (disk-code (str "(module test-app :x [f1 f2])\n\n"
                        "(df f1 [] -> I64 10)\n\n"
                        "(df f2 [] -> I64 888)"))
        (merge-res (vm/ast-3way-merge base-code staged-code disk-code))
        (merged-str (.-merged-content merge-res))]
    (assert (.-clean merge-res) "Disjoint form mutations must merge cleanly without conflict")
    (assert (= (list-length (.-conflicts merge-res)) 0) "Clean merge must have zero conflicting symbol IDs")
    (assert (= (.-merged-forms-count merge-res) 3) "Merged output must preserve all 3 top-level AST forms")
    (assert (string-contains? merged-str "999") "Merged output must incorporate staged mutation of f1")
    (assert (string-contains? merged-str "888") "Merged output must incorporate concurrent disk mutation of f2")
    (assert (string-contains? merged-str "module test-app") "Merged output must preserve unchanged module declaration")
    true))

(df test-vfs-merge-conflict [] -> Bool
  :d "Verifies 3-way AST structural merge emits structured conflicts when overlapping forms are modified concurrently."
  (let [(base-code (str "(df f1 [] -> I64 10)\n\n(df f2 [] -> I64 20)"))
        (staged-code (str "(df f1 [] -> I64 111)\n\n(df f2 [] -> I64 20)"))
        (disk-code (str "(df f1 [] -> I64 222)\n\n(df f2 [] -> I64 20)"))
        (merge-res (vm/ast-3way-merge base-code staged-code disk-code))
        (merged-str (.-merged-content merge-res))]
    (assert (not (.-clean merge-res)) "Concurrent divergent mutations to same form must trigger conflict")
    (assert (= (list-length (.-conflicts merge-res)) 1) "Merge must report exactly 1 conflicting symbol ID")
    (assert (list-contains? (.-conflicts merge-res) "f1") "Conflicting symbol identifier must be f1")
    (assert (string-contains? merged-str "<<<<<<< STAGED (f1)") "Conflict marker must include staged opening")
    (assert (string-contains? merged-str ">>>>>>> DISK (f1)") "Conflict marker must include disk boundary")
    (assert (string-contains? merged-str "=======") "Conflict marker must include central separator")
    true))

(df test-context-scoring [] -> Bool
  :d "Verifies multi-factor utility scoring protecting rare invariants and evicting unreferenced bait."
  (let [(inv-rec (cs/make-context-record "adr-0001" true 10.0 5 20))
        (bait-rec0 (cs/make-context-record "search-clickbait" false 2.0 0 1))
        (bait-rec1 (cs/penalize-unreferenced-matches bait-rec0 5))
        (items (list inv-rec bait-rec1))
        (evicted (cs/evict-below-threshold items 0.0))]
    (assert (> (.-utility-score inv-rec) 30.0) "Pinned architectural invariant must retain high utility score")
    (assert (< (.-utility-score bait-rec1) 0.0) "Unreferenced search match bait must receive heavy utility penalty")
    (assert (> (.-bait-penalty bait-rec1) 10.0) "Bait penalty must scale with unreferenced matches")
    (assert (= (list-length evicted) 1) "Eviction below threshold must prune bait and keep invariant")
    (assert (= (.-id (option-or (list-get evicted 0) bait-rec0)) "adr-0001") "Retained item must be invariant")
    (assert (.-is-invariant inv-rec) "Invariant flag must be preserved on scored record")
    true))

(df test-asn-boxart [] -> Bool
  :d "Verifies Unicode box-art terminal formatting for badges, trees, and DAG flow diagrams."
  (let [(badge-done (ba/format-box-badge "done" "Task Alpha"))
        (badge-active (ba/format-box-badge "running" "Task Beta"))
        (badge-failed (ba/format-box-badge "failed" "Task Gamma"))
        (tree-str (ba/render-tree-boxart "Root" (list "Child 1" "Child 2")))
        (n1 (ba/BoxNode :id "n1" :title "Compile" :state "done"))
        (n2 (ba/BoxNode :id "n2" :title "Audit" :state "running"))
        (e1 (ba/BoxEdge :from "n1" :to "n2" :label (some "depends-on")))
        (dag-str (ba/render-dag-boxart (list n1 n2) (list e1)))]
    (assert (string-contains? badge-done "[✓] Task Alpha") "Badge for done must format with checkmark")
    (assert (string-contains? badge-active "[▶] Task Beta") "Badge for running must format with play symbol")
    (assert (string-contains? badge-failed "[✗] Task Gamma") "Badge for failed must format with cross")
    (assert (string-contains? tree-str "├── Child 1") "Tree boxart must render intermediate branch")
    (assert (string-contains? tree-str "└── Child 2") "Tree boxart must render final branch")
    (assert (string-contains? dag-str "▼") "DAG boxart must render directional connector")
    true))

(df test-slm-profile [] -> Bool
  :d "Verifies compact 4-tool schema for small language models and constrained decoding request formatting."
  (let [(tools (slm/make-slm-essential-tools))
        (filtered (slm/filter-tool-schema tools (list "probe" "patch")))
        (req (slm/SlmRequest
               :model "qwen3.7-flash"
               :system-prompt "Autonomous agent executor."
               :tools tools
               :temperature 0.2
               :max-tokens 4096))
        (formatted (slm/format-slm-request req))]
    (assert (= (list-length tools) 4) "Essential SLM tooling profile must provide exactly 4 tools")
    (assert (= (list-length filtered) 2) "Filter tool schema must filter tools matching allowed list")
    (assert (string-contains? formatted "qwen3.7-flash") "Formatted SLM request must contain target model")
    (assert (string-contains? formatted "(:tool :name \"probe\"") "Formatted SLM request must include probe tool")
    (assert (string-contains? formatted "(:tool :name \"patch\"") "Formatted SLM request must include patch tool")
    (assert (string-contains? formatted "(:tool :name \"gate\"") "Formatted SLM request must include gate tool")
    true))

(df run-tests [] -> Bool
  :d "Executes all Phase 337 unit verification suites."
  (and (test-vfs-sync-occ)
       (and (test-vfs-merge-clean)
            (and (test-vfs-merge-conflict)
                 (and (test-context-scoring)
                      (and (test-asn-boxart)
                           (test-slm-profile)))))))
