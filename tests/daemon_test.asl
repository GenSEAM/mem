(module asl-mem/daemon-test
  :d "Unit tests for memory daemon configuration hierarchy, LRU eviction, and untangling step."
  :x [run-tests
      test-daemon-batch-unhollowed]
  :i [(daemon :a d)])

(df test-daemon-config [] -> Bool
  :d "Verifies daemon configuration construction."
  (let [(cfg (d/make-daemon-config "/tmp/test.sock" 500 true))]
    (assert (= (.-socket-path cfg) "/tmp/test.sock") "Socket path must match")
    (assert (= (.-max-clean-buffers cfg) 500) "Max clean buffers must be 500")
    (assert (.-airgap-enabled cfg) "Airgap must be enabled")
    true))

(df test-lru-eviction [] -> Bool
  :d "Verifies LRU eviction count calculation."
  (let [(e1 (d/evict-lru-buffers 650 500))
        (e2 (d/evict-lru-buffers 400 500))]
    (assert (= e1 150) "Eviction count e1 must be 150")
    (assert (= e2 0) "Eviction count e2 must be 0")
    true))

(df test-untangle-step [] -> Bool
  :d "Verifies planar untangle step calculation."
  (let [(f (d/untangle-step 3.0 4.0 5.0 0.5))]
    (assert (= f 0.0) "Untangle force must be 0.0")
    true))

(df test-vfs-create-buffer [] -> Bool
  :d "Verifies virtual buffer initialization and dirty state marking."
  (let [(buf (d/vfs-create-buffer "src/demo.asl" "(module demo)"))]
    (assert (= (.-rel-path buf) "src/demo.asl") "Rel path must match")
    (assert (= (.-content buf) "(module demo)") "Content must match")
    (assert (.-is-dirty buf) "Buffer must be dirty")
    (assert (= (.-last-access-epoch buf) 0) "Last access epoch must be 0")
    true))

(df test-vfs-delete-buffer [] -> Bool
  :d "Verifies tombstone virtual buffer creation for file deletion."
  (let [(buf (d/vfs-delete-buffer "obsolete.asl"))]
    (assert (= (.-rel-path buf) "obsolete.asl") "Rel path must match")
    (assert (= (.-content buf) "") "Content must be empty")
    (assert (.-is-dirty buf) "Tombstone buffer must be dirty")
    true))

(df test-vfs-patch-buffer [] -> Bool
  :d "Verifies in-memory symbol replacement patch buffer construction."
  (let [(buf (d/vfs-patch-buffer "src/demo.asl" "old-sym" "new-sym"))]
    (assert (= (.-rel-path buf) "src/demo.asl") "Rel path must match")
    (assert (= (.-content buf) "new-sym") "Content must match replacement")
    (assert (.-is-dirty buf) "Buffer must be dirty")
    true))

(df test-vfs-check-syntax [] -> Bool
  :d "Verifies delimiter balancing and string closure checks in virtual buffer."
  (let [(ok1 (d/vfs-check-syntax "(module test :x [a] :i [])"))
        (ok2 (d/vfs-check-syntax "(+ 1 2) ; closing )\n(str \"hello (world)\")"))
        (bad1 (d/vfs-check-syntax "(module test (:f x Str)"))
        (bad2 (d/vfs-check-syntax "())"))
        (bad3 (d/vfs-check-syntax "(str \"unclosed)"))]
    (assert ok1 "Balanced module must pass syntax check")
    (assert ok2 "Balanced expression with comments and strings must pass")
    (assert (not bad1) "Unclosed paren must fail syntax check")
    (assert (not bad2) "Extra paren must fail syntax check")
    (assert (not bad3) "Unclosed string must fail syntax check")
    true))

(df test-vfs-replace-all [] -> Bool
  :d "Verifies literal string replacement immune to regex and $ token corruption."
  (let [(res1 (d/vfs-replace-all "foo bar foo" "foo" "baz"))
        (res2 (d/vfs-replace-all "let x = 1;" "1" "$& $1 $price"))
        (res3 (d/vfs-replace-all "unchanged" "z" "x"))
        (res4 (d/vfs-replace-all "abc" "" "def"))]
    (assert (= res1 "baz bar baz") "Multiple occurrences must be replaced")
    (assert (= res2 "let x = $& $1 $price;") "Special dollar tokens must not be interpreted")
    (assert (= res3 "unchanged") "Non-matching target must leave string unchanged")
    (assert (= res4 "abc") "Empty search target must leave string unchanged")
    true))

(df test-is-mutation-op [] -> Bool
  :d "Verifies detection of mutating RPC operations."
  (assert (d/is-mutation-op? "edit") "edit must be mutation op")
  (assert (d/is-mutation-op? "replace") "replace must be mutation op")
  (assert (d/is-mutation-op? "create") "create must be mutation op")
  (assert (d/is-mutation-op? "write") "write must be mutation op")
  (assert (d/is-mutation-op? "delete") "delete must be mutation op")
  (assert (d/is-mutation-op? "rm") "rm must be mutation op")
  (assert (d/is-mutation-op? "patch") "patch must be mutation op")
  (assert (d/is-mutation-op? "flush") "flush must be mutation op")
  (assert (d/is-mutation-op? "discard") "discard must be mutation op")
  (assert (d/is-mutation-op? "exec") "exec must be mutation op")
  (assert (not (d/is-mutation-op? "read")) "read must not be mutation op")
  (assert (not (d/is-mutation-op? "search")) "search must not be mutation op")
  (assert (not (d/is-mutation-op? "ping")) "ping must not be mutation op")
  (assert (not (d/is-mutation-op? "status")) "status must not be mutation op")
  (assert (not (d/is-mutation-op? "gate")) "gate must not be mutation op")
  true)

(df test-is-polyglot-ext [] -> Bool
  :d "Verifies detection of supported polyglot file extensions."
  (assert (d/is-polyglot-ext? ".asl") ".asl must be polyglot ext")
  (assert (d/is-polyglot-ext? ".asn") ".asn must be polyglot ext")
  (assert (d/is-polyglot-ext? ".md") ".md must be polyglot ext")
  (assert (d/is-polyglot-ext? ".json") ".json must be polyglot ext")
  (assert (d/is-polyglot-ext? ".js") ".js must be polyglot ext")
  (assert (d/is-polyglot-ext? ".mjs") ".mjs must be polyglot ext")
  (assert (d/is-polyglot-ext? ".cjs") ".cjs must be polyglot ext")
  (assert (d/is-polyglot-ext? ".ts") ".ts must be polyglot ext")
  (assert (d/is-polyglot-ext? ".tsx") ".tsx must be polyglot ext")
  (assert (d/is-polyglot-ext? ".py") ".py must be polyglot ext")
  (assert (d/is-polyglot-ext? ".rs") ".rs must be polyglot ext")
  (assert (d/is-polyglot-ext? ".go") ".go must be polyglot ext")
  (assert (d/is-polyglot-ext? ".sh") ".sh must be polyglot ext")
  (assert (d/is-polyglot-ext? ".yaml") ".yaml must be polyglot ext")
  (assert (d/is-polyglot-ext? ".yml") ".yml must be polyglot ext")
  (assert (d/is-polyglot-ext? ".php") ".php must be polyglot ext")
  (assert (d/is-polyglot-ext? ".toml") ".toml must be polyglot ext")
  (assert (d/is-polyglot-ext? ".css") ".css must be polyglot ext")
  (assert (d/is-polyglot-ext? ".html") ".html must be polyglot ext")
  (assert (d/is-polyglot-ext? ".sql") ".sql must be polyglot ext")
  (assert (not (d/is-polyglot-ext? ".exe")) ".exe must not be polyglot ext")
  (assert (not (d/is-polyglot-ext? ".zip")) ".zip must not be polyglot ext")
  (assert (not (d/is-polyglot-ext? ".png")) ".png must not be polyglot ext")
  true)

(df test-clean-dead-socket-record [] -> Bool
  :d "Verifies cleanup logic for dead daemon socket and pid records."
  (let [(c1 (d/clean-dead-socket-record "/tmp/d.sock" "/tmp/d.pid" true))
        (c2 (d/clean-dead-socket-record "/tmp/d.sock" "/tmp/d.pid" false))
        (c3 (d/clean-dead-socket-record "" "" false))
        (c4 (d/clean-dead-socket-record "/tmp/d.sock" "" false))]
    (assert (not c1) "Active socket must not be cleaned")
    (assert c2 "Dead socket and pid must be cleaned")
    (assert (not c3) "Empty socket and pid must not be cleaned")
    (assert c4 "Dead socket without pid must be cleaned")
    true))

(df test-batch-step-and-result [] -> Bool
  :d "Verifies batch step and batch result record construction and failure detection."
  (let [(st-ok-val (d/st-ok))
        (st-fail-val (d/st-failed))
        (s1 (d/make-batch-step 1 "read" st-ok-val (none) (none) "(:content \"hello\")"))
        (s2 (d/make-batch-step 2 "edit" st-fail-val (some ":ERR_STRING_NOT_FOUND") (some "Target not found") ""))
        (res-ok (d/make-batch-result st-ok-val 0 (none) (none) 1 1 (list s1)))
        (res-err (d/make-batch-result st-fail-val 2 (some ":ERR_STRING_NOT_FOUND") (some "Target not found") 2 2 (list s1 s2)))]
    (assert (= (.-id s1) 1) "Step s1 id must be 1")
    (assert (= (.-op s1) "read") "Step s1 op must be read")
    (assert (= (.-id s2) 2) "Step s2 id must be 2")
    (assert (= (.-op s2) "edit") "Step s2 op must be edit")
    (assert (not (d/is-batch-failure? res-ok)) "res-ok must not be batch failure")
    (assert (d/is-batch-failure? res-err) "res-err must be batch failure")
    (assert (= (.-failed-step-index res-err) 2) "Failed step index must be 2")
    (assert (= (.-executed-count res-err) 2) "Executed count must be 2")
    (assert (= (.-total-steps res-err) 2) "Total steps must be 2")
    true))

(df test-evaluate-batch-policy [] -> Bool
  :d "Verifies batch policy evaluation under abort and continue configurations."
  (let [(c1 (d/evaluate-batch-policy false "abort"))
        (c2 (d/evaluate-batch-policy false "continue"))
        (c3 (d/evaluate-batch-policy true "continue"))
        (c4 (d/evaluate-batch-policy true "abort"))]
    (assert c1 "No-error abort policy must evaluate to true")
    (assert c2 "No-error continue policy must evaluate to true")
    (assert c3 "Error continue policy must evaluate to true")
    (assert (not c4) "Error abort policy must evaluate to false")
    true))

(df test-run-asl-batch [] -> Bool
  :d "Verifies pure ASL batch execution engine."
  (let [(cfg (d/make-daemon-config "/tmp/asl_test.sock" 100 true))
        (state (d/make-daemon-state cfg))
        (res-ping (d/run-asl-batch "(:batch (:ping))" state))
        (res-diff (d/run-asl-batch "(:batch (:diff))" state))
        (res-flush (d/run-asl-batch "(:batch (:flush))" state))
        (res-discard (d/run-asl-batch "(:batch (:discard))" state))
        (res-gate (d/run-asl-batch "(:batch (:gate))" state))
        (res-unknown (d/run-asl-batch "(:batch (:foo-bar))" state))]
    (assert (string-contains? res-ping ":op \"ping\" :status \"ok\"") "Ping must return ok status")
    (assert (string-contains? res-ping ":pong") "Ping must return pong output")
    (assert (string-contains? res-diff ":op \"diff\" :status \"ok\"") "Diff must return ok status")
    (assert (string-contains? res-flush ":op \"flush\" :status \"ok\"") "Flush must return ok status")
    (assert (string-contains? res-discard ":op \"discard\" :status \"ok\"") "Discard must return ok status")
    (assert (string-contains? res-gate ":op \"gate\" :status \"ok\"") "Gate must return ok status")
    (assert (string-contains? res-unknown ":status \"rejected\"") "Unknown op must return rejected")
    true))

(df test-daemon-batch-unhollowed [] -> Bool
  :d "Verifies unhollowed daemon batch operations for diff, flush, callers, and impact."
  (let [(cfg (d/make-daemon-config "/tmp/asl_test.sock" 100 true))
        (dirty-buf (d/vfs-create-buffer "src/core.asl" "(module core)"))
        (st-with-dirty (d/DaemonState
                         :config cfg
                         :indexed-files-count 1
                         :symbols-count 10
                         :dirty-buffers-count 1
                         :buffers (list dirty-buf)
                         :is-ready true))
        (res-diff (d/run-asl-batch "(:batch (:diff))" st-with-dirty))
        (res-flush (d/run-asl-batch "(:batch (:flush))" st-with-dirty))
        (res-callers (d/run-asl-batch "(:batch (:callers \"render-select\"))" st-with-dirty))
        (res-impact (d/run-asl-batch "(:batch (:impact \"render-select\"))" st-with-dirty))
        (res-find (d/run-asl-batch "(:batch (:find \"render-select\"))" st-with-dirty))
        (res-sym (d/run-asl-batch "(:batch (:sym \"render-select\"))" st-with-dirty))]
    (assert (string-contains? res-diff ":dirty-files 1") "Diff must report 1 dirty file")
    (assert (string-contains? res-flush ":flushed 1") "Flush must report 1 flushed file")
    (assert (string-contains? res-callers ":status \"ok\"") "Callers must have status ok")
    (assert (not (string-contains? res-callers ":status \"rejected\"")) "Callers must not be rejected")
    (assert (string-contains? res-impact ":status \"ok\"") "Impact must have status ok")
    (assert (not (string-contains? res-impact ":status \"rejected\"")) "Impact must not be rejected")
    (assert (string-contains? res-find ":status \"ok\"") "Find must have status ok")
    (assert (string-contains? res-sym ":status \"ok\"") "Sym must have status ok")
    true))

(df run-tests [] -> Bool
  :d "Executes all daemon test suites."
  (fold (fn [(acc Bool) (p Bool)] -> Bool (and acc p))
        true
        (list (test-daemon-config)
              (test-lru-eviction)
              (test-untangle-step)
              (test-vfs-create-buffer)
              (test-vfs-delete-buffer)
              (test-vfs-patch-buffer)
              (test-vfs-check-syntax)
              (test-vfs-replace-all)
              (test-is-mutation-op)
              (test-is-polyglot-ext)
              (test-clean-dead-socket-record)
              (test-batch-step-and-result)
              (test-evaluate-batch-policy)
              (test-run-asl-batch)
              (test-daemon-batch-unhollowed))))


