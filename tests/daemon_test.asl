(module asl-mem/daemon-test
  :d "Unit tests for memory daemon configuration hierarchy, LRU eviction, and untangling step."
  :x [run-tests]
  :i [(daemon :a d)])

(df test-daemon-config [] -> Bool
  :d "Verifies daemon configuration construction."
  (let [(cfg (d/make-daemon-config "/tmp/test.sock" 500 true))]
    (and (= (.-socket-path cfg) "/tmp/test.sock")
         (= (.-max-clean-buffers cfg) 500)
         (.-airgap-enabled cfg))))

(df test-lru-eviction [] -> Bool
  :d "Verifies LRU eviction count calculation."
  (let [(e1 (d/evict-lru-buffers 650 500))
        (e2 (d/evict-lru-buffers 400 500))]
    (and (= e1 150)
         (= e2 0))))

(df test-untangle-step [] -> Bool
  :d "Verifies planar untangle step calculation."
  (let [(f (d/untangle-step 3.0 4.0 5.0 0.5))]
    (= f 0.0)))

(df test-vfs-create-buffer [] -> Bool
  :d "Verifies virtual buffer initialization and dirty state marking."
  (let [(buf (d/vfs-create-buffer "src/demo.asl" "(module demo)"))]
    (and (= (.-rel-path buf) "src/demo.asl")
         (= (.-content buf) "(module demo)")
         (.-is-dirty buf)
         (= (.-last-access-epoch buf) 0))))

(df test-vfs-delete-buffer [] -> Bool
  :d "Verifies tombstone virtual buffer creation for file deletion."
  (let [(buf (d/vfs-delete-buffer "obsolete.asl"))]
    (and (= (.-rel-path buf) "obsolete.asl")
         (= (.-content buf) "")
         (.-is-dirty buf))))

(df test-vfs-patch-buffer [] -> Bool
  :d "Verifies in-memory symbol replacement patch buffer construction."
  (let [(buf (d/vfs-patch-buffer "src/demo.asl" "old-sym" "new-sym"))]
    (and (= (.-rel-path buf) "src/demo.asl")
         (= (.-content buf) "new-sym")
         (.-is-dirty buf))))

(df test-vfs-check-syntax [] -> Bool
  :d "Verifies delimiter balancing and string closure checks in virtual buffer."
  (let [(ok1 (d/vfs-check-syntax "(module test :x [a] :i [])"))
        (ok2 (d/vfs-check-syntax "(+ 1 2) ; closing )\n(str \"hello (world)\")"))
        (bad1 (d/vfs-check-syntax "(module test (:f x Str)"))
        (bad2 (d/vfs-check-syntax "())"))
        (bad3 (d/vfs-check-syntax "(str \"unclosed)"))]
    (and ok1
         ok2
         (not bad1)
         (not bad2)
         (not bad3))))

(df test-vfs-replace-all [] -> Bool
  :d "Verifies literal string replacement immune to regex and $ token corruption."
  (let [(res1 (d/vfs-replace-all "foo bar foo" "foo" "baz"))
        (res2 (d/vfs-replace-all "let x = 1;" "1" "$& $1 $price"))
        (res3 (d/vfs-replace-all "unchanged" "z" "x"))
        (res4 (d/vfs-replace-all "abc" "" "def"))]
    (and (= res1 "baz bar baz")
         (= res2 "let x = $& $1 $price;")
         (= res3 "unchanged")
         (= res4 "abc"))))

(df test-is-mutation-op [] -> Bool
  :d "Verifies detection of mutating RPC operations."
  (and (d/is-mutation-op? "edit")
       (d/is-mutation-op? "replace")
       (d/is-mutation-op? "create")
       (d/is-mutation-op? "write")
       (d/is-mutation-op? "delete")
       (d/is-mutation-op? "rm")
       (d/is-mutation-op? "patch")
       (d/is-mutation-op? "flush")
       (d/is-mutation-op? "discard")
       (d/is-mutation-op? "exec")
       (not (d/is-mutation-op? "read"))
       (not (d/is-mutation-op? "search"))
       (not (d/is-mutation-op? "ping"))
       (not (d/is-mutation-op? "status"))
       (not (d/is-mutation-op? "gate"))))

(df test-is-polyglot-ext [] -> Bool
  :d "Verifies detection of supported polyglot file extensions."
  (and (d/is-polyglot-ext? ".asl")
       (d/is-polyglot-ext? ".asn")
       (d/is-polyglot-ext? ".md")
       (d/is-polyglot-ext? ".json")
       (d/is-polyglot-ext? ".js")
       (d/is-polyglot-ext? ".mjs")
       (d/is-polyglot-ext? ".cjs")
       (d/is-polyglot-ext? ".ts")
       (d/is-polyglot-ext? ".tsx")
       (d/is-polyglot-ext? ".py")
       (d/is-polyglot-ext? ".rs")
       (d/is-polyglot-ext? ".go")
       (d/is-polyglot-ext? ".sh")
       (d/is-polyglot-ext? ".yaml")
       (d/is-polyglot-ext? ".yml")
       (d/is-polyglot-ext? ".php")
       (d/is-polyglot-ext? ".toml")
       (d/is-polyglot-ext? ".css")
       (d/is-polyglot-ext? ".html")
       (d/is-polyglot-ext? ".sql")
       (not (d/is-polyglot-ext? ".exe"))
       (not (d/is-polyglot-ext? ".zip"))
       (not (d/is-polyglot-ext? ".png"))))

(df test-clean-dead-socket-record [] -> Bool
  :d "Verifies cleanup logic for dead daemon socket and pid records."
  (let [(c1 (d/clean-dead-socket-record "/tmp/d.sock" "/tmp/d.pid" true))
        (c2 (d/clean-dead-socket-record "/tmp/d.sock" "/tmp/d.pid" false))
        (c3 (d/clean-dead-socket-record "" "" false))
        (c4 (d/clean-dead-socket-record "/tmp/d.sock" "" false))]
    (and (not c1)
         c2
         (not c3)
         c4)))

(df test-batch-step-and-result [] -> Bool
  :d "Verifies batch step and batch result record construction and failure detection."
  (let [(st-ok-val (d/st-ok))
        (st-fail-val (d/st-failed))
        (s1 (d/make-batch-step 1 "read" st-ok-val (none) (none) "(:content \"hello\")"))
        (s2 (d/make-batch-step 2 "edit" st-fail-val (some ":ERR_STRING_NOT_FOUND") (some "Target not found") ""))
        (res-ok (d/make-batch-result st-ok-val 0 (none) (none) 1 1 (list s1)))
        (res-err (d/make-batch-result st-fail-val 2 (some ":ERR_STRING_NOT_FOUND") (some "Target not found") 2 2 (list s1 s2)))]
    (and (= (.-id s1) 1)
         (= (.-op s1) "read")
         (= (.-id s2) 2)
         (= (.-op s2) "edit")
         (not (d/is-batch-failure? res-ok))
         (d/is-batch-failure? res-err)
         (= (.-failed-step-index res-err) 2)
         (= (.-executed-count res-err) 2)
         (= (.-total-steps res-err) 2))))

(df test-evaluate-batch-policy [] -> Bool
  :d "Verifies batch policy evaluation under abort and continue configurations."
  (let [(c1 (d/evaluate-batch-policy false "abort"))
        (c2 (d/evaluate-batch-policy false "continue"))
        (c3 (d/evaluate-batch-policy true "continue"))
        (c4 (d/evaluate-batch-policy true "abort"))]
    (and c1
         c2
         c3
         (not c4))))

(df run-tests [] -> Bool
  :d "Executes all daemon test suites."
  (and (test-daemon-config)
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
       (test-evaluate-batch-policy)))


