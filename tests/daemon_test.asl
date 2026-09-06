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

(df run-tests [] -> Bool
  :d "Executes all daemon test suites."
  (and (test-daemon-config)
       (test-lru-eviction)
       (test-untangle-step)))
