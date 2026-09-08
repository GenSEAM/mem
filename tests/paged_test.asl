(module asl-mem/test
  :d "Unit tests for asl-mem/paged adaptive tiered and LRU paged memory manager."
  :x [main run-tests]
  :i [(paged :a p)])

(df test-tier-decision [] -> Bool
  (assert (p/should-page? 100000000 50000000) "should page on large footprint")
  (assert (not (p/should-page? 10000000 50000000)) "should not page on small footprint")
  true)

(df test-paged-lru-lifecycle [] -> Bool
  (let [(mgr (p/make-paged-manager 2 1000 true))
        (s1 (p/make-segment "seg-1" "asl-core" 200 100 10))
        (s2 (p/make-segment "seg-2" "asl-vdom" 300 105 15))
        (s3 (p/make-segment "seg-3" "asl-mem" 400 110 20))
        (m1 (p/load-segment mgr s1 100))
        (m2 (p/load-segment m1 s2 105))
        (m3 (p/load-segment m2 s3 110))]
    (assert (= (.-max-resident-segments m3) 2) "max resident segments is 2")
    (assert (> (.-current-bytes m3) 0) "current bytes positive")
    true))

(df run-tests [] -> Bool
  (and (test-tier-decision)
       (test-paged-lru-lifecycle)))

(df ! main [(args (List Str))] -> (Result Unit IoError)
  :d "Runs unit tests for paged memory manager."
  (if (run-tests)
    (let [(u (println "asl-mem paged tests passed cleanly"))]
      (ok ()))
    (let [(u (eprintln "asl-mem paged test failure"))]
      (err (other)))))
