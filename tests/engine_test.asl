(module asl-mem/engine-test
  :d "Unit tests for RingBuffer overflow policies, WAL logging, snapshotting, and StorageEngine modes."
  :x [run-tests]
  :i [(ring :a r)
      (wal :a w)
      (graph :a g)
      (snapshot :a snap)
      (engine :a eng)])

(df test-ring-buffer-overwrite [] -> Bool
  :d "Verifies that circular FIFO overwrite policy drops oldest entry when capacity is exceeded."
  (let [(buf0 (r/make-ring-buffer 3 (r/policy-overwrite-oldest)))
        (res1 (r/ring-push buf0 "item-1"))
        (res2 (r/ring-push (.-buffer res1) "item-2"))
        (res3 (r/ring-push (.-buffer res2) "item-3"))
        (is-full (r/ring-is-full? (.-buffer res3)))
        (res4 (r/ring-push (.-buffer res3) "item-4"))
        (recent (r/ring-peek-recent (.-buffer res4) 3))]
    (and is-full
         (and (.-was-evicted res4)
              (and (= (.-evicted-count (.-buffer res4)) 1)
                   (and (= (list-length recent) 3)
                        (and (= (option-or (list-head recent) "") "item-2")
                             (= (r/ring-utilization (.-buffer res4)) 100))))))))

(df test-ring-buffer-reject [] -> Bool
  :d "Verifies backpressure policy rejects entries when at capacity."
  (let [(buf0 (r/make-ring-buffer 2 (r/policy-reject)))
        (res1 (r/ring-push buf0 "a"))
        (res2 (r/ring-push (.-buffer res1) "b"))
        (res3 (r/ring-push (.-buffer res2) "c"))]
    (and (r/ring-is-full? (.-buffer res2))
         (and (.-rejected res3)
              (= (list-length (.-items (.-buffer res3))) 2)))))

(df test-wal-serialization [] -> Bool
  :d "Verifies formatting and parsing of WAL log frames."
  (let [(wal0 (w/make-wal-state "/tmp/test.wal"))
        (pair1 (w/append-wal-entry wal0 (w/op-put-vector) "v-1" "@v:{v-1|sample|[0.1,0.2]}" 1740000000))
        (entry (.-second pair1))
        (frame (w/format-wal-frame entry))
        (parsed (w/parse-wal-frame frame))]
    (mt parsed
      ((some p)
       (and (= (.-seq-num p) 1)
            (and (= (.-key p) "v-1")
                 (= (.-timestamp-epoch p) 1740000000))))
      ((none) false))))

(df test-graph-batch-and-index [] -> Bool
  :d "Verifies batch node insertion and O(1) indexed lookup."
  (let [(n1 (g/GraphNode :id "n1" :label "pkg" :content "core" :timestamp-epoch 1000 :confidence 1.0))
        (n2 (g/GraphNode :id "n2" :label "pkg" :content "ui" :timestamp-epoch 1000 :confidence 1.0))
        (e1 (g/GraphEdge :source-id "n1" :target-id "n2" :relation "imports" :weight 1.0 :timestamp-epoch 1000))
        (empty-g (g/KnowledgeGraph :nodes (list) :edges (list)))
        (g-batched (g/add-nodes-batch empty-g (list n1 n2)))
        (g-full (g/add-edges-batch g-batched (list e1)))
        (idx (g/build-graph-index g-full))
        (found-n1 (g/find-node-fast idx "n1"))
        (neighbors (g/find-neighbors-fast idx "n1"))
        (is-found (mt found-n1 ((some _) true) ((none) false)))]
    (and (= (list-length (.-nodes g-full)) 2)
         (and (= (list-length (.-edges g-full)) 1)
              (and is-found
                   (= (list-length neighbors) 1))))))

(df test-engine-lifecycle [] -> Bool
  :d "Verifies engine state transitions across storage modes."
  (let [(engine0 (eng/make-engine (eng/mode-journaled-wal) 10 "/tmp/eng.wal" "/tmp/snap.asn"))
        (node1 (g/GraphNode :id "ent-1" :label "concept" :content "Agentic RAM" :timestamp-epoch 1000 :confidence 0.99))
        (eng1 (eng/engine-put-node engine0 node1 1000))
        (eng2 (eng/engine-put-vector eng1 "v-1" "query embedding" (list 0.5 0.5) 1000))
        (stats (eng/engine-stats eng2))
        (snap-pair (eng/engine-checkpoint eng2 2000))
        (snap-text (.-second snap-pair))]
    (and (= (.-vector-count stats) 1)
         (and (= (.-node-count stats) 1)
              (and (= (.-wal-entries-committed stats) 2)
                   (and (string-contains? snap-text "@snap:{v1|")
                        (string-contains? snap-text "@v:{v-1")))))))

(df test-wal-crash-recovery [] -> Bool
  :d "Verifies that an engine recovers graph nodes, edges, deletions, and sequence number from replaying a WAL stream."
  (let [(wal-log (string-join (list
                   "@wal:{1|1000|PUT-NODE|n-1|@n:{n-1|entity|1000|1.0|Alpha}}"
                   "@wal:{2|1001|PUT-NODE|n-2|@n:{n-2|entity|1001|1.0|Beta}}"
                   "@wal:{3|1002|PUT-EDGE|n-1|@e:{n-1|n-2|connects|1.0|1002}}"
                   "@wal:{4|1003|DEL-NODE|n-1|DELETED}")
                 "\n"))
        (base-eng (eng/make-engine (eng/mode-journaled-wal) 10 "/tmp/eng.wal" "/tmp/snap.asn"))
        (recovered (eng/engine-recover base-eng wal-log))
        (stats (eng/engine-stats recovered))]
    (and (= (.-node-count stats) 1)
         (and (= (.-edge-count stats) 0)
              (= (.-wal-entries-committed stats) 4)))))

(df run-tests [] -> Bool
  :d "Executes all storage engine and ring buffer test suites."
  (fold (fn [(acc Bool) (p Bool)] -> Bool (and acc p))
        true
        (list (test-ring-buffer-overwrite)
              (test-ring-buffer-reject)
              (test-wal-serialization)
              (test-graph-batch-and-index)
              (test-engine-lifecycle)
              (test-wal-crash-recovery))))
