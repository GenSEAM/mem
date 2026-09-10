(module asl-mem/daemon-proc-test
  :d "Pure ASL unit tests for resident memory daemon process session registry and spool buffers."
  :x [run-tests]
  :i [(daemon :a d) (daemon_proc :a dp)])

(df test-session-spawn-and-find [] -> Bool
  :d "Verifies spawning and retrieving in-memory process sessions in daemon state."
  (let [(cfg (d/make-daemon-config "/tmp/asl_test.sock" 100 true))
        (s0 (d/make-daemon-state cfg))
        (s1 (d/daemon-spawn-session s0 "sess-1" "python3" (list "-i")))
        (s2 (d/daemon-spawn-session s1 "sess-2" "cargo" (list "test")))
        (find-1 (d/daemon-find-session s2 "sess-1"))
        (find-2 (d/daemon-find-session s2 "sess-2"))
        (find-none (d/daemon-find-session s2 "sess-99"))]
    (assert (= (d/daemon-active-sessions-count s0) 0) "Initial active sessions must be 0")
    (assert (= (d/daemon-active-sessions-count s2) 2) "Active sessions after 2 spawns must be 2")
    (assert (option-is-some? find-1) "Session 1 must be found in daemon state")
    (assert (option-is-some? find-2) "Session 2 must be found in daemon state")
    (assert (option-is-none? find-none) "Non-existent session must return none")
    (let [(sess1 (option-unwrap find-1))]
      (assert (= (.-id sess1) "sess-1") "Session 1 ID must match")
      (assert (= (.-cmd sess1) "python3") "Session 1 command must match")
      (assert (= (.-state sess1) "active") "Session 1 state must be active")
      (assert (= (list-length (.-spool-lines sess1)) 0) "Session 1 spool must start empty")
      (assert (= (.-idle-ms sess1) 0) "Session 1 idle ms must start at 0"))
    true))

(df test-session-spool-and-input [] -> Bool
  :d "Verifies appending output lines and streaming input tracking in daemon state."
  (let [(cfg (d/make-daemon-config "/tmp/asl_test.sock" 100 true))
        (s0 (d/make-daemon-state cfg))
        (s1 (d/daemon-spawn-session s0 "sess-io" "bash" (list)))
        (s2 (d/daemon-append-session-stdout s1 "sess-io" "line 1: build started"))
        (s3 (d/daemon-append-session-stdout s2 "sess-io" "line 2: compilation finished"))
        (s4 (d/daemon-input-session s3 "sess-io" "exit 0\n"))
        (find-opt (d/daemon-find-session s4 "sess-io"))]
    (assert (option-is-some? find-opt) "Session must exist")
    (let [(sess (option-unwrap find-opt))]
      (assert (= (list-length (.-spool-lines sess)) 2) "Session must retain 2 spool lines")
      (assert (= (option-or (list-head (.-spool-lines sess)) "") "line 1: build started") "First line must match")
      (assert (= (option-or (list-get (.-spool-lines sess) 1) "") "line 2: compilation finished") "Second line must match")
      (assert (= (.-idle-ms sess) 0) "Input must reset idle timer to 0")
      (assert (= (.-state sess) "active") "Session must remain active"))
    true))

(df test-session-lifecycle-termination [] -> Bool
  :d "Verifies process session termination and active count tracking in daemon state."
  (let [(cfg (d/make-daemon-config "/tmp/asl_test.sock" 100 true))
        (s0 (d/make-daemon-state cfg))
        (s1 (d/daemon-spawn-session s0 "sess-term" "cat" (list)))
        (s2 (d/daemon-terminate-session s1 "sess-term" 0))
        (find-term (d/daemon-find-session s2 "sess-term"))]
    (assert (= (d/daemon-active-sessions-count s1) 1) "Session active count must be 1 before termination")
    (assert (= (d/daemon-active-sessions-count s2) 0) "Session active count must be 0 after termination")
    (assert (option-is-some? find-term) "Terminated session must remain queryable in daemon state")
    (let [(sess (option-unwrap find-term))]
      (assert (= (.-state sess) "terminated") "Session state must be terminated")
      (assert (option-is-some? (.-exit-code sess)) "Session exit code must be recorded")
      (assert (= (option-unwrap (.-exit-code sess)) 0) "Session exit code must match 0"))
    true))

(df test-batch-rpc-proc-dispatch [] -> Bool
  :d "Verifies single-roundtrip Batch RPC execution and dispatch for proc-spawn, proc-input, and proc-signal."
  (let [(cfg (d/make-daemon-config "/tmp/asl_test.sock" 100 true))
        (s0 (d/make-daemon-state cfg))
        (d-spawn (d/dispatch-rpc-op "proc-spawn" "sess-disp" s0))
        (d-input (d/dispatch-rpc-op "proc-input" "sess-disp" s0))
        (d-sig (d/dispatch-rpc-op "proc-signal" "sess-disp" s0))
        (res-spawn (d/run-asl-batch "(:batch (:proc-spawn :cmd \"cargo\" :id \"proc-sess-1\"))" s0))
        (res-input (d/run-asl-batch "(:batch (:proc-input :id \"proc-sess-1\" :data \"help\n\"))" s0))
        (res-sig (d/run-asl-batch "(:batch (:proc-signal :id \"proc-sess-1\" :sig \"TERM\"))" s0))]
    (assert (string-contains? d-spawn ":proc-spawn :id \"sess-disp\"") "Dispatch proc-spawn must route correctly")
    (assert (string-contains? d-input ":proc-input :id \"sess-disp\"") "Dispatch proc-input must route correctly")
    (assert (string-contains? d-sig ":proc-signal :id \"sess-disp\"") "Dispatch proc-signal must route correctly")
    (assert (string-contains? res-spawn ":op \"proc-spawn\" :status \"ok\"") "Batch proc-spawn must return ok")
    (assert (string-contains? res-spawn ":proc-spawned true") "Batch proc-spawn must report spawned")
    (assert (string-contains? res-spawn ":id \"proc-sess-1\"") "Batch proc-spawn must retain declared id")
    (assert (string-contains? res-input ":op \"proc-input\" :status \"ok\"") "Batch proc-input must return ok")
    (assert (string-contains? res-input ":proc-input-received true") "Batch proc-input must report received")
    (assert (string-contains? res-sig ":op \"proc-signal\" :status \"ok\"") "Batch proc-signal must return ok")
    (assert (string-contains? res-sig ":proc-signaled true") "Batch proc-signal must report signaled")
    (assert (string-contains? res-sig ":terminated true") "Batch proc-signal must report terminated")
    (assert (d/is-mutation-op? "proc-spawn") "proc-spawn must be recognized as mutation op")
    (assert (d/is-mutation-op? "proc-input") "proc-input must be recognized as mutation op")
    (assert (d/is-mutation-op? "proc-signal") "proc-signal must be recognized as mutation op")
    true))

(df test-session-skeleton-and-spool-navigation [] -> Bool
  :d "Verifies in-memory skeleton extraction, slice reading, and BM25 spool search via daemon."
  (let [(cfg (d/make-daemon-config "/tmp/asl_test.sock" 100 true))
        (s0 (d/make-daemon-state cfg))
        (s1 (d/daemon-spawn-session s0 "sess-nav" "rustc" (list)))
        (s2 (d/daemon-append-session-stdout s1 "sess-nav" "--> [1/2] Compiling module"))
        (s3 (d/daemon-append-session-stdout s2 "sess-nav" "warning: unused variable ctx"))
        (s4 (d/daemon-append-session-stdout s3 "sess-nav" "--> [2/2] Running checks"))
        (s5 (d/daemon-append-session-stdout s4 "sess-nav" "src/main.rs:12:1: error: unresolved symbol"))
        (skel (d/daemon-session-skeleton s5 "sess-nav"))
        (slice (d/daemon-session-read-slice s5 "sess-nav" 2 4))
        (find-bm (d/daemon-session-find-bm25 s5 "sess-nav" "unresolved" 3))
        (res-skel (d/run-asl-batch "(:batch (:proc-skeleton :id \"sess-nav\"))" s5))
        (res-read (d/run-asl-batch "(:batch (:proc-read :id \"sess-nav\"))" s5))
        (res-find (d/run-asl-batch "(:batch (:proc-find :id \"sess-nav\" :query \"unresolved\"))" s5))]
    (assert (string-contains? skel ":total-lines 4") "Skeleton must report 4 total lines")
    (assert (string-contains? skel ":errors 1") "Skeleton must report 1 error line")
    (assert (string-contains? slice ":start 2") "Slice must start at declared line")
    (assert (string-contains? slice ":count 2") "Slice must return 2 lines")
    (assert (string-contains? find-bm ":total 1") "BM25 query must find 1 matching line")
    (assert (string-contains? res-skel ":op \"proc-skeleton\" :status \"ok\"") "Batch proc-skeleton must return ok")
    (assert (string-contains? res-skel ":total-lines 4") "Batch proc-skeleton must reflect session lines")
    (assert (string-contains? res-read ":op \"proc-read\" :status \"ok\"") "Batch proc-read must return ok")
    (assert (string-contains? res-find ":op \"proc-find\" :status \"ok\"") "Batch proc-find must return ok")
    (assert (string-contains? res-find ":total 1") "Batch proc-find must reflect matches")
    true))

(df test-session-dynamic-timeout-management [] -> Bool
  :d "Verifies dynamic timeout extension, custom spawn timeout, and deadlock audit."
  (let [(cfg (d/make-daemon-config "/tmp/asl_test.sock" 100 true))
        (s0 (d/make-daemon-state cfg))
        (s1 (d/daemon-spawn-session-timeout s0 "sess-tout" "make" (list "-j4") 15000))
        (find-1 (d/daemon-find-session s1 "sess-tout"))
        (sess1 (option-unwrap find-1))
        (s2 (d/daemon-extend-session-timeout s1 "sess-tout" 10000))
        (find-2 (d/daemon-find-session s2 "sess-tout"))
        (sess2 (option-unwrap find-2))
        (s3 (d/daemon-set-session-timeout s2 "sess-tout" 60000))
        (find-3 (d/daemon-find-session s3 "sess-tout"))
        (sess3 (option-unwrap find-3))
        (d-tout (d/dispatch-rpc-op "proc-timeout" "sess-tout" s3))
        (res-tout (d/run-asl-batch "(:batch (:proc-timeout :id \"sess-tout\" :extend-ms 20000))" s3))
        (res-spawn-tout (d/run-asl-batch "(:batch (:proc-spawn :cmd \"cargo\" :id \"sess-t2\" :timeout-ms 30000))" s3))]
    (assert (= (.-timeout-ms sess1) 15000) "Initial session timeout must match configured value")
    (assert (= (.-timeout-ms sess2) 25000) "Extended session timeout must reflect addition")
    (assert (= (.-timeout-ms sess3) 60000) "Updated session timeout must reflect new ceiling")
    (assert (string-contains? d-tout ":proc-timeout :id \"sess-tout\"") "Dispatch proc-timeout must route correctly")
    (assert (string-contains? res-tout ":op \"proc-timeout\" :status \"ok\"") "Batch proc-timeout must return ok")
    (assert (string-contains? res-tout ":proc-timeout-updated true") "Batch proc-timeout must report updated")
    (assert (string-contains? res-tout ":extend-ms 20000") "Batch proc-timeout must reflect extension")
    (assert (string-contains? res-spawn-tout ":timeout-ms 30000") "Batch proc-spawn must reflect declared timeout")
    (assert (d/is-mutation-op? "proc-timeout") "proc-timeout must be recognized as mutation op")
    (let [(idle-sess (dp/DaemonProcSession :id "s-idle" :cmd "cat" :args (list) :state "active" :spool-lines (list) :exit-code (none) :idle-ms 12000 :timeout-ms 10000 :deadlock-detected false))
          (checked-idle (d/daemon-session-check-timeout idle-sess))
          (safe-sess (dp/DaemonProcSession :id "s-safe" :cmd "cat" :args (list) :state "active" :spool-lines (list) :exit-code (none) :idle-ms 8000 :timeout-ms 10000 :deadlock-detected false))
          (checked-safe (d/daemon-session-check-timeout safe-sess))]
      (assert (.-deadlock-detected checked-idle) "Session exceeding timeout must flag deadlock")
      (assert (not (.-deadlock-detected checked-safe)) "Session within timeout must not flag deadlock"))
    true))

(df test-daemon-mode-and-storage-modes [] -> Bool
  :d "Verifies 15m autonomous default, daemon detached mode, and storage tier configuration."
  (let [(cfg (d/make-daemon-config "/tmp/asl_test.sock" 100 true))
        (s0 (d/make-daemon-state cfg))
        (res-default (d/run-asl-batch "(:batch (:proc-spawn :cmd \"cargo\" :id \"sess-def\"))" s0))
        (res-daemon (d/run-asl-batch "(:batch (:proc-spawn :cmd \"server\" :id \"sess-srv\" :daemon true))" s0))
        (res-exec-def (d/run-asl-batch "(:batch (:exec :cmd \"ls\"))" s0))
        (res-exec-daemon (d/run-asl-batch "(:batch (:exec :cmd \"watch\" :daemon true))" s0))
        (res-storage-eph (d/run-asl-batch "(:batch (:storage :mode \"ephemeral\"))" s0))
        (res-storage-path (d/run-asl-batch "(:batch (:storage :mode \"git-native\" :path \"/tmp/asl_mem\"))" s0))
        (d-storage (d/dispatch-rpc-op "storage" "custom-path" s0))
        (daemon-sess (dp/DaemonProcSession :id "s-daemon" :cmd "srv" :args (list) :state "active" :spool-lines (list) :exit-code (none) :idle-ms 3600000 :timeout-ms 0 :deadlock-detected false))
        (checked-daemon (d/daemon-session-check-timeout daemon-sess))]
    (assert (string-contains? res-default ":timeout-ms 900000") "Default spawn must use 15m autonomous ceiling")
    (assert (string-contains? res-daemon ":daemon true") "Daemon spawn must flag daemon mode")
    (assert (string-contains? res-daemon ":timeout-ms 0") "Daemon spawn must disable timeout ceiling")
    (assert (string-contains? res-exec-def ":timeout-ms 900000") "Default exec must use 15m autonomous ceiling")
    (assert (string-contains? res-exec-daemon ":daemon true") "Daemon exec must flag daemon mode")
    (assert (not (.-deadlock-detected checked-daemon)) "Daemon session with 0 timeout must never deadlock on runtime")
    (assert (string-contains? res-storage-eph ":storage-configured true") "Storage batch must configure cleanly")
    (assert (string-contains? res-storage-eph ":mode \"ephemeral\"") "Ephemeral storage mode must be reflected")
    (assert (string-contains? res-storage-path ":path \"/tmp/asl_mem\"") "Custom storage path must be reflected")
    (assert (string-contains? d-storage ":storage") "Dispatch storage must route correctly")
    (assert (d/is-mutation-op? "storage") "storage must be recognized as mutation op")
    true))

(df run-tests [] -> Bool
  :d "Test runner entrypoint executing all daemon process registry tests."
  (and (test-session-spawn-and-find)
  (and (test-session-spool-and-input)
  (and (test-session-lifecycle-termination)
  (and (test-batch-rpc-proc-dispatch)
  (and (test-session-skeleton-and-spool-navigation)
  (and (test-session-dynamic-timeout-management)
       (test-daemon-mode-and-storage-modes))))))))
