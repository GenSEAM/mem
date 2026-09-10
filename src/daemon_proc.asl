(module asl-mem/daemon-proc
  :d "In-memory interactive process session supervision, watchdog deadlocks, and spool buffers."
  :x [DaemonProcSession
      make-daemon-proc-session
      session-spawn
      session-spawn-timeout
      session-extend-timeout
      session-set-timeout
      session-check-timeout
      daemon-session-check-timeout
      session-find
      session-append-stdout
      session-input
      session-terminate
      session-active-count
      session-skeleton
      session-read-slice
      session-find-bm25]
  :i [(spool_search :a ss)])

(dfs DaemonProcSession
  (:f id Str "Unique process session identifier")
  (:f cmd Str "Process command binary or name")
  (:f args (List Str) "Process command argument vector")
  (:f state Str "Session lifecycle state: active, idle, or terminated")
  (:f spool-lines (List Str) "Buffered standard output lines in FIFO order")
  (:f exit-code (Option I64) "Termination exit status code if finished")
  (:f idle-ms I64 "Milliseconds elapsed since last activity")
  (:f timeout-ms I64 "Configured watchdog timeout ceiling in milliseconds")
  (:f deadlock-detected Bool "True if session exceeded idle timeout threshold"))

(df make-daemon-proc-session [(id Str) (cmd Str) (args (List Str))] -> DaemonProcSession
  :d "Constructs an active in-memory process session record with empty spool buffer."
  (DaemonProcSession
    :id id :cmd cmd :args args :state "active" :spool-lines (list)
    :exit-code (none) :idle-ms 0 :timeout-ms 900000 :deadlock-detected false))

(df session-spawn [(sessions (List DaemonProcSession)) (id Str) (cmd Str) (args (List Str))] -> (List DaemonProcSession)
  :d "Spawns and appends a new interactive process session."
  (let [(sess (make-daemon-proc-session id cmd args))]
    (list-append sessions (list sess))))

(df session-spawn-timeout [(sessions (List DaemonProcSession)) (id Str) (cmd Str) (args (List Str)) (timeout-ms I64)] -> (List DaemonProcSession)
  :d "Spawns a new interactive process session with custom watchdog timeout ceiling."
  (let [(cap (if (<= timeout-ms 0) 10000 timeout-ms))
        (sess (DaemonProcSession
                :id id :cmd cmd :args args :state "active" :spool-lines (list)
                :exit-code (none) :idle-ms 0 :timeout-ms cap :deadlock-detected false))]
    (list-append sessions (list sess))))

(df session-extend-timeout [(sessions (List DaemonProcSession)) (id Str) (extend-ms I64)] -> (List DaemonProcSession)
  :d "Dynamically extends watchdog timeout ceiling on active session resetting idle timer."
  (map (fn [(s DaemonProcSession)] -> DaemonProcSession
         (if (= (.-id s) id)
           (DaemonProcSession
             :id (.-id s) :cmd (.-cmd s) :args (.-args s) :state (.-state s)
             :spool-lines (.-spool-lines s) :exit-code (.-exit-code s) :idle-ms 0
             :timeout-ms (+ (.-timeout-ms s) extend-ms) :deadlock-detected false)
           s))
       sessions))

(df session-set-timeout [(sessions (List DaemonProcSession)) (id Str) (new-timeout-ms I64)] -> (List DaemonProcSession)
  :d "Explicitly reconfigures watchdog timeout ceiling on active session."
  (let [(cap (if (<= new-timeout-ms 0) 10000 new-timeout-ms))]
    (map (fn [(s DaemonProcSession)] -> DaemonProcSession
           (if (= (.-id s) id)
             (DaemonProcSession
               :id (.-id s) :cmd (.-cmd s) :args (.-args s) :state (.-state s)
               :spool-lines (.-spool-lines s) :exit-code (.-exit-code s) :idle-ms (.-idle-ms s)
               :timeout-ms cap :deadlock-detected (.-deadlock-detected s))
             s))
         sessions)))

(df daemon-session-check-timeout [(s DaemonProcSession)] -> DaemonProcSession
  :d "Audits session idle duration against configured timeout setting deadlock flag if breached."
  (let [(cap (.-timeout-ms s))
        (breached (if (<= cap 0) false (>= (.-idle-ms s) cap)))]
    (DaemonProcSession
      :id (.-id s) :cmd (.-cmd s) :args (.-args s)
      :state (if breached "idle" (.-state s))
      :spool-lines (.-spool-lines s) :exit-code (.-exit-code s) :idle-ms (.-idle-ms s)
      :timeout-ms (.-timeout-ms s) :deadlock-detected breached)))

(df session-check-timeout [(s DaemonProcSession)] -> DaemonProcSession
  :d "Alias for daemon-session-check-timeout."
  (daemon-session-check-timeout s))

(df session-find [(sessions (List DaemonProcSession)) (id Str)] -> (Option DaemonProcSession)
  :d "Retrieves a process session by identifier."
  (let [(matches (filter (fn [(s DaemonProcSession)] -> Bool (= (.-id s) id)) sessions))]
    (list-head matches)))

(df session-append-stdout [(sessions (List DaemonProcSession)) (id Str) (line Str)] -> (List DaemonProcSession)
  :d "Buffers standard output line to session ring buffer."
  (map (fn [(s DaemonProcSession)] -> DaemonProcSession
         (if (= (.-id s) id)
           (DaemonProcSession
             :id (.-id s) :cmd (.-cmd s) :args (.-args s) :state (.-state s)
             :spool-lines (list-append (.-spool-lines s) (list line))
             :exit-code (.-exit-code s) :idle-ms 0 :timeout-ms (.-timeout-ms s) :deadlock-detected false)
           s))
       sessions))

(df session-input [(sessions (List DaemonProcSession)) (id Str) (input-data Str)] -> (List DaemonProcSession)
  :d "Tracks stdin transmission resetting idle timer."
  (map (fn [(s DaemonProcSession)] -> DaemonProcSession
         (if (= (.-id s) id)
           (DaemonProcSession
             :id (.-id s) :cmd (.-cmd s) :args (.-args s) :state (.-state s)
             :spool-lines (.-spool-lines s) :exit-code (.-exit-code s) :idle-ms 0
             :timeout-ms (.-timeout-ms s) :deadlock-detected false)
           s))
       sessions))

(df session-terminate [(sessions (List DaemonProcSession)) (id Str) (exit-code I64)] -> (List DaemonProcSession)
  :d "Transitions process session to terminated state and captures exit code."
  (map (fn [(s DaemonProcSession)] -> DaemonProcSession
         (if (= (.-id s) id)
           (DaemonProcSession
             :id (.-id s) :cmd (.-cmd s) :args (.-args s) :state "terminated"
             :spool-lines (.-spool-lines s) :exit-code (some exit-code) :idle-ms (.-idle-ms s)
             :timeout-ms (.-timeout-ms s) :deadlock-detected (.-deadlock-detected s))
           s))
       sessions))

(df session-active-count [(sessions (List DaemonProcSession))] -> I64
  :d "Returns count of currently active interactive process sessions."
  (list-length (filter (fn [(s DaemonProcSession)] -> Bool (= (.-state s) "active")) sessions)))

(df session-skeleton [(sess DaemonProcSession)] -> Str
  :d "Extracts structural output skeleton and error coordinates from session spool in memory."
  (let [(lines (.-spool-lines sess))
        (total (list-length lines))
        (err-lines (filter (fn [(l Str)] -> Bool
                             (or (string-contains? l "error")
                             (or (string-contains? l "Error")
                             (or (string-contains? l "fatal")
                                 (string-contains? l "failed")))))
                           lines))
        (err-cnt (list-length err-lines))]
    (str ":res (:proc-skeleton :id \"" (.-id sess) "\" :total-lines " (string-from-int64 total) " :errors " (string-from-int64 err-cnt) " :summary \"Lines: " (string-from-int64 total) ", Errors: " (string-from-int64 err-cnt) "\")")))

(df session-read-slice [(sess DaemonProcSession) (start-line I64) (end-line I64)] -> Str
  :d "Extracts narrow line slice from process session spool without dumping whole buffer."
  (let [(all-lines (.-spool-lines sess))
        (total (list-length all-lines))
        (s-idx (if (< start-line 1) 0 (- start-line 1)))
        (e-idx (if (> end-line total) total end-line))
        (sliced (if (>= s-idx e-idx) (list) (option-or (list-slice all-lines s-idx e-idx) (list))))
        (cnt (list-length sliced))]
    (str ":res (:proc-slice :id \"" (.-id sess) "\" :start " (string-from-int64 start-line) " :end " (string-from-int64 e-idx) " :count " (string-from-int64 cnt) " :lines " (string-from-int64 cnt) ")")))

(df session-find-bm25 [(sess DaemonProcSession) (query-str Str) (top-k I64)] -> Str
  :d "Executes sub-millisecond in-memory Okapi BM25 ranked query over process session spool."
  (let [(lines (.-spool-lines sess))
        (k (if (> top-k 0) top-k 5))
        (index (ss/spool-index-lines lines))
        (matches (ss/spool-search-index index lines query-str k))
        (cnt (list-length matches))]
    (str ":res (:proc-matches :id \"" (.-id sess) "\" :query \"" query-str "\" :total " (string-from-int64 cnt) ")")))
