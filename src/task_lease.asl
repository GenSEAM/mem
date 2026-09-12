(module asl-mem/task-lease
  :d "Multi-agent session lease management, heartbeat renewal, and deterministic timeout stealing engine per d45."
  :x [SessionLease
      make-session-lease
      renew-session-lease
      is-lease-expired?
      steal-expired-lease
      format-lease-asn
      make
      renew
      expired?
      steal
      format-lease]
  :i [(asl-text/escape :a esc)])

(dfs SessionLease
  (:f task-id Str "Target task identifier e.g. task-397-01")
  (:f session-id Str "Active agent session identifier e.g. agent-alpha")
  (:f holder-role Str "Agent archetype holding lease: scout, planner, implementer, auditor")
  (:f acquired-at I64 "Epoch timestamp in milliseconds when lease was initially acquired")
  (:f renewed-at I64 "Epoch timestamp in milliseconds of latest heartbeat renewal")
  (:f expires-at I64 "Epoch timestamp in milliseconds when lease expires")
  (:f ttl-ms I64 "Lease duration time-to-live in milliseconds")
  (:f heartbeat-count I64 "Total number of successful heartbeat renewals"))

(df make-session-lease [(task-id Str) (session-id Str) (holder-role Str) (ttl-ms I64) (now-epoch I64)] -> SessionLease
  :d "Constructs a new exclusive session lease with time-bounded expiration."
  (SessionLease
    :task-id task-id
    :session-id session-id
    :holder-role holder-role
    :acquired-at now-epoch
    :renewed-at now-epoch
    :expires-at (+ now-epoch ttl-ms)
    :ttl-ms ttl-ms
    :heartbeat-count 0))

(df renew-session-lease [(lease SessionLease) (now-epoch I64)] -> SessionLease
  :d "Renews an active session lease, updating heartbeat timestamp and extending expiration by TTL."
  (SessionLease
    :task-id (.-task-id lease)
    :session-id (.-session-id lease)
    :holder-role (.-holder-role lease)
    :acquired-at (.-acquired-at lease)
    :renewed-at now-epoch
    :expires-at (+ now-epoch (.-ttl-ms lease))
    :ttl-ms (.-ttl-ms lease)
    :heartbeat-count (+ (.-heartbeat-count lease) 1)))

(df is-lease-expired? [(lease SessionLease) (now-epoch I64)] -> Bool
  :d "Checks if a session lease has reached or passed its expiration timestamp."
  (and (> (.-expires-at lease) 0)
       (>= now-epoch (.-expires-at lease))))

(df steal-expired-lease [(lease SessionLease) (new-session-id Str) (new-holder-role Str) (new-ttl-ms I64) (now-epoch I64)] -> SessionLease
  :d "Reclaims an expired lease for a replacement agent session, retaining existing lease unmodified if still active."
  (if (is-lease-expired? lease now-epoch)
    (SessionLease
      :task-id (.-task-id lease)
      :session-id new-session-id
      :holder-role new-holder-role
      :acquired-at now-epoch
      :renewed-at now-epoch
      :expires-at (+ now-epoch new-ttl-ms)
      :ttl-ms new-ttl-ms
      :heartbeat-count 0)
    lease))

(df format-lease-asn [(lease SessionLease)] -> Str
  :d "Serializes session lease into canonical machine ASN notation."
  (str "(:lease\n"
       "  :task-id \"" (esc/escape-asn-str (.-task-id lease)) "\"\n"
       "  :session-id \"" (esc/escape-asn-str (.-session-id lease)) "\"\n"
       "  :holder-role \"" (esc/escape-asn-str (.-holder-role lease)) "\"\n"
       "  :acquired-at " (string-from-int64 (.-acquired-at lease)) "\n"
       "  :renewed-at " (string-from-int64 (.-renewed-at lease)) "\n"
       "  :expires-at " (string-from-int64 (.-expires-at lease)) "\n"
       "  :ttl-ms " (string-from-int64 (.-ttl-ms lease)) "\n"
       "  :heartbeat-count " (string-from-int64 (.-heartbeat-count lease)) ")\n"))

(df make [(task-id Str) (session-id Str) (holder-role Str) (ttl-ms I64) (now-epoch I64)] -> SessionLease
  :d "1-to-2 token alias for make-session-lease."
  (make-session-lease task-id session-id holder-role ttl-ms now-epoch))

(df renew [(lease SessionLease) (now-epoch I64)] -> SessionLease
  :d "1-to-2 token alias for renew-session-lease."
  (renew-session-lease lease now-epoch))

(df expired? [(lease SessionLease) (now-epoch I64)] -> Bool
  :d "1-to-2 token alias for is-lease-expired?."
  (is-lease-expired? lease now-epoch))

(df steal [(lease SessionLease) (new-session-id Str) (new-holder-role Str) (new-ttl-ms I64) (now-epoch I64)] -> SessionLease
  :d "1-to-2 token alias for steal-expired-lease."
  (steal-expired-lease lease new-session-id new-holder-role new-ttl-ms now-epoch))

(df format-lease [(lease SessionLease)] -> Str
  :d "1-to-2 token alias for format-lease-asn."
  (format-lease-asn lease))
