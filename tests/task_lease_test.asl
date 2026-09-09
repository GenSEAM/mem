(module asl-mem/tests/task-lease-test
  :d "Unit verification suite for agent session lease heartbeat, timeout, and stealing engine."
  :x [test-lease-make
      test-lease-renew-heartbeat
      test-lease-expiration-predicate
      test-lease-stealing-active-rejected
      test-lease-stealing-expired-success
      test-lease-asn-serialization
      test-lease-modular-aliases
      run-tests]
  :i [(task_lease :a lease)])

(df test-lease-make [] -> Bool
  :d "Verifies initial session lease creation and field invariants."
  (let [(l (lease/make-session-lease "task-397-01" "sess-alpha" "implementer" 5000 10000))]
    (assert (= (.-task-id l) "task-397-01") "Task id must match")
    (assert (= (.-session-id l) "sess-alpha") "Session id must match")
    (assert (= (.-holder-role l) "implementer") "Holder role must match")
    (assert (= (.-acquired-at l) 10000) "Acquired-at must match creation epoch")
    (assert (= (.-renewed-at l) 10000) "Renewed-at must initially match creation epoch")
    (assert (= (.-expires-at l) 15000) "Expires-at must equal acquired-at plus ttl")
    (assert (= (.-ttl-ms l) 5000) "TTL must match configured duration")
    (assert (= (.-heartbeat-count l) 0) "Heartbeat count must initialize to 0")
    (assert (not (lease/is-lease-expired? l 10000)) "Lease must not be expired at creation")
    (assert (not (lease/is-lease-expired? l 14999)) "Lease must not be expired before TTL")
    true))

(df test-lease-renew-heartbeat [] -> Bool
  :d "Verifies heartbeat renewal increments counter and extends expiration."
  (let [(l0 (lease/make-session-lease "task-397-01" "sess-alpha" "implementer" 5000 10000))
        (l1 (lease/renew-session-lease l0 12000))
        (l2 (lease/renew-session-lease l1 14500))]
    (assert (= (.-heartbeat-count l1) 1) "First renewal must increment heartbeat count to 1")
    (assert (= (.-renewed-at l1) 12000) "First renewal must update renewed-at to 12000")
    (assert (= (.-expires-at l1) 17000) "First renewal must extend expiration to 12000 + 5000")
    (assert (= (.-heartbeat-count l2) 2) "Second renewal must increment heartbeat count to 2")
    (assert (= (.-renewed-at l2) 14500) "Second renewal must update renewed-at to 14500")
    (assert (= (.-expires-at l2) 19500) "Second renewal must extend expiration to 14500 + 5000")
    (assert (= (.-acquired-at l2) 10000) "Original acquired-at must be preserved across renewals")
    (assert (= (.-session-id l2) "sess-alpha") "Session ID must be preserved across renewals")
    true))

(df test-lease-expiration-predicate [] -> Bool
  :d "Verifies expiration predicate boundaries before, at, and after TTL deadline."
  (let [(l (lease/make-session-lease "task-397-01" "sess-alpha" "implementer" 3000 1000))]
    (assert (not (lease/is-lease-expired? l 999)) "Lease is not expired before creation")
    (assert (not (lease/is-lease-expired? l 1000)) "Lease is not expired at creation")
    (assert (not (lease/is-lease-expired? l 3999)) "Lease is not expired at ms 3999")
    (assert (lease/is-lease-expired? l 4000) "Lease is expired exactly at deadline 4000")
    (assert (lease/is-lease-expired? l 5000) "Lease is expired well past deadline 5000")
    true))

(df test-lease-stealing-active-rejected [] -> Bool
  :d "Verifies active non-expired lease cannot be stolen by replacement agent."
  (let [(l0 (lease/make-session-lease "task-397-01" "sess-alpha" "implementer" 5000 10000))
        (attempt (lease/steal-expired-lease l0 "sess-beta" "implementer" 6000 12000))]
    (assert (= (.-session-id attempt) "sess-alpha") "Active lease theft must be rejected, retaining original session")
    (assert (= (.-expires-at attempt) 15000) "Active lease expiration must remain unchanged")
    (assert (= (.-ttl-ms attempt) 5000) "Active lease TTL must remain unchanged")
    (assert (= (.-heartbeat-count attempt) 0) "Heartbeat count must remain unchanged on rejected theft")
    true))

(df test-lease-stealing-expired-success [] -> Bool
  :d "Verifies expired lease is deterministically reclaimed by replacement agent."
  (let [(l0 (lease/make-session-lease "task-397-01" "sess-alpha" "implementer" 5000 10000))
        (l-renewed (lease/renew-session-lease l0 11000))
        (stolen (lease/steal-expired-lease l-renewed "sess-beta" "implementer" 8000 16500))]
    (assert (lease/is-lease-expired? l-renewed 16500) "Lease must be expired at 16500")
    (assert (= (.-session-id stolen) "sess-beta") "Stolen lease must belong to new session")
    (assert (= (.-holder-role stolen) "implementer") "Stolen lease holder role must match new agent")
    (assert (= (.-acquired-at stolen) 16500) "Stolen lease acquired-at must be set to stealing epoch")
    (assert (= (.-renewed-at stolen) 16500) "Stolen lease renewed-at must be set to stealing epoch")
    (assert (= (.-expires-at stolen) 24500) "Stolen lease expires-at must be 16500 + 8000")
    (assert (= (.-ttl-ms stolen) 8000) "Stolen lease TTL must be updated to new TTL")
    (assert (= (.-heartbeat-count stolen) 0) "Stolen lease heartbeat count must reset to 0")
    (assert (not (lease/is-lease-expired? stolen 16500)) "Stolen lease must be active at acquisition")
    true))

(df test-lease-asn-serialization [] -> Bool
  :d "Verifies ASN serialization format matches machine specification."
  (let [(l (lease/make-session-lease "task-397-01" "sess-alpha" "implementer" 5000 10000))
        (asn-str (lease/format-lease-asn l))]
    (assert (string-contains? asn-str "(:lease") "Serialized output must contain (:lease tag")
    (assert (string-contains? asn-str ":task-id \"task-397-01\"") "Serialized output must contain task id")
    (assert (string-contains? asn-str ":session-id \"sess-alpha\"") "Serialized output must contain session id")
    (assert (string-contains? asn-str ":holder-role \"implementer\"") "Serialized output must contain holder role")
    (assert (string-contains? asn-str ":acquired-at 10000") "Serialized output must contain acquired-at")
    (assert (string-contains? asn-str ":expires-at 15000") "Serialized output must contain expires-at")
    (assert (string-contains? asn-str ":ttl-ms 5000") "Serialized output must contain ttl-ms")
    (assert (string-contains? asn-str ":heartbeat-count 0") "Serialized output must contain heartbeat-count")
    true))

(df test-lease-modular-aliases [] -> Bool
  :d "Verifies 1-to-2 token modular aliases execute identical semantics."
  (let [(l0 (lease/make "task-397-01" "sess-mod" "scout" 4000 20000))
        (l1 (lease/renew l0 21000))
        (expired-pre (lease/expired? l1 22000))
        (expired-post (lease/expired? l1 26000))
        (l2 (lease/steal l1 "sess-stolen" "planner" 5000 26000))
        (asn-str (lease/format l2))]
    (assert (not expired-pre) "Modular expired? must report false before deadline")
    (assert expired-post "Modular expired? must report true at deadline")
    (assert (= (.-session-id l2) "sess-stolen") "Modular steal must update session ID")
    (assert (= (.-holder-role l2) "planner") "Modular steal must update holder role")
    (assert (string-contains? asn-str "(:lease") "Modular format must return ASN")
    (assert (string-contains? asn-str "sess-stolen") "Modular format must contain new session")
    true))

(df run-tests [] -> Bool
  :d "Runs all task session lease unit tests."
  (and (test-lease-make)
       (and (test-lease-renew-heartbeat)
            (and (test-lease-expiration-predicate)
                 (and (test-lease-stealing-active-rejected)
                      (and (test-lease-stealing-expired-success)
                           (and (test-lease-asn-serialization)
                                (test-lease-modular-aliases))))))))

(run-tests)
