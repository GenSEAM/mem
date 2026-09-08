(module asl-mem/roadmap
  :d "Canonical ASN Roadmap grammar, Defrecord storage schema, and lifecycle state transitions."
  :x [ItemRecord
      PhaseRecord
      LeaseRecord
      RoadmapRecord
      ClaimResult
      make-item
      make-phase
      make-lease
      make-roadmap
      is-lease-active?
      format-phase-defrecord
      format-roadmap-asn
      phase-get
      phase-register
      phase-claim
      item-complete
      phase-complete]
  :i [])

(dfs ItemRecord
  (:f id Str "Granular work item identifier e.g. 296.1")
  (:f name Str "Human-readable item title")
  (:f status Str "Execution state: pending, in-progress, completed, failed")
  (:f gate Str "Verification gate shell command"))

(dfs PhaseRecord
  (:f id Str "Canonical phase identifier e.g. phase-296-native-asl-mem-roadmap-and-phase-engine")
  (:f name Str "Human-readable phase title")
  (:f status Str "Phase lifecycle state: pending, claimed, in-progress, completed, failed")
  (:f wave Str "Wave grouping identifier e.g. Wave E")
  (:f gate Str "Composite acceptance gate shell command")
  (:f items (List ItemRecord) "Granular work item records"))

(dfs LeaseRecord
  (:f phase-id Str "Locked phase identifier")
  (:f agent-id Str "Worker agent holding exclusive execution lease")
  (:f acquired-epoch I64 "Epoch timestamp in milliseconds of lease grant")
  (:f ttl-ms I64 "Lease duration in milliseconds")
  (:f is-active Bool "True if lease is currently valid"))

(dfs RoadmapRecord
  (:f iteration Str "Active roadmap iteration name")
  (:f phases (List PhaseRecord) "Tracked phase records list")
  (:f leases (List LeaseRecord) "Active agent execution leases")
  (:f version I64 "Monotonic roadmap revision counter"))

(dfs ClaimResult
  (:f roadmap RoadmapRecord "Updated roadmap state")
  (:f lease (Option LeaseRecord) "Acquired lease record or none")
  (:f success Bool "True if claim succeeded")
  (:f reason Str "Diagnostic message on failure or success"))

(df make-item [(id Str) (name Str) (status Str) (gate Str)] -> ItemRecord
  :d "Constructs a work item record."
  (ItemRecord :id id :name name :status status :gate gate))

(df make-phase [(id Str) (name Str) (status Str) (wave Str) (gate Str) (items (List ItemRecord))] -> PhaseRecord
  :d "Constructs a phase record."
  (PhaseRecord :id id :name name :status status :wave wave :gate gate :items items))

(df make-lease [(phase-id Str) (agent-id Str) (acquired I64) (ttl I64)] -> LeaseRecord
  :d "Constructs an active phase lease record."
  (LeaseRecord :phase-id phase-id :agent-id agent-id :acquired-epoch acquired :ttl-ms ttl :is-active true))

(df make-roadmap [(iteration Str) (phases (List PhaseRecord))] -> RoadmapRecord
  :d "Constructs a roadmap record initialized at version 1 with no active leases."
  (RoadmapRecord :iteration iteration :phases phases :leases (list) :version 1))

(df is-lease-active? [(lease LeaseRecord) (now-epoch I64)] -> Bool
  :d "Checks if lease is active and unexpired."
  (and (.-is-active lease)
       (< now-epoch (+ (.-acquired-epoch lease) (.-ttl-ms lease)))))

(df format-phase-defrecord [(p PhaseRecord)] -> Str
  :d "Serializes a single phase record into a compact ASN Defrecord tuple."
  (str "    (\"" (.-id p) "\" \"" (.-name p) "\" \"" (.-status p) "\" \"" (.-wave p) "\" \"" (.-gate p) "\")"))

(df format-roadmap-asn [(rm RoadmapRecord)] -> Str
  :d "Serializes roadmap into canonical ASN using the token-compact Defrecord pattern."
  (let [(phase-lines (map (fn [(p PhaseRecord)] -> Str (format-phase-defrecord p)) (.-phases rm)))
        (body (string-join phase-lines "\n"))]
    (str "(:roadmap\n"
         "  :iteration \"" (.-iteration rm) "\"\n"
         "  :version " (string-from-int64 (.-version rm)) "\n"
         "  (:phases\n"
         "    :schema (id name status wave gate)\n"
         "    (\n"
         body "\n"
         "    )))\n")))

(df phase-get [(rm RoadmapRecord) (phase-id Str)] -> (Option PhaseRecord)
  :d "Finds a phase by its identifier."
  (let [(matches (filter (fn [(p PhaseRecord)] -> Bool (= (.-id p) phase-id)) (.-phases rm)))]
    (list-head matches)))

(df phase-register [(rm RoadmapRecord) (phase PhaseRecord)] -> RoadmapRecord
  :d "Registers or updates a phase in the roadmap, incrementing monotonic revision version."
  (let [(filtered (filter (fn [(p PhaseRecord)] -> Bool (!= (.-id p) (.-id phase))) (.-phases rm)))]
    (RoadmapRecord
      :iteration (.-iteration rm)
      :phases (list-append filtered (list phase))
      :leases (.-leases rm)
      :version (+ (.-version rm) 1))))

(df phase-claim [(rm RoadmapRecord) (phase-id Str) (agent-id Str) (now-epoch I64) (ttl-ms I64)] -> ClaimResult
  :d "Claims execution lease for a phase."
  (let [(p-opt (phase-get rm phase-id))]
    (mt p-opt
      ((none) (ClaimResult :roadmap rm :lease (none) :success false :reason "Phase not found"))
      ((some p)
       (let [(new-lease (make-lease phase-id agent-id now-epoch ttl-ms))
             (upd-phase (make-phase (.-id p) (.-name p) "claimed" (.-wave p) (.-gate p) (.-items p)))
             (other-ph (filter (fn [(ph PhaseRecord)] -> Bool (!= (.-id ph) phase-id)) (.-phases rm)))]
         (ClaimResult
           :roadmap (RoadmapRecord
                      :iteration (.-iteration rm)
                      :phases (list-append other-ph (list upd-phase))
                      :leases (list-append (.-leases rm) (list new-lease))
                      :version (+ (.-version rm) 1))
           :lease (some new-lease)
           :success true
           :reason "Lease acquired"))))))

(df item-complete [(rm RoadmapRecord) (phase-id Str) (item-id Str)] -> RoadmapRecord
  :d "Marks a specific work item within a phase as completed."
  (let [(p-opt (phase-get rm phase-id))]
    (mt p-opt
      ((none) rm)
      ((some p)
       (let [(upd-items (map (fn [(it ItemRecord)] -> ItemRecord
                               (if (= (.-id it) item-id)
                                 (make-item (.-id it) (.-name it) "completed" (.-gate it))
                                 it))
                             (.-items p)))
             (upd-phase (make-phase (.-id p) (.-name p) (.-status p) (.-wave p) (.-gate p) upd-items))
             (other-ph (filter (fn [(ph PhaseRecord)] -> Bool (!= (.-id ph) phase-id)) (.-phases rm)))]
         (RoadmapRecord
           :iteration (.-iteration rm)
           :phases (list-append other-ph (list upd-phase))
           :leases (.-leases rm)
           :version (+ (.-version rm) 1)))))))

(df phase-complete [(rm RoadmapRecord) (phase-id Str)] -> RoadmapRecord
  :d "Marks a phase as completed and releases any active leases for it."
  (let [(p-opt (phase-get rm phase-id))]
    (mt p-opt
      ((none) rm)
      ((some p)
       (let [(upd-items (map (fn [(it ItemRecord)] -> ItemRecord
                               (make-item (.-id it) (.-name it) "completed" (.-gate it)))
                             (.-items p)))
             (upd-phase (make-phase (.-id p) (.-name p) "completed" (.-wave p) (.-gate p) upd-items))
             (other-ph (filter (fn [(ph PhaseRecord)] -> Bool (!= (.-id ph) phase-id)) (.-phases rm)))
             (other-l (filter (fn [(l LeaseRecord)] -> Bool (!= (.-phase-id l) phase-id)) (.-leases rm)))]
         (RoadmapRecord
           :iteration (.-iteration rm)
           :phases (list-append other-ph (list upd-phase))
           :leases other-l
           :version (+ (.-version rm) 1)))))))
