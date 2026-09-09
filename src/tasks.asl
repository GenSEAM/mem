(module asl-mem/tasks
  :d "Canonical task management, holistic task protocol, dependency DAG resolution, ownership conflict detection, and parallel wave scheduling."
  :x [TaskReceipt
      TaskRecord
      make-task-receipt
      receipt-is-verified?
      format-receipt-asn
      make-holistic-task
      make-task
      task-owns-file?
      task-owns-intersect?
      task-overlap-files
      task-overlap-blast-radius?
      task-dependencies-satisfied?
      task-parallel-eligible?
      task-is-ready?
      task-find-ready
      task-claim
      task-claim-session
      task-is-claimed-by?
      task-is-lease-expired?
      task-recover-session
      task-spawn-subtask
      task-start
      task-advance-step
      task-capture-handoff
      task-attach-receipt
      task-complete
      task-format-asn
      task-format-holistic-asn
      AnticipatedRisk
      TaskTolerance
      make-anticipated-risk
      make-task-tolerance
      task-is-within-leeway?
      task-has-escalation-trigger?
      format-anticipated-risk-asn
      format-task-tolerance-asn
      claim
      state
      settle
      task/claim
      task/state
      task/settle]
  :i [(vfs :a v)
      (asl-text/escape :a esc)])

(dfs TaskReceipt
  (:f exit-code I64 "Physical command execution exit code, must be 0 for verified success")
  (:f duration-ms I64 "Elapsed execution wall time in milliseconds")
  (:f rss-mb I64 "Resident memory consumption ceiling in megabytes")
  (:f asserts-evaluated I64 "Monotonically evaluated assertion count, must be greater than zero")
  (:f tests-passed I64 "Number of passing test cases evaluated")
  (:f tests-failed I64 "Number of failed test cases, must be zero for verified success")
  (:f mutated-files (List Str) "List of file paths physically modified during execution")
  (:f diff-hash Str "Deterministic content hash of applied modifications")
  (:f checked-invariants (List Str) "List of verified architectural invariants e.g. c-0001 c-0002 c-0003")
  (:f raw-evidence Str "Compact evidence string summarizing physical execution output"))

(df make-task-receipt [(exit-code I64) (duration-ms I64) (rss-mb I64) (asserts-evaluated I64) (tests-passed I64) (tests-failed I64) (mutated-files (List Str)) (diff-hash Str) (checked-invariants (List Str)) (raw-evidence Str)] -> TaskReceipt
  :d "Constructs a standardized physical execution TaskReceipt."
  (TaskReceipt
    :exit-code exit-code
    :duration-ms duration-ms
    :rss-mb rss-mb
    :asserts-evaluated asserts-evaluated
    :tests-passed tests-passed
    :tests-failed tests-failed
    :mutated-files mutated-files
    :diff-hash diff-hash
    :checked-invariants checked-invariants
    :raw-evidence raw-evidence))

(df receipt-is-verified? [(receipt TaskReceipt)] -> Bool
  :d "Validates exit code zero, zero test failures, and positive asserts evaluated."
  (and (= (.-exit-code receipt) 0)
       (and (= (.-tests-failed receipt) 0)
            (> (.-asserts-evaluated receipt) 0))))

(df format-receipt-asn [(receipt TaskReceipt)] -> Str
  :d "Serializes TaskReceipt into canonical ASN representation."
  (let [(files-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-mutated-files receipt)))
        (files-str (str "[" (string-join files-quoted " ") "]"))
        (inv-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-checked-invariants receipt)))
        (inv-str (str "[" (string-join inv-quoted " ") "]"))
        (esc-ev (esc/escape-asn-str (.-raw-evidence receipt)))]
    (str "(:receipt"
         " :exit " (string-from-int64 (.-exit-code receipt))
         " :duration-ms " (string-from-int64 (.-duration-ms receipt))
         " :rss-mb " (string-from-int64 (.-rss-mb receipt))
         " :asserts " (string-from-int64 (.-asserts-evaluated receipt))
         " :passed " (string-from-int64 (.-tests-passed receipt))
         " :failed " (string-from-int64 (.-tests-failed receipt))
         " :mutated " files-str
         " :hash \"" (.-diff-hash receipt) "\""
         " :invariants " inv-str
         " :evidence \"" esc-ev "\")")))

(dfs AnticipatedRisk
  (:f condition Str "Anticipated risk trigger condition")
  (:f impact Str "Potential architectural or operational impact")
  (:f prescribed-mitigation Str "Pre-approved autonomous mitigation action")
  (:f contingency-gate Str "Falsifiable verification command proving successful mitigation"))

(dfs TaskTolerance
  (:f strict-invariants (List Str) "List of inviolable constraints: c-0001 c-0002 c-0003 wire protocols gate assertions")
  (:f variance-leeway (List Str) "List of authorized autonomous adaptation dimensions within declared owns")
  (:f anticipated-risks (List AnticipatedRisk) "List of pre-computed scenario contingencies")
  (:f escalation-triggers (List Str) "List of conditions mandating immediate task stoppage and handoff"))

(df make-anticipated-risk [(condition Str) (impact Str) (mitigation Str) (gate Str)] -> AnticipatedRisk
  :d "Constructs an AnticipatedRisk contingency specification."
  (AnticipatedRisk
    :condition condition
    :impact impact
    :prescribed-mitigation mitigation
    :contingency-gate gate))

(df make-task-tolerance [(invariants (List Str)) (leeway (List Str)) (risks (List AnticipatedRisk)) (triggers (List Str))] -> TaskTolerance
  :d "Constructs a TaskTolerance record defining adaptation corridors for autonomous executors."
  (TaskTolerance
    :strict-invariants invariants
    :variance-leeway leeway
    :anticipated-risks risks
    :escalation-triggers triggers))

(df task-is-within-leeway? [(tolerance TaskTolerance) (proposed-action Str)] -> Bool
  :d "Validates that a proposed action falls within pre-approved leeway dimensions and violates zero strict invariants."
  (let [(violates (filter (fn [(inv Str)] -> Bool (= inv proposed-action)) (.-strict-invariants tolerance)))
        (permitted (filter (fn [(lee Str)] -> Bool (= lee proposed-action)) (.-variance-leeway tolerance)))]
    (and (= (list-length violates) 0)
         (> (list-length permitted) 0))))

(df task-has-escalation-trigger? [(tolerance TaskTolerance) (event-desc Str)] -> Bool
  :d "Determines if an execution event matches any mandatory escalation triggers."
  (let [(matches (filter (fn [(trig Str)] -> Bool (= trig event-desc)) (.-escalation-triggers tolerance)))]
    (> (list-length matches) 0)))

(df format-anticipated-risk-asn [(risk AnticipatedRisk)] -> Str
  :d "Serializes an AnticipatedRisk contingency into canonical ASN representation."
  (str "(:risk"
       " :condition \"" (esc/escape-asn-str (.-condition risk)) "\""
       " :impact \"" (esc/escape-asn-str (.-impact risk)) "\""
       " :mitigation \"" (esc/escape-asn-str (.-prescribed-mitigation risk)) "\""
       " :gate \"" (esc/escape-asn-str (.-contingency-gate risk)) "\")"))

(df format-task-tolerance-asn [(tolerance TaskTolerance)] -> Str
  :d "Serializes a TaskTolerance record into compact canonical ASN."
  (let [(inv-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-strict-invariants tolerance)))
        (inv-str (str "[" (string-join inv-quoted " ") "]"))
        (lee-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-variance-leeway tolerance)))
        (lee-str (str "[" (string-join lee-quoted " ") "]"))
        (risk-strs (map (fn [(r AnticipatedRisk)] -> Str (format-anticipated-risk-asn r)) (.-anticipated-risks tolerance)))
        (risk-body (string-join risk-strs " "))
        (trig-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-escalation-triggers tolerance)))
        (trig-str (str "[" (string-join trig-quoted " ") "]"))]
    (str "(:tolerance"
         " :strict " inv-str
         " :leeway " lee-str
         " :risks [" risk-body "]"
         " :escalate " trig-str ")")))

(dfs TaskRecord
  (:f id Str "Unique task identifier e.g. task-382-01")
  (:f parent-id Str "Parent task identifier for hierarchical task decomposition or empty string")
  (:f phase Str "Canonical phase identifier e.g. phase-382-holistic-task-protocol")
  (:f title Str "Human-readable task summary")
  (:f kind Str "Task output classification: code-mutation, task-spawn, audit-verdict, doc-artifact, operational")
  (:f lane Str "Target execution lane e.g. main, harness, agent-core, codec")
  (:f state Str "Lifecycle state: queued, routing, ready, in-progress, verifying, completed, failed, blocked, cancelled")
  (:f priority Str "Scheduling priority: low, normal, high, urgent")
  (:f owner-role Str "Target agent archetype: scout, planner, implementer, auditor")
  (:f created-at I64 "Epoch timestamp in milliseconds of task creation")
  (:f updated-at I64 "Epoch timestamp in milliseconds of latest state update")
  (:f started-at I64 "Epoch timestamp in milliseconds of execution initiation")
  (:f completed-at I64 "Epoch timestamp in milliseconds of terminal settlement")
  (:f owns (List Str) "Declared file ownership paths (strict boundary isolation)")
  (:f target-symbols (List Str) "List of primary functions, structs, or types modified or declared")
  (:f related-symbols (List Str) "List of dependent callers, AST references, or blast-radius impacted symbols")
  (:f depends-on (List Str) "List of prerequisite task identifiers")
  (:f gate Str "Falsifiable verification gate shell command")
  (:f why Str "Architectural rationale and invariant checked e.g. c-0001, c-0003, d-0034")
  (:f spec Str "Precise step-by-step specification of actions, constraints, and deliverables")
  (:f acceptance-criteria (List Str) "Explicit boolean properties and predicates required for acceptance")
  (:f step-index I64 "Monotonic index of active sub-step within action DAG")
  (:f action-dag (List Str) "Ordered list of sub-step action descriptions")
  (:f handoff-context Str "Serialized working memory snapshot for seamless cross-agent handoff")
  (:f receipts (List Str) "Recorded physical execution receipts")
  (:f session-id Str "Unique active session lease or empty string when unclaimed")
  (:f lease-expires-at I64 "Epoch ms timestamp when session lease expires (0 = no lease)"))

(df make-holistic-task [(id Str) (parent-id Str) (phase Str) (title Str) (kind Str) (lane Str) (state Str) (priority Str) (owner-role Str) (created-at I64) (owns (List Str)) (target-symbols (List Str)) (related-symbols (List Str)) (depends-on (List Str)) (gate Str) (why Str) (spec Str) (acceptance-criteria (List Str)) (action-dag (List Str))] -> TaskRecord
  :d "Constructs a fully-specified holistic task record."
  (TaskRecord
    :id id
    :parent-id parent-id
    :phase phase
    :title title
    :kind kind
    :lane lane
    :state state
    :priority priority
    :owner-role owner-role
    :created-at created-at
    :updated-at created-at
    :started-at 0
    :completed-at 0
    :owns owns
    :target-symbols target-symbols
    :related-symbols related-symbols
    :depends-on depends-on
    :gate gate
    :why why
    :spec spec
    :acceptance-criteria acceptance-criteria
    :step-index 0
    :action-dag action-dag
    :handoff-context ""
    :receipts (list)
    :session-id ""
    :lease-expires-at 0))

(df make-task [(id Str) (phase Str) (title Str) (lane Str) (state Str) (priority Str) (created-at I64) (owns (List Str)) (depends-on (List Str)) (gate Str) (why Str)] -> TaskRecord
  :d "Constructs a standard task record with updated-at initialized to created-at and empty receipts."
  (TaskRecord
    :id id
    :parent-id ""
    :phase phase
    :title title
    :kind "code-mutation"
    :lane lane
    :state state
    :priority priority
    :owner-role "implementer"
    :created-at created-at
    :updated-at created-at
    :started-at 0
    :completed-at 0
    :owns owns
    :target-symbols (list)
    :related-symbols (list)
    :depends-on depends-on
    :gate gate
    :why why
    :spec ""
    :acceptance-criteria (list)
    :step-index 0
    :action-dag (list)
    :handoff-context ""
    :receipts (list)
    :session-id ""
    :lease-expires-at 0))

(df task-owns-file? [(task TaskRecord) (file Str)] -> Bool
  :d "Checks if a task declared ownership over a specific file path using normalized path comparison."
  (let [(norm (v/normalize-path file))
        (owns-norm (map (fn [(f Str)] -> Str (v/normalize-path f)) (.-owns task)))]
    (list-contains? owns-norm norm)))

(df task-owns-intersect? [(t1 TaskRecord) (t2 TaskRecord)] -> Bool
  :d "Checks if two tasks declare overlapping file ownership, ignoring dash placeholder."
  (let [(files1 (filter (fn [(f Str)] -> Bool (!= f "-")) (map (fn [(f Str)] -> Str (v/normalize-path f)) (.-owns t1))))
        (files2 (filter (fn [(f Str)] -> Bool (!= f "-")) (map (fn [(f Str)] -> Str (v/normalize-path f)) (.-owns t2))))]
    (let [(common (filter (fn [(f Str)] -> Bool (list-contains? files2 f)) files1))]
      (> (list-length common) 0))))

(df task-overlap-files [(t1 TaskRecord) (t2 TaskRecord)] -> (List Str)
  :d "Returns the list of overlapping files between two tasks, ignoring dash placeholder."
  (let [(files1 (filter (fn [(f Str)] -> Bool (!= f "-")) (map (fn [(f Str)] -> Str (v/normalize-path f)) (.-owns t1))))
        (files2 (filter (fn [(f Str)] -> Bool (!= f "-")) (map (fn [(f Str)] -> Str (v/normalize-path f)) (.-owns t2))))]
    (filter (fn [(f Str)] -> Bool (list-contains? files2 f)) files1)))

(df task-overlap-blast-radius? [(t1 TaskRecord) (t2 TaskRecord)] -> Bool
  :d "Detects AST symbol and caller blast-radius collisions between tasks."
  (let [(t1-targets (filter (fn [(s Str)] -> Bool (!= s "")) (.-target-symbols t1)))
        (t2-targets (filter (fn [(s Str)] -> Bool (!= s "")) (.-target-symbols t2)))
        (t1-related (filter (fn [(s Str)] -> Bool (!= s "")) (.-related-symbols t1)))
        (t2-related (filter (fn [(s Str)] -> Bool (!= s "")) (.-related-symbols t2)))
        (target-collision (filter (fn [(s Str)] -> Bool (list-contains? t2-targets s)) t1-targets))
        (blast1 (filter (fn [(s Str)] -> Bool (list-contains? t2-related s)) t1-targets))
        (blast2 (filter (fn [(s Str)] -> Bool (list-contains? t1-related s)) t2-targets))]
    (or (> (list-length target-collision) 0)
        (or (> (list-length blast1) 0)
            (> (list-length blast2) 0)))))

(df task-dependencies-satisfied? [(task TaskRecord) (completed-ids (List Str))] -> Bool
  :d "Checks if all prerequisite tasks of a task have been completed."
  (let [(missing (filter (fn [(dep Str)] -> Bool (not (list-contains? completed-ids dep))) (.-depends-on task)))]
    (list-empty? missing)))

(df task-parallel-eligible? [(t1 TaskRecord) (t2 TaskRecord)] -> Bool
  :d "Determines if two tasks can be safely executed in parallel without dependency, file ownership, or blast-radius symbol conflict."
  (and (!= (.-id t1) (.-id t2))
       (and (not (list-contains? (.-depends-on t1) (.-id t2)))
            (and (not (list-contains? (.-depends-on t2) (.-id t1)))
                 (and (not (task-owns-intersect? t1 t2))
                      (not (task-overlap-blast-radius? t1 t2)))))))

(df task-is-ready? [(task TaskRecord) (completed-ids (List Str))] -> Bool
  :d "Checks if a task is queued or ready and has all its prerequisites satisfied."
  (and (or (= (.-state task) "queued")
           (= (.-state task) "ready"))
       (task-dependencies-satisfied? task completed-ids)))

(df task-find-ready [(tasks (List TaskRecord)) (completed-ids (List Str))] -> (List TaskRecord)
  :d "Filters tasks that are eligible for immediate execution."
  (filter (fn [(t TaskRecord)] -> Bool (task-is-ready? t completed-ids)) tasks))

(df task-claim [(task TaskRecord) (now-epoch I64)] -> TaskRecord
  :d "Transitions task to claimed state and updates timestamp."
  (TaskRecord
    :id (.-id task)
    :parent-id (.-parent-id task)
    :phase (.-phase task)
    :title (.-title task)
    :kind (.-kind task)
    :lane (.-lane task)
    :state "claimed"
    :priority (.-priority task)
    :owner-role (.-owner-role task)
    :created-at (.-created-at task)
    :updated-at now-epoch
    :started-at (.-started-at task)
    :completed-at (.-completed-at task)
    :owns (.-owns task)
    :target-symbols (.-target-symbols task)
    :related-symbols (.-related-symbols task)
    :depends-on (.-depends-on task)
    :gate (.-gate task)
    :why (.-why task)
    :spec (.-spec task)
    :acceptance-criteria (.-acceptance-criteria task)
    :step-index (.-step-index task)
    :action-dag (.-action-dag task)
    :handoff-context (.-handoff-context task)
    :receipts (.-receipts task)
    :session-id (.-session-id task)
    :lease-expires-at (.-lease-expires-at task)))

(df task-claim-session [(task TaskRecord) (session-id Str) (lease-ms I64) (now-epoch I64)] -> TaskRecord
  :d "Claims task exclusively for a specific session ID with time-bounded lease."
  (TaskRecord
    :id (.-id task)
    :parent-id (.-parent-id task)
    :phase (.-phase task)
    :title (.-title task)
    :kind (.-kind task)
    :lane (.-lane task)
    :state "claimed"
    :priority (.-priority task)
    :owner-role (.-owner-role task)
    :created-at (.-created-at task)
    :updated-at now-epoch
    :started-at (.-started-at task)
    :completed-at (.-completed-at task)
    :owns (.-owns task)
    :target-symbols (.-target-symbols task)
    :related-symbols (.-related-symbols task)
    :depends-on (.-depends-on task)
    :gate (.-gate task)
    :why (.-why task)
    :spec (.-spec task)
    :acceptance-criteria (.-acceptance-criteria task)
    :step-index (.-step-index task)
    :action-dag (.-action-dag task)
    :handoff-context (.-handoff-context task)
    :receipts (.-receipts task)
    :session-id session-id
    :lease-expires-at (+ now-epoch lease-ms)))

(df task-is-claimed-by? [(task TaskRecord) (session-id Str)] -> Bool
  :d "Checks if task is actively claimed by a specific session."
  (= (.-session-id task) session-id))

(df task-is-lease-expired? [(task TaskRecord) (now-epoch I64)] -> Bool
  :d "Checks if a session lease has expired."
  (and (> (.-lease-expires-at task) 0)
       (>= now-epoch (.-lease-expires-at task))))

(df task-recover-session [(task TaskRecord) (new-session-id Str) (lease-ms I64) (now-epoch I64)] -> TaskRecord
  :d "Recovers an abandoned or crashed task for a new session, preserving step-index and handoff-context."
  (TaskRecord
    :id (.-id task)
    :parent-id (.-parent-id task)
    :phase (.-phase task)
    :title (.-title task)
    :kind (.-kind task)
    :lane (.-lane task)
    :state "in-progress"
    :priority (.-priority task)
    :owner-role (.-owner-role task)
    :created-at (.-created-at task)
    :updated-at now-epoch
    :started-at (if (= (.-started-at task) 0) now-epoch (.-started-at task))
    :completed-at (.-completed-at task)
    :owns (.-owns task)
    :target-symbols (.-target-symbols task)
    :related-symbols (.-related-symbols task)
    :depends-on (.-depends-on task)
    :gate (.-gate task)
    :why (.-why task)
    :spec (.-spec task)
    :acceptance-criteria (.-acceptance-criteria task)
    :step-index (.-step-index task)
    :action-dag (.-action-dag task)
    :handoff-context (.-handoff-context task)
    :receipts (.-receipts task)
    :session-id new-session-id
    :lease-expires-at (+ now-epoch lease-ms)))

(df task-spawn-subtask [(parent TaskRecord) (subtask-id Str) (title Str) (owns (List Str)) (now-epoch I64)] -> TaskRecord
  :d "Spawns a discovered child task linked to parent task in queued state."
  (TaskRecord
    :id subtask-id
    :parent-id (.-id parent)
    :phase (.-phase parent)
    :title title
    :kind "task-spawn"
    :lane (.-lane parent)
    :state "queued"
    :priority (.-priority parent)
    :owner-role (.-owner-role parent)
    :created-at now-epoch
    :updated-at now-epoch
    :started-at 0
    :completed-at 0
    :owns owns
    :target-symbols (list)
    :related-symbols (list)
    :depends-on (list (.-id parent))
    :gate (.-gate parent)
    :why (str "Discovered sub-task derived from " (.-id parent))
    :spec (str "Sub-task dynamically spawned during execution of " (.-id parent))
    :acceptance-criteria (list "exit-code 0")
    :step-index 0
    :action-dag (list "Execute discovered sub-task")
    :handoff-context ""
    :receipts (list)
    :session-id ""
    :lease-expires-at 0))

(df task-start [(task TaskRecord) (now-epoch I64)] -> TaskRecord
  :d "Transitions task to in-progress state, sets started-at, and updates timestamp."
  (TaskRecord
    :id (.-id task)
    :parent-id (.-parent-id task)
    :phase (.-phase task)
    :title (.-title task)
    :kind (.-kind task)
    :lane (.-lane task)
    :state "in-progress"
    :priority (.-priority task)
    :owner-role (.-owner-role task)
    :created-at (.-created-at task)
    :updated-at now-epoch
    :started-at now-epoch
    :completed-at (.-completed-at task)
    :owns (.-owns task)
    :target-symbols (.-target-symbols task)
    :related-symbols (.-related-symbols task)
    :depends-on (.-depends-on task)
    :gate (.-gate task)
    :why (.-why task)
    :spec (.-spec task)
    :acceptance-criteria (.-acceptance-criteria task)
    :step-index (.-step-index task)
    :action-dag (.-action-dag task)
    :handoff-context (.-handoff-context task)
    :receipts (.-receipts task)
    :session-id (.-session-id task)
    :lease-expires-at (.-lease-expires-at task)))

(df task-advance-step [(task TaskRecord) (now-epoch I64)] -> TaskRecord
  :d "Monotonically advances task execution step counter."
  (TaskRecord
    :id (.-id task)
    :parent-id (.-parent-id task)
    :phase (.-phase task)
    :title (.-title task)
    :kind (.-kind task)
    :lane (.-lane task)
    :state (.-state task)
    :priority (.-priority task)
    :owner-role (.-owner-role task)
    :created-at (.-created-at task)
    :updated-at now-epoch
    :started-at (.-started-at task)
    :completed-at (.-completed-at task)
    :owns (.-owns task)
    :target-symbols (.-target-symbols task)
    :related-symbols (.-related-symbols task)
    :depends-on (.-depends-on task)
    :gate (.-gate task)
    :why (.-why task)
    :spec (.-spec task)
    :acceptance-criteria (.-acceptance-criteria task)
    :step-index (+ (.-step-index task) 1)
    :action-dag (.-action-dag task)
    :handoff-context (.-handoff-context task)
    :receipts (.-receipts task)
    :session-id (.-session-id task)
    :lease-expires-at (.-lease-expires-at task)))

(df task-capture-handoff [(task TaskRecord) (context-snapshot Str) (now-epoch I64)] -> TaskRecord
  :d "Captures working memory context snapshot for agent handoff."
  (TaskRecord
    :id (.-id task)
    :parent-id (.-parent-id task)
    :phase (.-phase task)
    :title (.-title task)
    :kind (.-kind task)
    :lane (.-lane task)
    :state (.-state task)
    :priority (.-priority task)
    :owner-role (.-owner-role task)
    :created-at (.-created-at task)
    :updated-at now-epoch
    :started-at (.-started-at task)
    :completed-at (.-completed-at task)
    :owns (.-owns task)
    :target-symbols (.-target-symbols task)
    :related-symbols (.-related-symbols task)
    :depends-on (.-depends-on task)
    :gate (.-gate task)
    :why (.-why task)
    :spec (.-spec task)
    :acceptance-criteria (.-acceptance-criteria task)
    :step-index (.-step-index task)
    :action-dag (.-action-dag task)
    :handoff-context context-snapshot
    :receipts (.-receipts task)
    :session-id (.-session-id task)
    :lease-expires-at (.-lease-expires-at task)))

(df task-attach-receipt [(task TaskRecord) (receipt TaskReceipt) (now-epoch I64)] -> TaskRecord
  :d "Attaches verified execution receipt and sets completion timestamp."
  (let [(r-asn (format-receipt-asn receipt))
        (new-state (if (receipt-is-verified? receipt) "completed" "failed"))
        (comp-ts (if (receipt-is-verified? receipt) now-epoch 0))]
    (TaskRecord
      :id (.-id task)
      :parent-id (.-parent-id task)
      :phase (.-phase task)
      :title (.-title task)
      :kind (.-kind task)
      :lane (.-lane task)
      :state new-state
      :priority (.-priority task)
      :owner-role (.-owner-role task)
      :created-at (.-created-at task)
      :updated-at now-epoch
      :started-at (.-started-at task)
      :completed-at comp-ts
      :owns (.-owns task)
      :target-symbols (.-target-symbols task)
      :related-symbols (.-related-symbols task)
      :depends-on (.-depends-on task)
      :gate (.-gate task)
      :why (.-why task)
      :spec (.-spec task)
      :acceptance-criteria (.-acceptance-criteria task)
      :step-index (.-step-index task)
      :action-dag (.-action-dag task)
      :handoff-context (.-handoff-context task)
      :receipts (list-append (.-receipts task) (list r-asn))
      :session-id (.-session-id task)
      :lease-expires-at (.-lease-expires-at task))))

(df task-complete [(task TaskRecord) (receipt Str) (now-epoch I64)] -> TaskRecord
  :d "Transitions task to completed state, attaches receipt, and updates timestamp."
  (TaskRecord
    :id (.-id task)
    :parent-id (.-parent-id task)
    :phase (.-phase task)
    :title (.-title task)
    :kind (.-kind task)
    :lane (.-lane task)
    :state "completed"
    :priority (.-priority task)
    :owner-role (.-owner-role task)
    :created-at (.-created-at task)
    :updated-at now-epoch
    :started-at (.-started-at task)
    :completed-at now-epoch
    :owns (.-owns task)
    :target-symbols (.-target-symbols task)
    :related-symbols (.-related-symbols task)
    :depends-on (.-depends-on task)
    :gate (.-gate task)
    :why (.-why task)
    :spec (.-spec task)
    :acceptance-criteria (.-acceptance-criteria task)
    :step-index (.-step-index task)
    :action-dag (.-action-dag task)
    :handoff-context (.-handoff-context task)
    :receipts (list-append (.-receipts task) (list receipt))
    :session-id (.-session-id task)
    :lease-expires-at (.-lease-expires-at task)))

(df task-format-asn [(task TaskRecord)] -> Str
  :d "Formats task into canonical ASN representation."
  (let [(owns-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-owns task)))
        (owns-str (str "[" (string-join owns-quoted " ") "]"))
        (deps-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-depends-on task)))
        (deps-str (str "[" (string-join deps-quoted " ") "]"))
        (rc-str (str "[" (string-join (.-receipts task) " ") "]"))]
    (str "(:task\n"
         "  :id \"" (.-id task) "\"\n"
         "  :phase \"" (.-phase task) "\"\n"
         "  :title \"" (esc/escape-asn-str (.-title task)) "\"\n"
         "  :lane \"" (.-lane task) "\"\n"
         "  :state :" (.-state task) "\n"
         "  :priority :" (.-priority task) "\n"
         "  :created-at " (string-from-int64 (.-created-at task)) "\n"
         "  :updated-at " (string-from-int64 (.-updated-at task)) "\n"
         "  :owns " owns-str "\n"
         "  :depends-on " deps-str "\n"
         "  :gate \"" (esc/escape-asn-str (.-gate task)) "\"\n"
         "  :why \"" (esc/escape-asn-str (.-why task)) "\"\n"
         "  :receipts " rc-str ")\n")))

(df task-format-holistic-asn [(task TaskRecord)] -> Str
  :d "Formats comprehensive holistic task specification into ASN."
  (let [(owns-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-owns task)))
        (owns-str (str "[" (string-join owns-quoted " ") "]"))
        (tsym-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-target-symbols task)))
        (tsym-str (str "[" (string-join tsym-quoted " ") "]"))
        (rsym-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-related-symbols task)))
        (rsym-str (str "[" (string-join rsym-quoted " ") "]"))
        (deps-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-depends-on task)))
        (deps-str (str "[" (string-join deps-quoted " ") "]"))
        (crit-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-acceptance-criteria task)))
        (crit-str (str "[" (string-join crit-quoted " ") "]"))
        (dag-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-action-dag task)))
        (dag-str (str "[" (string-join dag-quoted " ") "]"))
        (rc-str (str "[" (string-join (.-receipts task) " ") "]"))]
    (str "(:task\n"
         "  :id \"" (.-id task) "\"\n"
         "  :parent-id \"" (.-parent-id task) "\"\n"
         "  :phase \"" (.-phase task) "\"\n"
         "  :title \"" (esc/escape-asn-str (.-title task)) "\"\n"
         "  :kind :" (.-kind task) "\n"
         "  :lane \"" (.-lane task) "\"\n"
         "  :state :" (.-state task) "\n"
         "  :priority :" (.-priority task) "\n"
         "  :owner-role \"" (.-owner-role task) "\"\n"
         "  :created-at " (string-from-int64 (.-created-at task)) "\n"
         "  :updated-at " (string-from-int64 (.-updated-at task)) "\n"
         "  :started-at " (string-from-int64 (.-started-at task)) "\n"
         "  :completed-at " (string-from-int64 (.-completed-at task)) "\n"
         "  :owns " owns-str "\n"
         "  :target-symbols " tsym-str "\n"
         "  :related-symbols " rsym-str "\n"
         "  :depends-on " deps-str "\n"
         "  :gate \"" (esc/escape-asn-str (.-gate task)) "\"\n"
         "  :why \"" (esc/escape-asn-str (.-why task)) "\"\n"
         "  :spec \"" (esc/escape-asn-str (.-spec task)) "\"\n"
         "  :acceptance-criteria " crit-str "\n"
         "  :step-index " (string-from-int64 (.-step-index task)) "\n"
         "  :action-dag " dag-str "\n"
         "  :handoff-context \"" (esc/escape-asn-str (.-handoff-context task)) "\"\n"
         "  :session-id \"" (esc/escape-asn-str (.-session-id task)) "\"\n"
         "  :lease-expires-at " (string-from-int64 (.-lease-expires-at task)) "\n"
         "  :receipts " rc-str ")\n")))


(df claim [(task TaskRecord) (now-epoch I64)] -> TaskRecord
  :d "1-to-2 token alias for task-claim."
  (task-claim task now-epoch))

(df state [(task TaskRecord)] -> Str
  :d "1-to-2 token accessor for task state."
  (.-state task))

(df settle [(task TaskRecord) (receipt Str) (now-epoch I64)] -> TaskRecord
  :d "1-to-2 token alias for task-complete."
  (task-complete task receipt now-epoch))
