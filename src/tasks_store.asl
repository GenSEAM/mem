(module asl-mem/tasks-store
  :d "Task record definitions, risk tolerances, receipts, and canonical ASN serialization."
  :x [TaskReceipt
      AnticipatedRisk
      TaskTolerance
      TaskRecord
      make-task-receipt
      receipt-is-verified?
      format-receipt-asn
      make-anticipated-risk
      make-task-tolerance
      task-is-within-leeway?
      task-has-escalation-trigger?
      format-anticipated-risk-asn
      format-task-tolerance-asn
      make-holistic-task
      make-task
      make-gap-task
      make-cybernetic-task
      task-format-asn
      task-format-holistic-asn
      state
      PostAction
      PostActionQueue
      make-post-action
      make-post-action-queue
      post-action-enqueue
      post-action-drain
      format-post-action-asn]
  :i [(asl-text/escape :a esc)])

(dfs TaskReceipt
  (:f exit-code I64 "Physical command execution exit code, must be 0 for verified success")
  (:f duration-ms I64 "Elapsed execution wall time in milliseconds")
  (:f rss-mb I64 "Resident set size memory high watermark in megabytes")
  (:f asserts-evaluated I64 "Count of evaluated runtime assertions, must be greater than zero")
  (:f tests-passed I64 "Total count of passing test cases")
  (:f tests-failed I64 "Total count of failing test cases, must be 0 for verified success")
  (:f mutated-files (List Str) "List of files touched by execution")
  (:f diff-hash Str "Cryptographic hash of physical code modifications or changes")
  (:f checked-invariants (List Str) "Set of verified invariant labels e.g. c-0001, d-0034")
  (:f raw-evidence Str "Sanitized snippet of stdout or physical verification output"))

(df make-task-receipt [(exit-code I64) (duration-ms I64) (rss-mb I64) (asserts-evaluated I64) (tests-passed I64) (tests-failed I64) (mutated-files (List Str)) (diff-hash Str) (checked-invariants (List Str)) (raw-evidence Str)] -> TaskReceipt
  :d "Constructs a verified TaskReceipt capturing physical execution metrics."
  (TaskReceipt
    :exit-code exit-code :duration-ms duration-ms
    :rss-mb rss-mb :asserts-evaluated asserts-evaluated
    :tests-passed tests-passed :tests-failed tests-failed
    :mutated-files mutated-files :diff-hash diff-hash
    :checked-invariants checked-invariants :raw-evidence raw-evidence))

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
  (AnticipatedRisk :condition condition :impact impact :prescribed-mitigation mitigation :contingency-gate gate))

(df make-task-tolerance [(invariants (List Str)) (leeway (List Str)) (risks (List AnticipatedRisk)) (triggers (List Str))] -> TaskTolerance
  :d "Constructs a TaskTolerance record defining adaptation corridors for autonomous executors."
  (TaskTolerance :strict-invariants invariants :variance-leeway leeway :anticipated-risks risks :escalation-triggers triggers))

(df task-is-within-leeway? [(tolerance TaskTolerance) (proposed-action Str)] -> Bool
  :d "Validates that a proposed action falls within pre-approved leeway dimensions and violates zero strict invariants."
  (let [(violates (filter (fn [(inv Str)] -> Bool (= inv proposed-action)) (.-strict-invariants tolerance)))
        (permitted (filter (fn [(lee Str)] -> Bool (= lee proposed-action)) (.-variance-leeway tolerance)))]
    (and (= (list-length violates) 0) (> (list-length permitted) 0))))

(df task-has-escalation-trigger? [(tolerance TaskTolerance) (event-desc Str)] -> Bool
  :d "Determines if an execution event matches any mandatory escalation triggers."
  (let [(matches (filter (fn [(trig Str)] -> Bool (= trig event-desc)) (.-escalation-triggers tolerance)))]
    (> (list-length matches) 0)))

(df format-anticipated-risk-asn [(risk AnticipatedRisk)] -> Str
  :d "Serializes an AnticipatedRisk contingency into canonical ASN representation."
  (str "(:risk :condition \"" (esc/escape-asn-str (.-condition risk))
       "\" :impact \"" (esc/escape-asn-str (.-impact risk))
       "\" :mitigation \"" (esc/escape-asn-str (.-prescribed-mitigation risk))
       "\" :gate \"" (esc/escape-asn-str (.-contingency-gate risk)) "\")"))

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
    (str "(:tolerance :strict " inv-str " :leeway " lee-str " :risks [" risk-body "] :escalate " trig-str ")")))

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
  (:f lease-expires-at I64 "Epoch ms timestamp when session lease expires (0 = no lease)")
  (:f effort Str "Effort dimension: s, m, l, xl")
  (:f risk Str "Risk rating dimension: low, medium, high, critical")
  (:f root-cause Str "Underlying architectural root cause for gaps or defects")
  (:f consequences (List Str) "List of downstream architectural consequences if unaddressed")
  (:f drawbacks (List Str) "Known tradeoffs, drawbacks, and costs of proposed resolution"))

(df make-holistic-task [(id Str) (parent-id Str) (phase Str) (title Str) (kind Str) (lane Str) (state Str) (priority Str) (owner-role Str) (created-at I64) (owns (List Str)) (target-symbols (List Str)) (related-symbols (List Str)) (depends-on (List Str)) (gate Str) (why Str) (spec Str) (acceptance-criteria (List Str)) (action-dag (List Str))] -> TaskRecord
  :d "Constructs a fully-specified holistic task record."
  (TaskRecord
    :id id :parent-id parent-id :phase phase :title title :kind kind :lane lane :state state :priority priority
    :owner-role owner-role :created-at created-at :updated-at created-at :started-at 0 :completed-at 0
    :owns owns :target-symbols target-symbols :related-symbols related-symbols :depends-on depends-on
    :gate gate :why why :spec spec :acceptance-criteria acceptance-criteria :step-index 0 :action-dag action-dag
    :handoff-context "" :receipts (list) :session-id "" :lease-expires-at 0
    :effort "m" :risk "low" :root-cause "" :consequences (list) :drawbacks (list)))

(df make-task [(id Str) (phase Str) (title Str) (lane Str) (state Str) (priority Str) (created-at I64) (owns (List Str)) (depends-on (List Str)) (gate Str) (why Str)] -> TaskRecord
  :d "Constructs a standard task record with updated-at initialized to created-at and empty receipts."
  (TaskRecord
    :id id :parent-id "" :phase phase :title title :kind "code-mutation" :lane lane :state state :priority priority
    :owner-role "implementer" :created-at created-at :updated-at created-at :started-at 0 :completed-at 0
    :owns owns :target-symbols (list) :related-symbols (list) :depends-on depends-on :gate gate :why why
    :spec "" :acceptance-criteria (list) :step-index 0 :action-dag (list) :handoff-context "" :receipts (list)
    :session-id "" :lease-expires-at 0
    :effort "s" :risk "low" :root-cause "" :consequences (list) :drawbacks (list)))

(df make-gap-task [(id Str) (phase Str) (title Str) (lane Str) (priority Str) (created-at I64) (owns (List Str)) (gate Str) (why Str) (effort Str) (risk Str) (root-cause Str) (consequences (List Str)) (drawbacks (List Str))] -> TaskRecord
  :d "Constructs a first-class GAP task record with cybernetic dimensions."
  (TaskRecord
    :id id :parent-id "" :phase phase :title title :kind "gap" :lane lane :state "queued" :priority priority
    :owner-role "auditor" :created-at created-at :updated-at created-at :started-at 0 :completed-at 0
    :owns owns :target-symbols (list) :related-symbols (list) :depends-on (list) :gate gate :why why
    :spec root-cause :acceptance-criteria (list "exit-code 0" "strict-falsify") :step-index 0 :action-dag (list "Diagnose gap root cause" "Apply corrective delta" "Reconcile gate")
    :handoff-context "" :receipts (list) :session-id "" :lease-expires-at 0
    :effort effort :risk risk :root-cause root-cause :consequences consequences :drawbacks drawbacks))

(df make-cybernetic-task [(id Str) (phase Str) (title Str) (kind Str) (lane Str) (state Str) (priority Str) (created-at I64) (owns (List Str)) (depends-on (List Str)) (gate Str) (why Str) (effort Str) (risk Str) (root-cause Str) (consequences (List Str)) (drawbacks (List Str))] -> TaskRecord
  :d "Constructs a full cybernetic TaskRecord with explicit effort, risk, root-cause, consequences, and drawbacks."
  (TaskRecord
    :id id :parent-id "" :phase phase :title title :kind kind :lane lane :state state :priority priority
    :owner-role "implementer" :created-at created-at :updated-at created-at :started-at 0 :completed-at 0
    :owns owns :target-symbols (list) :related-symbols (list) :depends-on depends-on :gate gate :why why
    :spec "" :acceptance-criteria (list) :step-index 0 :action-dag (list) :handoff-context "" :receipts (list)
    :session-id "" :lease-expires-at 0
    :effort effort :risk risk :root-cause root-cause :consequences consequences :drawbacks drawbacks))

(df task-format-asn [(task TaskRecord)] -> Str
  :d "Formats task into canonical ASN representation."
  (let [(owns-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-owns task)))
        (owns-str (str "[" (string-join owns-quoted " ") "]"))
        (deps-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-depends-on task)))
        (deps-str (str "[" (string-join deps-quoted " ") "]"))
        (rc-str (str "[" (string-join (.-receipts task) " ") "]"))
        (cons-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-consequences task)))
        (cons-str (str "[" (string-join cons-quoted " ") "]"))
        (draw-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-drawbacks task)))
        (draw-str (str "[" (string-join draw-quoted " ") "]"))]
    (str "(:task\n  :id \"" (.-id task) "\"\n  :phase \"" (.-phase task) "\"\n  :title \"" (esc/escape-asn-str (.-title task))
         "\"\n  :kind :" (.-kind task) "\n  :lane \"" (.-lane task) "\"\n  :state :" (.-state task) "\n  :priority :" (.-priority task)
         "\n  :effort :" (.-effort task) "\n  :risk :" (.-risk task)
         (if (= (.-root-cause task) "") "" (str "\n  :root-cause \"" (esc/escape-asn-str (.-root-cause task)) "\""))
         (if (= (list-length (.-consequences task)) 0) "" (str "\n  :consequences " cons-str))
         (if (= (list-length (.-drawbacks task)) 0) "" (str "\n  :drawbacks " draw-str))
         "\n  :created-at " (string-from-int64 (.-created-at task)) "\n  :updated-at " (string-from-int64 (.-updated-at task))
         "\n  :owns " owns-str "\n  :depends-on " deps-str "\n  :gate \"" (esc/escape-asn-str (.-gate task))
         "\"\n  :why \"" (esc/escape-asn-str (.-why task)) "\"\n  :receipts " rc-str ")\n")))

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
        (rc-str (str "[" (string-join (.-receipts task) " ") "]"))
        (cons-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-consequences task)))
        (cons-str (str "[" (string-join cons-quoted " ") "]"))
        (draw-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-drawbacks task)))
        (draw-str (str "[" (string-join draw-quoted " ") "]"))]
    (str "(:task\n  :id \"" (.-id task) "\"\n  :parent-id \"" (.-parent-id task) "\"\n  :phase \"" (.-phase task)
         "\"\n  :title \"" (esc/escape-asn-str (.-title task)) "\"\n  :kind :" (.-kind task) "\n  :lane \"" (.-lane task)
         "\"\n  :state :" (.-state task) "\n  :priority :" (.-priority task) "\n  :effort :" (.-effort task) "\n  :risk :" (.-risk task)
         "\n  :owner-role \"" (.-owner-role task)
         (if (= (.-root-cause task) "") "" (str "\n  :root-cause \"" (esc/escape-asn-str (.-root-cause task)) "\""))
         (if (= (list-length (.-consequences task)) 0) "" (str "\n  :consequences " cons-str))
         (if (= (list-length (.-drawbacks task)) 0) "" (str "\n  :drawbacks " draw-str))
         "\n  :created-at " (string-from-int64 (.-created-at task)) "\n  :updated-at " (string-from-int64 (.-updated-at task))
         "\n  :started-at " (string-from-int64 (.-started-at task)) "\n  :completed-at " (string-from-int64 (.-completed-at task))
         "\n  :owns " owns-str "\n  :target-symbols " tsym-str "\n  :related-symbols " rsym-str "\n  :depends-on " deps-str
         "\n  :gate \"" (esc/escape-asn-str (.-gate task)) "\"\n  :why \"" (esc/escape-asn-str (.-why task))
         "\"\n  :spec \"" (esc/escape-asn-str (.-spec task)) "\"\n  :acceptance-criteria " crit-str
         "\n  :step-index " (string-from-int64 (.-step-index task)) "\n  :action-dag " dag-str
         "\n  :handoff-context \"" (esc/escape-asn-str (.-handoff-context task))
         "\"\n  :session-id \"" (esc/escape-asn-str (.-session-id task))
         "\"\n  :lease-expires-at " (string-from-int64 (.-lease-expires-at task)) "\n  :receipts " rc-str ")\n")))

(df state [(task TaskRecord)] -> Str
  :d "1-to-2 token accessor for task state."
  (.-state task))

(dfs PostAction
  (:f action-id Str "Unique post-action identifier e.g. post-382-review")
  (:f trigger-state Str "Lifecycle state triggering post-action e.g. completed, failed, any")
  (:f kind Str "Post-action category: review, followup-plan, metric-audit, retrospection")
  (:f target-phase Str "Target phase or backlog collection identifier")
  (:f description Str "Specification of follow-up review or planning action")
  (:f payload Str "Context or directives to seed the consequent plan or review"))

(dfs PostActionQueue
  (:f queue-id Str "Queue identifier e.g. default-post-actions")
  (:f actions (List PostAction) "Ordered list of pending post-action items")
  (:f processed-count I64 "Count of settled post-action items"))

(df make-post-action [(action-id Str) (trigger-state Str) (kind Str) (target-phase Str) (description Str) (payload Str)] -> PostAction
  :d "Constructs a PostAction specification."
  (PostAction
    :action-id action-id
    :trigger-state trigger-state
    :kind kind
    :target-phase target-phase
    :description description
    :payload payload))

(df make-post-action-queue [(queue-id Str)] -> PostActionQueue
  :d "Constructs an empty PostActionQueue."
  (PostActionQueue
    :queue-id queue-id
    :actions (list)
    :processed-count 0))

(df post-action-enqueue [(q PostActionQueue) (action PostAction)] -> PostActionQueue
  :d "Appends a new post-action to the queue."
  (PostActionQueue
    :queue-id (.-queue-id q)
    :actions (list-append (.-actions q) (list action))
    :processed-count (.-processed-count q)))

(df post-action-drain [(q PostActionQueue)] -> PostActionQueue
  :d "Clears the active actions and advances the processed count."
  (let [(count (list-length (.-actions q)))]
    (PostActionQueue
      :queue-id (.-queue-id q)
      :actions (list)
      :processed-count (+ (.-processed-count q) count))))

(df format-post-action-asn [(action PostAction)] -> Str
  :d "Serializes a PostAction into canonical ASN representation."
  (str "(:post-action :id \"" (esc/escape-asn-str (.-action-id action)) "\""
       " :trigger \"" (esc/escape-asn-str (.-trigger-state action)) "\""
       " :kind \"" (esc/escape-asn-str (.-kind action)) "\""
       " :target-phase \"" (esc/escape-asn-str (.-target-phase action)) "\""
       " :description \"" (esc/escape-asn-str (.-description action)) "\""
       " :payload \"" (esc/escape-asn-str (.-payload action)) "\")"))
