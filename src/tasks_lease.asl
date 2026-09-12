(module asl-mem/tasks-lease
  :d "Task claiming, multi-agent session leases, step lifecycle execution, and recovery."
  :x [task-claim
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
      claim
      settle
]
  :i [(tasks_store :a ts)])

(df task-claim [(task ts/TaskRecord) (now-epoch I64)] -> ts/TaskRecord
  :d "Transitions task to claimed state and updates timestamp."
  (ts/TaskRecord
    :id (.-id task) :parent-id (.-parent-id task) :phase (.-phase task) :title (.-title task)
    :kind (.-kind task) :lane (.-lane task) :state "claimed" :priority (.-priority task)
    :owner-role (.-owner-role task) :created-at (.-created-at task) :updated-at now-epoch
    :started-at (.-started-at task) :completed-at (.-completed-at task) :owns (.-owns task)
    :target-symbols (.-target-symbols task) :related-symbols (.-related-symbols task)
    :depends-on (.-depends-on task) :gate (.-gate task) :why (.-why task) :spec (.-spec task)
    :acceptance-criteria (.-acceptance-criteria task) :step-index (.-step-index task)
    :action-dag (.-action-dag task) :handoff-context (.-handoff-context task)
    :receipts (.-receipts task) :session-id (.-session-id task) :lease-expires-at (.-lease-expires-at task)
    :effort (.-effort task) :risk (.-risk task) :root-cause (.-root-cause task)
    :consequences (.-consequences task) :drawbacks (.-drawbacks task)))

(df task-claim-session [(task ts/TaskRecord) (session-id Str) (lease-ms I64) (now-epoch I64)] -> ts/TaskRecord
  :d "Claims task exclusively for a specific session ID with time-bounded lease."
  (ts/TaskRecord
    :id (.-id task) :parent-id (.-parent-id task) :phase (.-phase task) :title (.-title task)
    :kind (.-kind task) :lane (.-lane task) :state "claimed" :priority (.-priority task)
    :owner-role (.-owner-role task) :created-at (.-created-at task) :updated-at now-epoch
    :started-at (.-started-at task) :completed-at (.-completed-at task) :owns (.-owns task)
    :target-symbols (.-target-symbols task) :related-symbols (.-related-symbols task)
    :depends-on (.-depends-on task) :gate (.-gate task) :why (.-why task) :spec (.-spec task)
    :acceptance-criteria (.-acceptance-criteria task) :step-index (.-step-index task)
    :action-dag (.-action-dag task) :handoff-context (.-handoff-context task)
    :receipts (.-receipts task) :session-id session-id :lease-expires-at (+ now-epoch lease-ms)
    :effort (.-effort task) :risk (.-risk task) :root-cause (.-root-cause task)
    :consequences (.-consequences task) :drawbacks (.-drawbacks task)))

(df task-is-claimed-by? [(task ts/TaskRecord) (session-id Str)] -> Bool
  :d "Checks if task is actively claimed by a specific session."
  (= (.-session-id task) session-id))

(df task-is-lease-expired? [(task ts/TaskRecord) (now-epoch I64)] -> Bool
  :d "Checks if a session lease has expired."
  (and (> (.-lease-expires-at task) 0)
       (>= now-epoch (.-lease-expires-at task))))

(df task-recover-session [(task ts/TaskRecord) (new-session-id Str) (lease-ms I64) (now-epoch I64)] -> ts/TaskRecord
  :d "Recovers an abandoned or crashed task for a new session, preserving step-index and handoff-context."
  (ts/TaskRecord
    :id (.-id task) :parent-id (.-parent-id task) :phase (.-phase task) :title (.-title task)
    :kind (.-kind task) :lane (.-lane task) :state "in-progress" :priority (.-priority task)
    :owner-role (.-owner-role task) :created-at (.-created-at task) :updated-at now-epoch
    :started-at (if (= (.-started-at task) 0) now-epoch (.-started-at task))
    :completed-at (.-completed-at task) :owns (.-owns task) :target-symbols (.-target-symbols task)
    :related-symbols (.-related-symbols task) :depends-on (.-depends-on task) :gate (.-gate task)
    :why (.-why task) :spec (.-spec task) :acceptance-criteria (.-acceptance-criteria task)
    :step-index (.-step-index task) :action-dag (.-action-dag task) :handoff-context (.-handoff-context task)
    :receipts (.-receipts task) :session-id new-session-id :lease-expires-at (+ now-epoch lease-ms)
    :effort (.-effort task) :risk (.-risk task) :root-cause (.-root-cause task)
    :consequences (.-consequences task) :drawbacks (.-drawbacks task)))

(df task-spawn-subtask [(parent ts/TaskRecord) (subtask-id Str) (title Str) (owns (List Str)) (now-epoch I64)] -> ts/TaskRecord
  :d "Spawns a discovered child task linked to parent task in queued state."
  (ts/TaskRecord
    :id subtask-id :parent-id (.-id parent) :phase (.-phase parent) :title title :kind "task-spawn"
    :lane (.-lane parent) :state "queued" :priority (.-priority parent) :owner-role (.-owner-role parent)
    :created-at now-epoch :updated-at now-epoch :started-at 0 :completed-at 0 :owns owns
    :target-symbols (list) :related-symbols (list) :depends-on (list (.-id parent)) :gate (.-gate parent)
    :why (str "Discovered sub-task derived from " (.-id parent))
    :spec (str "Sub-task dynamically spawned during execution of " (.-id parent))
    :acceptance-criteria (list "exit-code 0") :step-index 0 :action-dag (list "Execute discovered sub-task")
    :handoff-context "" :receipts (list) :session-id "" :lease-expires-at 0
    :effort (.-effort parent) :risk (.-risk parent) :root-cause ""
    :consequences (list) :drawbacks (list)))

(df task-start [(task ts/TaskRecord) (now-epoch I64)] -> ts/TaskRecord
  :d "Transitions task to in-progress state, sets started-at, and updates timestamp."
  (ts/TaskRecord
    :id (.-id task) :parent-id (.-parent-id task) :phase (.-phase task) :title (.-title task)
    :kind (.-kind task) :lane (.-lane task) :state "in-progress" :priority (.-priority task)
    :owner-role (.-owner-role task) :created-at (.-created-at task) :updated-at now-epoch
    :started-at now-epoch :completed-at (.-completed-at task) :owns (.-owns task)
    :target-symbols (.-target-symbols task) :related-symbols (.-related-symbols task)
    :depends-on (.-depends-on task) :gate (.-gate task) :why (.-why task) :spec (.-spec task)
    :acceptance-criteria (.-acceptance-criteria task) :step-index (.-step-index task)
    :action-dag (.-action-dag task) :handoff-context (.-handoff-context task)
    :receipts (.-receipts task) :session-id (.-session-id task) :lease-expires-at (.-lease-expires-at task)
    :effort (.-effort task) :risk (.-risk task) :root-cause (.-root-cause task)
    :consequences (.-consequences task) :drawbacks (.-drawbacks task)))

(df task-advance-step [(task ts/TaskRecord) (now-epoch I64)] -> ts/TaskRecord
  :d "Advances monotonic step-index within action DAG and updates timestamp."
  (ts/TaskRecord
    :id (.-id task) :parent-id (.-parent-id task) :phase (.-phase task) :title (.-title task)
    :kind (.-kind task) :lane (.-lane task) :state (.-state task) :priority (.-priority task)
    :owner-role (.-owner-role task) :created-at (.-created-at task) :updated-at now-epoch
    :started-at (.-started-at task) :completed-at (.-completed-at task) :owns (.-owns task)
    :target-symbols (.-target-symbols task) :related-symbols (.-related-symbols task)
    :depends-on (.-depends-on task) :gate (.-gate task) :why (.-why task) :spec (.-spec task)
    :acceptance-criteria (.-acceptance-criteria task) :step-index (+ (.-step-index task) 1)
    :action-dag (.-action-dag task) :handoff-context (.-handoff-context task)
    :receipts (.-receipts task) :session-id (.-session-id task) :lease-expires-at (.-lease-expires-at task)
    :effort (.-effort task) :risk (.-risk task) :root-cause (.-root-cause task)
    :consequences (.-consequences task) :drawbacks (.-drawbacks task)))

(df task-capture-handoff [(task ts/TaskRecord) (context-snapshot Str) (now-epoch I64)] -> ts/TaskRecord
  :d "Captures working memory context snapshot for agent handoff."
  (ts/TaskRecord
    :id (.-id task) :parent-id (.-parent-id task) :phase (.-phase task) :title (.-title task)
    :kind (.-kind task) :lane (.-lane task) :state (.-state task) :priority (.-priority task)
    :owner-role (.-owner-role task) :created-at (.-created-at task) :updated-at now-epoch
    :started-at (.-started-at task) :completed-at (.-completed-at task) :owns (.-owns task)
    :target-symbols (.-target-symbols task) :related-symbols (.-related-symbols task)
    :depends-on (.-depends-on task) :gate (.-gate task) :why (.-why task) :spec (.-spec task)
    :acceptance-criteria (.-acceptance-criteria task) :step-index (.-step-index task)
    :action-dag (.-action-dag task) :handoff-context context-snapshot
    :receipts (.-receipts task) :session-id (.-session-id task) :lease-expires-at (.-lease-expires-at task)
    :effort (.-effort task) :risk (.-risk task) :root-cause (.-root-cause task)
    :consequences (.-consequences task) :drawbacks (.-drawbacks task)))

(df task-attach-receipt [(task ts/TaskRecord) (receipt ts/TaskReceipt) (now-epoch I64)] -> ts/TaskRecord
  :d "Attaches verified execution receipt and sets completion timestamp."
  (let [(r-asn (ts/format-receipt-asn receipt))
        (new-state (if (ts/receipt-is-verified? receipt) "completed" "failed"))
        (comp-ts (if (ts/receipt-is-verified? receipt) now-epoch 0))]
    (ts/TaskRecord
      :id (.-id task) :parent-id (.-parent-id task) :phase (.-phase task) :title (.-title task)
      :kind (.-kind task) :lane (.-lane task) :state new-state :priority (.-priority task)
      :owner-role (.-owner-role task) :created-at (.-created-at task) :updated-at now-epoch
      :started-at (.-started-at task) :completed-at comp-ts :owns (.-owns task)
      :target-symbols (.-target-symbols task) :related-symbols (.-related-symbols task)
      :depends-on (.-depends-on task) :gate (.-gate task) :why (.-why task) :spec (.-spec task)
      :acceptance-criteria (.-acceptance-criteria task) :step-index (.-step-index task)
      :action-dag (.-action-dag task) :handoff-context (.-handoff-context task)
      :receipts (list-append (.-receipts task) (list r-asn))
      :session-id (.-session-id task) :lease-expires-at (.-lease-expires-at task)
      :effort (.-effort task) :risk (.-risk task) :root-cause (.-root-cause task)
      :consequences (.-consequences task) :drawbacks (.-drawbacks task))))

(df task-complete [(task ts/TaskRecord) (receipt Str) (now-epoch I64)] -> ts/TaskRecord
  :d "Transitions task to completed state, attaches receipt, and updates timestamp."
  (ts/TaskRecord
    :id (.-id task) :parent-id (.-parent-id task) :phase (.-phase task) :title (.-title task)
    :kind (.-kind task) :lane (.-lane task) :state "completed" :priority (.-priority task)
    :owner-role (.-owner-role task) :created-at (.-created-at task) :updated-at now-epoch
    :started-at (.-started-at task) :completed-at now-epoch :owns (.-owns task)
    :target-symbols (.-target-symbols task) :related-symbols (.-related-symbols task)
    :depends-on (.-depends-on task) :gate (.-gate task) :why (.-why task) :spec (.-spec task)
    :acceptance-criteria (.-acceptance-criteria task) :step-index (.-step-index task)
    :action-dag (.-action-dag task) :handoff-context (.-handoff-context task)
    :receipts (list-append (.-receipts task) (list receipt))
    :session-id (.-session-id task) :lease-expires-at (.-lease-expires-at task)
    :effort (.-effort task) :risk (.-risk task) :root-cause (.-root-cause task)
    :consequences (.-consequences task) :drawbacks (.-drawbacks task)))

(df claim [(task ts/TaskRecord) (now-epoch I64)] -> ts/TaskRecord
  :d "1-to-2 token alias for task-claim."
  (task-claim task now-epoch))

(df settle [(task ts/TaskRecord) (receipt Str) (now-epoch I64)] -> ts/TaskRecord
  :d "1-to-2 token alias for task-complete."
  (task-complete task receipt now-epoch))
