(module asl-mem/tasks
  :d "Unified task management facade composing store, dependency DAG, and session lease sub-modules."
  :x [make-task-receipt
      receipt-is-verified?
      format-receipt-asn
      make-holistic-task
      make-task
      make-gap-task
      make-cybernetic-task
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
      task/settle
      make-post-action
      make-post-action-queue
      post-action-enqueue
      post-action-drain
      format-post-action-asn]
  :i [(tasks_store :a ts)
      (tasks_dag :a td)
      (tasks_lease :a tl)])

(df make-task-receipt [(exit-code I64) (duration-ms I64) (rss-mb I64) (asserts-evaluated I64) (tests-passed I64) (tests-failed I64) (mutated-files (List Str)) (diff-hash Str) (checked-invariants (List Str)) (raw-evidence Str)] -> ts/TaskReceipt
  :d "Forwarding constructor for TaskReceipt."
  (ts/make-task-receipt exit-code duration-ms rss-mb asserts-evaluated tests-passed tests-failed mutated-files diff-hash checked-invariants raw-evidence))

(df receipt-is-verified? [(receipt ts/TaskReceipt)] -> Bool
  :d "Delegates receipt verification to tasks_store."
  (ts/receipt-is-verified? receipt))

(df format-receipt-asn [(receipt ts/TaskReceipt)] -> Str
  :d "Delegates receipt ASN formatting to tasks_store."
  (ts/format-receipt-asn receipt))

(df make-anticipated-risk [(condition Str) (impact Str) (mitigation Str) (gate Str)] -> ts/AnticipatedRisk
  :d "Delegates risk construction to tasks_store."
  (ts/make-anticipated-risk condition impact mitigation gate))

(df make-task-tolerance [(invariants (List Str)) (leeway (List Str)) (risks (List ts/AnticipatedRisk)) (triggers (List Str))] -> ts/TaskTolerance
  :d "Delegates tolerance construction to tasks_store."
  (ts/make-task-tolerance invariants leeway risks triggers))

(df task-is-within-leeway? [(tolerance ts/TaskTolerance) (proposed-action Str)] -> Bool
  :d "Delegates leeway check to tasks_store."
  (ts/task-is-within-leeway? tolerance proposed-action))

(df task-has-escalation-trigger? [(tolerance ts/TaskTolerance) (event-desc Str)] -> Bool
  :d "Delegates escalation trigger check to tasks_store."
  (ts/task-has-escalation-trigger? tolerance event-desc))

(df format-anticipated-risk-asn [(risk ts/AnticipatedRisk)] -> Str
  :d "Delegates risk serialization to tasks_store."
  (ts/format-anticipated-risk-asn risk))

(df format-task-tolerance-asn [(tolerance ts/TaskTolerance)] -> Str
  :d "Delegates tolerance serialization to tasks_store."
  (ts/format-task-tolerance-asn tolerance))

(df make-holistic-task [(id Str) (parent-id Str) (phase Str) (title Str) (kind Str) (lane Str) (state Str) (priority Str) (owner-role Str) (created-at I64) (owns (List Str)) (target-symbols (List Str)) (related-symbols (List Str)) (depends-on (List Str)) (gate Str) (why Str) (spec Str) (acceptance-criteria (List Str)) (action-dag (List Str))] -> ts/TaskRecord
  :d "Forwarding constructor for holistic TaskRecord."
  (ts/make-holistic-task id parent-id phase title kind lane state priority owner-role created-at owns target-symbols related-symbols depends-on gate why spec acceptance-criteria action-dag))

(df make-task [(id Str) (phase Str) (title Str) (lane Str) (state Str) (priority Str) (created-at I64) (owns (List Str)) (depends-on (List Str)) (gate Str) (why Str)] -> ts/TaskRecord
  :d "Forwarding constructor for standard TaskRecord."
  (ts/make-task id phase title lane state priority created-at owns depends-on gate why))

(df make-gap-task [(id Str) (phase Str) (title Str) (lane Str) (priority Str) (created-at I64) (owns (List Str)) (gate Str) (why Str) (effort Str) (risk Str) (root-cause Str) (consequences (List Str)) (drawbacks (List Str))] -> ts/TaskRecord
  :d "Forwarding constructor for first-class GAP TaskRecord."
  (ts/make-gap-task id phase title lane priority created-at owns gate why effort risk root-cause consequences drawbacks))

(df make-cybernetic-task [(id Str) (phase Str) (title Str) (kind Str) (lane Str) (state Str) (priority Str) (created-at I64) (owns (List Str)) (depends-on (List Str)) (gate Str) (why Str) (effort Str) (risk Str) (root-cause Str) (consequences (List Str)) (drawbacks (List Str))] -> ts/TaskRecord
  :d "Forwarding constructor for cybernetic TaskRecord."
  (ts/make-cybernetic-task id phase title kind lane state priority created-at owns depends-on gate why effort risk root-cause consequences drawbacks))

(df task-owns-file? [(task ts/TaskRecord) (file Str)] -> Bool
  :d "Delegates ownership check to tasks_dag."
  (td/task-owns-file? task file))

(df task-owns-intersect? [(t1 ts/TaskRecord) (t2 ts/TaskRecord)] -> Bool
  :d "Delegates intersection check to tasks_dag."
  (td/task-owns-intersect? t1 t2))

(df task-overlap-files [(t1 ts/TaskRecord) (t2 ts/TaskRecord)] -> (List Str)
  :d "Delegates overlap extraction to tasks_dag."
  (td/task-overlap-files t1 t2))

(df task-overlap-blast-radius? [(t1 ts/TaskRecord) (t2 ts/TaskRecord)] -> Bool
  :d "Delegates blast-radius collision detection to tasks_dag."
  (td/task-overlap-blast-radius? t1 t2))

(df task-dependencies-satisfied? [(task ts/TaskRecord) (completed-ids (List Str))] -> Bool
  :d "Delegates dependency resolution to tasks_dag."
  (td/task-dependencies-satisfied? task completed-ids))

(df task-parallel-eligible? [(t1 ts/TaskRecord) (t2 ts/TaskRecord)] -> Bool
  :d "Delegates parallel eligibility check to tasks_dag."
  (td/task-parallel-eligible? t1 t2))

(df task-is-ready? [(task ts/TaskRecord) (completed-ids (List Str))] -> Bool
  :d "Delegates readiness check to tasks_dag."
  (td/task-is-ready? task completed-ids))

(df task-find-ready [(tasks (List ts/TaskRecord)) (completed-ids (List Str))] -> (List ts/TaskRecord)
  :d "Delegates ready task filtering to tasks_dag."
  (td/task-find-ready tasks completed-ids))

(df task-claim [(task ts/TaskRecord) (now-epoch I64)] -> ts/TaskRecord
  :d "Delegates claim transition to tasks_lease."
  (tl/task-claim task now-epoch))

(df task-claim-session [(task ts/TaskRecord) (session-id Str) (lease-ms I64) (now-epoch I64)] -> ts/TaskRecord
  :d "Delegates session claim to tasks_lease."
  (tl/task-claim-session task session-id lease-ms now-epoch))

(df task-is-claimed-by? [(task ts/TaskRecord) (session-id Str)] -> Bool
  :d "Delegates claimed session check to tasks_lease."
  (tl/task-is-claimed-by? task session-id))

(df task-is-lease-expired? [(task ts/TaskRecord) (now-epoch I64)] -> Bool
  :d "Delegates lease expiration check to tasks_lease."
  (tl/task-is-lease-expired? task now-epoch))

(df task-recover-session [(task ts/TaskRecord) (new-session-id Str) (lease-ms I64) (now-epoch I64)] -> ts/TaskRecord
  :d "Delegates task recovery to tasks_lease."
  (tl/task-recover-session task new-session-id lease-ms now-epoch))

(df task-spawn-subtask [(parent ts/TaskRecord) (subtask-id Str) (title Str) (owns (List Str)) (now-epoch I64)] -> ts/TaskRecord
  :d "Delegates subtask spawn to tasks_lease."
  (tl/task-spawn-subtask parent subtask-id title owns now-epoch))

(df task-start [(task ts/TaskRecord) (now-epoch I64)] -> ts/TaskRecord
  :d "Delegates start transition to tasks_lease."
  (tl/task-start task now-epoch))

(df task-advance-step [(task ts/TaskRecord) (now-epoch I64)] -> ts/TaskRecord
  :d "Delegates step advancement to tasks_lease."
  (tl/task-advance-step task now-epoch))

(df task-capture-handoff [(task ts/TaskRecord) (context-snapshot Str) (now-epoch I64)] -> ts/TaskRecord
  :d "Delegates handoff capture to tasks_lease."
  (tl/task-capture-handoff task context-snapshot now-epoch))

(df task-attach-receipt [(task ts/TaskRecord) (receipt ts/TaskReceipt) (now-epoch I64)] -> ts/TaskRecord
  :d "Delegates receipt attachment to tasks_lease."
  (tl/task-attach-receipt task receipt now-epoch))

(df task-complete [(task ts/TaskRecord) (receipt Str) (now-epoch I64)] -> ts/TaskRecord
  :d "Delegates completion transition to tasks_lease."
  (tl/task-complete task receipt now-epoch))

(df task-format-asn [(task ts/TaskRecord)] -> Str
  :d "Delegates ASN formatting to tasks_store."
  (ts/task-format-asn task))

(df task-format-holistic-asn [(task ts/TaskRecord)] -> Str
  :d "Delegates holistic ASN formatting to tasks_store."
  (ts/task-format-holistic-asn task))

(df claim [(task ts/TaskRecord) (now-epoch I64)] -> ts/TaskRecord
  :d "1-to-2 token alias for task-claim."
  (tl/claim task now-epoch))

(df state [(task ts/TaskRecord)] -> Str
  :d "1-to-2 token accessor for task state."
  (ts/state task))

(df settle [(task ts/TaskRecord) (receipt Str) (now-epoch I64)] -> ts/TaskRecord
  :d "1-to-2 token alias for task-complete."
  (tl/settle task receipt now-epoch))

(df task/claim [(task ts/TaskRecord) (now-epoch I64)] -> ts/TaskRecord
  :d "Modular alias for task-claim."
  (tl/task/claim task now-epoch))

(df task/state [(task ts/TaskRecord)] -> Str
  :d "Modular accessor for task state."
  (ts/state task))

(df task/settle [(task ts/TaskRecord) (receipt Str) (now-epoch I64)] -> ts/TaskRecord
  :d "Modular alias for task-complete."
  (tl/task/settle task receipt now-epoch))

(df make-post-action [(action-id Str) (trigger-state Str) (kind Str) (target-phase Str) (description Str) (payload Str)] -> ts/PostAction
  :d "Forwarding constructor for PostAction."
  (ts/make-post-action action-id trigger-state kind target-phase description payload))

(df make-post-action-queue [(queue-id Str)] -> ts/PostActionQueue
  :d "Forwarding constructor for PostActionQueue."
  (ts/make-post-action-queue queue-id))

(df post-action-enqueue [(q ts/PostActionQueue) (action ts/PostAction)] -> ts/PostActionQueue
  :d "Forwarding operation for post-action-enqueue."
  (ts/post-action-enqueue q action))

(df post-action-drain [(q ts/PostActionQueue)] -> ts/PostActionQueue
  :d "Forwarding operation for post-action-drain."
  (ts/post-action-drain q))

(df format-post-action-asn [(action ts/PostAction)] -> Str
  :d "Forwarding operation for format-post-action-asn."
  (ts/format-post-action-asn action))
