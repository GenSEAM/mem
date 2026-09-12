(module asl-mem/task-recovery
  :d "Checkpointed action DAG step recovery protocol for seamless agent crash and dropout recovery per d45."
  :x [RecoverySnapshot
      recover-task-session
      resume-action-dag
      extract-handoff-state
      verify-checkpoint-integrity
      format-recovery-asn
      snapshot
      resume
      extract
      verify
      format-recovery]
  :i [(tasks_store :a t)
      (asl-text/escape :a esc)])

(dfs RecoverySnapshot
  (:f task-id Str "Unique task identifier e.g. task-397-02")
  (:f previous-session-id Str "Session ID of dropped or crashed agent")
  (:f recovered-session-id Str "Session ID of replacement agent")
  (:f step-index I64 "Active step index resumed without repeated work")
  (:f total-steps I64 "Total number of sub-steps in action DAG")
  (:f remaining-steps (List Str) "List of pending sub-steps remaining to execute")
  (:f handoff-state Str "Restored serialized working memory context")
  (:f is-valid Bool "Integrity status of restored checkpoint")
  (:f recovered-at I64 "Epoch ms timestamp of recovery"))

(df verify-checkpoint-integrity [(task t/TaskRecord)] -> Bool
  :d "Validates that checkpoint state is structurally intact, step index is within DAG bounds, and handoff string is uncorrupted."
  (let [(dag-len (list-length (.-action-dag task)))
        (idx (.-step-index task))
        (handoff (.-handoff-context task))]
    (and (> (string-length (.-id task)) 0)
         (and (>= idx 0)
              (and (<= idx dag-len)
                   (not (string-contains? handoff "CORRUPTED")))))))

(df extract-handoff-state [(task t/TaskRecord)] -> Str
  :d "Extracts working memory context snapshot from checkpointed task record."
  (if (verify-checkpoint-integrity task)
    (.-handoff-context task)
    ""))

(df resume-action-dag [(action-dag (List Str)) (from-step-index I64)] -> (List Str)
  :d "Extracts remaining sub-steps in action DAG starting from active step index."
  (let [(total (list-length action-dag))]
    (if (<= from-step-index 0)
      action-dag
      (if (>= from-step-index total)
        (list)
        (option-or (list-slice action-dag from-step-index total) (list))))))

(df recover-task-session [(task t/TaskRecord) (new-session-id Str) (now-epoch I64)] -> RecoverySnapshot
  :d "Constructs a recovery snapshot for a replacement agent session from checkpointed task state."
  (let [(valid? (verify-checkpoint-integrity task))
        (handoff (extract-handoff-state task))
        (remaining (resume-action-dag (.-action-dag task) (.-step-index task)))]
    (RecoverySnapshot
      :task-id (.-id task)
      :previous-session-id (.-session-id task)
      :recovered-session-id new-session-id
      :step-index (.-step-index task)
      :total-steps (list-length (.-action-dag task))
      :remaining-steps remaining
      :handoff-state handoff
      :is-valid valid?
      :recovered-at now-epoch)))

(df format-recovery-asn [(snap RecoverySnapshot)] -> Str
  :d "Serializes recovery snapshot into canonical machine ASN notation."
  (let [(steps-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-remaining-steps snap)))
        (steps-str (str "[" (string-join steps-quoted " ") "]"))]
    (str "(:recovery-snapshot\n"
         "  :task-id \"" (esc/escape-asn-str (.-task-id snap)) "\"\n"
         "  :previous-session-id \"" (esc/escape-asn-str (.-previous-session-id snap)) "\"\n"
         "  :recovered-session-id \"" (esc/escape-asn-str (.-recovered-session-id snap)) "\"\n"
         "  :step-index " (string-from-int64 (.-step-index snap)) "\n"
         "  :total-steps " (string-from-int64 (.-total-steps snap)) "\n"
         "  :remaining-steps " steps-str "\n"
         "  :handoff-state \"" (esc/escape-asn-str (.-handoff-state snap)) "\"\n"
         "  :is-valid " (if (.-is-valid snap) "true" "false") "\n"
         "  :recovered-at " (string-from-int64 (.-recovered-at snap)) ")\n")))

(df snapshot [(task t/TaskRecord) (new-session-id Str) (now-epoch I64)] -> RecoverySnapshot
  :d "1-to-2 token alias for recover-task-session."
  (recover-task-session task new-session-id now-epoch))

(df resume [(action-dag (List Str)) (from-step-index I64)] -> (List Str)
  :d "1-to-2 token alias for resume-action-dag."
  (resume-action-dag action-dag from-step-index))

(df extract [(task t/TaskRecord)] -> Str
  :d "1-to-2 token alias for extract-handoff-state."
  (extract-handoff-state task))

(df verify [(task t/TaskRecord)] -> Bool
  :d "1-to-2 token alias for verify-checkpoint-integrity."
  (verify-checkpoint-integrity task))

(df format-recovery [(snap RecoverySnapshot)] -> Str
  :d "1-to-2 token alias for format-recovery-asn."
  (format-recovery-asn snap))
