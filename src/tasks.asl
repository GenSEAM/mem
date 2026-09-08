(module asl-mem/tasks
  :d "Canonical task management, dependency DAG resolution, ownership conflict detection, and parallel wave scheduling."
  :x [TaskRecord
      make-task
      task-owns-file?
      task-owns-intersect?
      task-overlap-files
      task-dependencies-satisfied?
      task-parallel-eligible?
      task-is-ready?
      task-find-ready
      task-claim
      task-start
      task-complete
      task-format-asn]
  :i [(vfs :a v)])

(dfs TaskRecord
  (:f id Str "Unique task identifier e.g. task-325-1")
  (:f phase Str "Canonical phase identifier e.g. phase-325-unified-llm-client")
  (:f title Str "Human-readable task summary")
  (:f lane Str "Target execution lane e.g. main or harness")
  (:f state Str "Lifecycle state: queued, ready, claimed, in-progress, completed, failed")
  (:f priority Str "Scheduling priority: low, normal, high, urgent")
  (:f created-at I64 "Epoch timestamp in milliseconds of task creation")
  (:f updated-at I64 "Epoch timestamp in milliseconds of latest state update")
  (:f owns (List Str) "Declared file ownership paths")
  (:f depends-on (List Str) "List of prerequisite task identifiers")
  (:f gate Str "Verification gate shell command")
  (:f why Str "Architectural rationale and invariant checked")
  (:f receipts (List Str) "Recorded physical execution receipts"))

(df make-task [(id Str) (phase Str) (title Str) (lane Str) (state Str) (priority Str) (created-at I64) (owns (List Str)) (depends-on (List Str)) (gate Str) (why Str)] -> TaskRecord
  :d "Constructs a new task record with updated-at initialized to created-at and empty receipts."
  (TaskRecord
    :id id
    :phase phase
    :title title
    :lane lane
    :state state
    :priority priority
    :created-at created-at
    :updated-at created-at
    :owns owns
    :depends-on depends-on
    :gate gate
    :why why
    :receipts (list)))

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

(df task-dependencies-satisfied? [(task TaskRecord) (completed-ids (List Str))] -> Bool
  :d "Checks if all prerequisite tasks of a task have been completed."
  (let [(missing (filter (fn [(dep Str)] -> Bool (not (list-contains? completed-ids dep))) (.-depends-on task)))]
    (list-empty? missing)))

(df task-parallel-eligible? [(t1 TaskRecord) (t2 TaskRecord)] -> Bool
  :d "Determines if two tasks can be safely executed in parallel without dependency or file ownership conflict."
  (and (!= (.-id t1) (.-id t2))
       (and (not (list-contains? (.-depends-on t1) (.-id t2)))
            (and (not (list-contains? (.-depends-on t2) (.-id t1)))
                 (not (task-owns-intersect? t1 t2))))))

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
    :phase (.-phase task)
    :title (.-title task)
    :lane (.-lane task)
    :state "claimed"
    :priority (.-priority task)
    :created-at (.-created-at task)
    :updated-at now-epoch
    :owns (.-owns task)
    :depends-on (.-depends-on task)
    :gate (.-gate task)
    :why (.-why task)
    :receipts (.-receipts task)))

(df task-start [(task TaskRecord) (now-epoch I64)] -> TaskRecord
  :d "Transitions task to in-progress state and updates timestamp."
  (TaskRecord
    :id (.-id task)
    :phase (.-phase task)
    :title (.-title task)
    :lane (.-lane task)
    :state "in-progress"
    :priority (.-priority task)
    :created-at (.-created-at task)
    :updated-at now-epoch
    :owns (.-owns task)
    :depends-on (.-depends-on task)
    :gate (.-gate task)
    :why (.-why task)
    :receipts (.-receipts task)))

(df task-complete [(task TaskRecord) (receipt Str) (now-epoch I64)] -> TaskRecord
  :d "Transitions task to completed state, attaches receipt, and updates timestamp."
  (TaskRecord
    :id (.-id task)
    :phase (.-phase task)
    :title (.-title task)
    :lane (.-lane task)
    :state "completed"
    :priority (.-priority task)
    :created-at (.-created-at task)
    :updated-at now-epoch
    :owns (.-owns task)
    :depends-on (.-depends-on task)
    :gate (.-gate task)
    :why (.-why task)
    :receipts (list-append (.-receipts task) (list receipt))))

(df task-format-asn [(task TaskRecord)] -> Str
  :d "Formats task into canonical ASN representation."
  (let [(owns-quoted (map (fn [(s Str)] -> Str (str "\"" s "\"")) (.-owns task)))
        (owns-str (str "[" (string-join owns-quoted " ") "]"))
        (deps-quoted (map (fn [(s Str)] -> Str (str "\"" s "\"")) (.-depends-on task)))
        (deps-str (str "[" (string-join deps-quoted " ") "]"))]
    (str "(:task\n"
         "  :id \"" (.-id task) "\"\n"
         "  :phase \"" (.-phase task) "\"\n"
         "  :title \"" (.-title task) "\"\n"
         "  :lane \"" (.-lane task) "\"\n"
         "  :state :" (.-state task) "\n"
         "  :priority :" (.-priority task) "\n"
         "  :created-at " (string-from-int64 (.-created-at task)) "\n"
         "  :updated-at " (string-from-int64 (.-updated-at task)) "\n"
         "  :owns " owns-str "\n"
         "  :depends-on " deps-str "\n"
         "  :gate \"" (.-gate task) "\"\n"
         "  :why \"" (.-why task) "\"\n"
         "  :receipts [])\n")))
