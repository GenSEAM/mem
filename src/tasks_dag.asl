(module asl-mem/tasks-dag
  :d "Task dependency DAG resolution, resource ownership conflict detection, and parallel execution eligibility."
  :x [task-owns-file?
      task-owns-intersect?
      task-overlap-files
      task-overlap-blast-radius?
      task-dependencies-satisfied?
      task-parallel-eligible?
      task-is-ready?
      task-find-ready]
  :i [(tasks_store :a ts)
      (vfs :a v)])

(df task-owns-file? [(task ts/TaskRecord) (file Str)] -> Bool
  :d "Checks if a task declared ownership over a specific file path using normalized path comparison."
  (let [(norm (v/normalize-path file))
        (owns-norm (map (fn [(f Str)] -> Str (v/normalize-path f)) (.-owns task)))]
    (list-contains? owns-norm norm)))

(df task-owns-intersect? [(t1 ts/TaskRecord) (t2 ts/TaskRecord)] -> Bool
  :d "Checks if two tasks declare overlapping file ownership, ignoring dash placeholder."
  (let [(files1 (filter (fn [(f Str)] -> Bool (!= f "-")) (map (fn [(f Str)] -> Str (v/normalize-path f)) (.-owns t1))))
        (files2 (filter (fn [(f Str)] -> Bool (!= f "-")) (map (fn [(f Str)] -> Str (v/normalize-path f)) (.-owns t2))))]
    (let [(common (filter (fn [(f Str)] -> Bool (list-contains? files2 f)) files1))]
      (> (list-length common) 0))))

(df task-overlap-files [(t1 ts/TaskRecord) (t2 ts/TaskRecord)] -> (List Str)
  :d "Returns the list of overlapping files between two tasks, ignoring dash placeholder."
  (let [(files1 (filter (fn [(f Str)] -> Bool (!= f "-")) (map (fn [(f Str)] -> Str (v/normalize-path f)) (.-owns t1))))
        (files2 (filter (fn [(f Str)] -> Bool (!= f "-")) (map (fn [(f Str)] -> Str (v/normalize-path f)) (.-owns t2))))]
    (filter (fn [(f Str)] -> Bool (list-contains? files2 f)) files1)))

(df task-overlap-blast-radius? [(t1 ts/TaskRecord) (t2 ts/TaskRecord)] -> Bool
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

(df task-dependencies-satisfied? [(task ts/TaskRecord) (completed-ids (List Str))] -> Bool
  :d "Checks if all prerequisite tasks of a task have been completed."
  (let [(missing (filter (fn [(dep Str)] -> Bool (not (list-contains? completed-ids dep))) (.-depends-on task)))]
    (list-empty? missing)))

(df task-parallel-eligible? [(t1 ts/TaskRecord) (t2 ts/TaskRecord)] -> Bool
  :d "Determines if two tasks can be safely executed in parallel without dependency, file ownership, or blast-radius symbol conflict."
  (and (!= (.-id t1) (.-id t2))
       (and (not (list-contains? (.-depends-on t1) (.-id t2)))
            (and (not (list-contains? (.-depends-on t2) (.-id t1)))
                 (and (not (task-owns-intersect? t1 t2))
                      (not (task-overlap-blast-radius? t1 t2)))))))

(df task-is-ready? [(task ts/TaskRecord) (completed-ids (List Str))] -> Bool
  :d "Checks if a task is queued or ready and has all its prerequisites satisfied."
  (and (or (= (.-state task) "queued")
           (= (.-state task) "ready"))
       (task-dependencies-satisfied? task completed-ids)))

(df task-find-ready [(tasks (List ts/TaskRecord)) (completed-ids (List Str))] -> (List ts/TaskRecord)
  :d "Filters tasks that are eligible for immediate execution."
  (filter (fn [(t ts/TaskRecord)] -> Bool (task-is-ready? t completed-ids)) tasks))
