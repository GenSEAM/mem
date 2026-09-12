(module asl-mem/stm
  :d "Software Transactional Memory coordinator for multi-file atomic in-memory transactions"
  :x [
    STMFileSnapshot
    STMTransaction
    STMCommitResult
    stm-begin
    stm-read
    stm-write
    stm-rollback
    stm-validate-and-merge
    stm-get-snapshot
    stm-dirty-files
  ]
  :i [(ast_merge :a am) (vfs :a v)])

(dfs ASTCollisionRecord
  (:f symbol-id Str "Conflicted symbol identifier")
  (:f base-form Str "Original baseline form text")
  (:f branch-a Str "Branch A mutation text")
  (:f branch-b Str "Branch B mutation text")
  (:f collision-type Str "Type of collision"))

(dfs AstMergeResult
  (:f merged-content Str "Full merged file content")
  (:f conflicts (List Str) "List of conflicting symbol identifiers")
  (:f collisions (List ASTCollisionRecord) "List of structured collision records")
  (:f clean Bool "True if zero conflicts")
  (:f merged-forms-count I64 "Number of successfully resolved forms"))

(dfs STMFileSnapshot
  (:f path Str "Virtual file path")
  (:f base-content Str "Baseline content at transaction start")
  (:f staged-content Str "Locally modified content in transaction")
  (:f base-hash Str "Content hash at snapshot time")
  (:f dirty Bool "True if staged content diverged from base"))

(dfs STMTransaction
  (:f tx-id Str "Unique transaction identifier")
  (:f agent-id Str "Owner agent identifier")
  (:f epoch I64 "Logical epoch timestamp")
  (:f snapshots (List STMFileSnapshot) "Map of file snapshots in transaction")
  (:f status Str "active, committed, aborted, or conflict")
  (:f compensations (List Str) "List of rollback compensation actions"))

(dfs STMCommitResult
  (:f tx-id Str "Transaction identifier")
  (:f committed Bool "True if all files committed atomically")
  (:f touched-files (List Str) "List of modified file paths")
  (:f conflicted-files (List Str) "List of files with unresolvable collisions")
  (:f reconciled-files (List Str) "List of files auto-reconciled via AST merge")
  (:f merged-contents (List Str) "Final committed content for each touched file")
  (:f error-message (Option Str) "Failure reason if aborted"))

(df stm-begin [(tx-id Str) (agent-id Str) (epoch I64)] -> STMTransaction
  :d "Initializes an isolated in-memory software transactional memory session."
  (STMTransaction
    :tx-id tx-id
    :agent-id agent-id
    :epoch epoch
    :snapshots (list)
    :status "active"
    :compensations (list)))

(df stm-get-snapshot [(tx STMTransaction) (target-path Str)] -> (Option STMFileSnapshot)
  :d "Retrieves existing snapshot for path in transaction if present."
  (fold (fn [(acc (Option STMFileSnapshot)) (s STMFileSnapshot)] -> (Option STMFileSnapshot)
          (mt acc
            ((some found) (some found))
            ((none) (if (= (.-path s) target-path) (some s) (none)))))
        (none)
        (.-snapshots tx)))

(df stm-read [(tx STMTransaction) (path Str) (current-trunk Str)] -> STMTransaction
  :d "Records file read in transaction read-set if not already loaded."
  (let [(existing (stm-get-snapshot tx path))]
    (mt existing
      ((some _) tx)
      ((none)
       (let [(snap (STMFileSnapshot
                     :path path
                     :base-content current-trunk
                     :staged-content current-trunk
                     :base-hash (v/vfs-cas-hash current-trunk)
                     :dirty false))
             (next-snaps (list-append (.-snapshots tx) (list snap)))]
         (STMTransaction
           :tx-id (.-tx-id tx)
           :agent-id (.-agent-id tx)
           :epoch (.-epoch tx)
           :snapshots next-snaps
           :status (.-status tx)
           :compensations (.-compensations tx)))))))

(df stm-write [(tx STMTransaction) (path Str) (new-content Str)] -> STMTransaction
  :d "Stages modification in transaction write-set, marking snapshot dirty."
  (let [(snaps (.-snapshots tx))
        (existing (stm-get-snapshot tx path))]
    (mt existing
      ((some curr)
       (let [(updated (STMFileSnapshot
                        :path path
                        :base-content (.-base-content curr)
                        :staged-content new-content
                        :base-hash (.-base-hash curr)
                        :dirty true))
             (next-snaps (fold (fn [(acc (List STMFileSnapshot)) (s STMFileSnapshot)] -> (List STMFileSnapshot)
                                 (if (= (.-path s) path)
                                   (list-append acc (list updated))
                                   (list-append acc (list s))))
                               (list)
                               snaps))]
         (STMTransaction
           :tx-id (.-tx-id tx)
           :agent-id (.-agent-id tx)
           :epoch (.-epoch tx)
           :snapshots next-snaps
           :status (.-status tx)
           :compensations (.-compensations tx))))
      ((none)
       (let [(snap (STMFileSnapshot
                     :path path
                     :base-content ""
                     :staged-content new-content
                     :base-hash "0"
                     :dirty true))
             (next-snaps (list-append snaps (list snap)))]
         (STMTransaction
           :tx-id (.-tx-id tx)
           :agent-id (.-agent-id tx)
           :epoch (.-epoch tx)
           :snapshots next-snaps
           :status (.-status tx)
           :compensations (.-compensations tx)))))))

(df stm-rollback [(tx STMTransaction) (reason Str)] -> STMTransaction
  :d "Aborts transaction and marks status as aborted."
  (STMTransaction
    :tx-id (.-tx-id tx)
    :agent-id (.-agent-id tx)
    :epoch (.-epoch tx)
    :snapshots (.-snapshots tx)
    :status "aborted"
    :compensations (list-append (.-compensations tx) (list reason))))

(df stm-dirty-files [(tx STMTransaction)] -> (List Str)
  :d "Returns paths of all files modified in transaction."
  (fold (fn [(acc (List Str)) (s STMFileSnapshot)] -> (List Str)
          (if (.-dirty s)
            (list-append acc (list (.-path s)))
            acc))
        (list)
        (.-snapshots tx)))

(df find-trunk-content [(paths (List Str)) (contents (List Str)) (target Str)] -> (Option Str)
  :d "Helper finding current trunk content for target path."
  (let [(idx-opt (fold (fn [(acc (Option I64)) (p Str)] -> (Option I64)
                         (mt acc
                           ((some found) (some found))
                           ((none) (if (= p target) (some 0) (none)))))
                       (none)
                       paths))]
    (mt idx-opt
      ((none) (none))
      ((some _)
       (let [(res (fold (fn [(acc (Option Str)) (pair Str)] -> (Option Str)
                          (mt acc
                            ((some found) (some found))
                            ((none)
                             (if (string-starts-with? pair (str target ":"))
                               (some (let [(pref (str target ":")) (plen (string-length pref))] (option-or (string-slice pair plen (string-length pair)) "")))
                               (none)))))
                        (none)
                        contents))]
         res)))))

(dfs OCCValidationAccumulator
  (:f touched (List Str) "Touched file paths")
  (:f conflicts (List Str) "Conflicted file paths")
  (:f reconciled (List Str) "Reconciled file paths")
  (:f merged-texts (List Str) "Final merged file contents")
  (:f ok Bool "True if all files validated cleanly"))

(df stm-validate-and-merge [(tx STMTransaction)
                            (trunk-paths (List Str))
                            (trunk-contents (List Str))] -> STMCommitResult
  :d "Validates OCC invariants and merges dirty snapshots atomically across all files."
  (let [(snaps (.-snapshots tx))
        (init (OCCValidationAccumulator
                :touched (list)
                :conflicts (list)
                :reconciled (list)
                :merged-texts (list)
                :ok true))
        (res (fold (fn [(acc OCCValidationAccumulator) (s STMFileSnapshot)] -> OCCValidationAccumulator
                     (if (not (.-dirty s))
                       acc
                       (let [(p (.-path s))
                             (base-txt (.-base-content s))
                             (staged-txt (.-staged-content s))
                             (trunk-opt (find-trunk-content trunk-paths trunk-contents p))
                             (cur-trunk (option-or trunk-opt base-txt))
                             (touched (list-append (.-touched acc) (list p)))]
                         (if (= cur-trunk base-txt)
                           (OCCValidationAccumulator
                             :touched touched
                             :conflicts (.-conflicts acc)
                             :reconciled (.-reconciled acc)
                             :merged-texts (list-append (.-merged-texts acc) (list staged-txt))
                             :ok (.-ok acc))
                           (let [(merge-res (am/universal-ast-3way-merge base-txt staged-txt cur-trunk))]
                             (if (.-clean merge-res)
                               (OCCValidationAccumulator
                                 :touched touched
                                 :conflicts (.-conflicts acc)
                                 :reconciled (list-append (.-reconciled acc) (list p))
                                 :merged-texts (list-append (.-merged-texts acc) (list (.-merged-content merge-res)))
                                 :ok (.-ok acc))
                               (OCCValidationAccumulator
                                 :touched touched
                                 :conflicts (list-append (.-conflicts acc) (list p))
                                 :reconciled (.-reconciled acc)
                                 :merged-texts (.-merged-texts acc)
                                 :ok false)))))))
                   init
                   snaps))]
    (if (.-ok res)
      (STMCommitResult
        :tx-id (.-tx-id tx)
        :committed true
        :touched-files (.-touched res)
        :conflicted-files (list)
        :reconciled-files (.-reconciled res)
        :merged-contents (.-merged-texts res)
        :error-message (none))
      (STMCommitResult
        :tx-id (.-tx-id tx)
        :committed false
        :touched-files (.-touched res)
        :conflicted-files (.-conflicts res)
        :reconciled-files (.-reconciled res)
        :merged-contents (list)
        :error-message (some "OCC transaction aborted: AST collision on concurrent trunk write")))))
