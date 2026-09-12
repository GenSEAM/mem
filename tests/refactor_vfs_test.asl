(module asl-mem/refactor-vfs-test
  :d "Unit tests for VFS in-memory staged refactoring engine with CAS diff validation"
  :x [test-stage-refactor-diff
      test-commit-staged-refactor-success
      test-commit-staged-refactor-conflict
      test-rollback-staged-refactor
      test-preview-token-savings
      run-tests]
  :i [(vfs :a v)
      (refactor_vfs :a rv)
      (asl-compiler/compact-rewrite :a cr)])

(df test-stage-refactor-diff [] -> Bool
  :d "Verifies in-memory refactor staging with diff and token savings telemetry."
  (let [(reg0 (v/vfs-init))
        (src "(let [(norm (normalize-path p))] (string-starts-with? norm \"/root\"))")
        (reg1 (v/vfs-write reg0 "src/main.asl" src))
        (rules (cr/default-compaction-rules))
        (plan-opt (rv/stage-refactor-diff reg1 "src/main.asl" rules))]
    (assert (is-some? plan-opt) "Staging refactor diff on existing buffer must succeed")
    (mt plan-opt
      ((none) false)
      ((some plan)
       (do
         (assert (> (string-length (.-diff plan)) 0) "Diff must not be empty for modified buffer")
         (assert (> (.-tokens-saved plan) 0) "Estimated tokens saved must be greater than zero")
         (assert (= (.-staged-content plan) "(let [(norm (v/norm p))] (txt/starts? norm \"/root\"))") "Staged content must replace verbose tokens with modular aliases")
         (assert (= (.-status plan) "staged") "Plan status must be staged")
         true)))))

(df test-commit-staged-refactor-success [] -> Bool
  :d "Verifies successful commit when in-memory CAS hash matches base hash."
  (let [(reg0 (v/vfs-init))
        (src "(task-claim task-1 session-a)")
        (reg1 (v/vfs-write reg0 "tasks/run.asl" src))
        (rules (cr/default-compaction-rules))
        (plan-opt (rv/stage-refactor-diff reg1 "tasks/run.asl" rules))]
    (assert (is-some? plan-opt) "Plan must be created cleanly")
    (mt plan-opt
      ((none) false)
      ((some plan)
       (let [(commit-opt (rv/commit-staged-refactor reg1 plan))]
         (assert (is-some? commit-opt) "Commit must succeed when buffer CAS hash matches base hash")
         (mt commit-opt
           ((none) false)
           ((some reg2)
            (let [(buf-opt (v/vfs-read reg2 "tasks/run.asl"))]
              (assert (is-some? buf-opt) "Committed buffer must be readable from updated registry")
              (mt buf-opt
                ((none) false)
                ((some b)
                 (do
                   (assert (= (.-content b) "(task/claim task-1 session-a)") "Committed buffer must contain staged content")
                   (assert (= (.-cas-hash b) (.-staged-hash plan)) "Committed buffer CAS hash must equal staged hash")
                   true)))))))))))

(df test-commit-staged-refactor-conflict [] -> Bool
  :d "Verifies rejection of commit when in-memory buffer drifts creating CAS conflict."
  (let [(reg0 (v/vfs-init))
        (src "(supervise-step proc-1 0)")
        (reg1 (v/vfs-write reg0 "proc/runner.asl" src))
        (rules (cr/default-compaction-rules))
        (plan-opt (rv/stage-refactor-diff reg1 "proc/runner.asl" rules))]
    (assert (is-some? plan-opt) "Plan must be created cleanly")
    (mt plan-opt
      ((none) false)
      ((some plan)
       (let [(reg-drifted (v/vfs-write reg1 "proc/runner.asl" "(supervise-step proc-1 999)"))
             (commit-opt (rv/commit-staged-refactor reg-drifted plan))]
         (assert (is-none? commit-opt) "Commit must fail with none on CAS hash conflict")
         (let [(cur-buf (option-or (v/vfs-read reg-drifted "proc/runner.asl") (v/VFSBuffer :path "" :content "" :base-content "" :cas-hash "" :base-hash "" :revision 0 :dirty false :loaded-at 0)))]
           (assert (= (.-content cur-buf) "(supervise-step proc-1 999)") "Drifted content must remain untouched on rejected commit")
           true))))))

(df test-rollback-staged-refactor [] -> Bool
  :d "Verifies rollback restores buffer to pristine base content and hash."
  (let [(reg0 (v/vfs-init))
        (src "(string-contains? text \"needle\")")
        (reg1 (v/vfs-write reg0 "search/query.asl" src))
        (rules (cr/default-compaction-rules))
        (plan-opt (rv/stage-refactor-diff reg1 "search/query.asl" rules))]
    (assert (is-some? plan-opt) "Plan must be created cleanly")
    (mt plan-opt
      ((none) false)
      ((some plan)
       (let [(commit-opt (rv/commit-staged-refactor reg1 plan))]
         (assert (is-some? commit-opt) "Initial commit must succeed")
         (mt commit-opt
           ((none) false)
           ((some reg2)
            (let [(reg-rolled (rv/rollback-staged-refactor reg2 plan))
                  (buf-rolled (option-or (v/vfs-read reg-rolled "search/query.asl") (v/VFSBuffer :path "" :content "" :base-content "" :cas-hash "" :base-hash "" :revision 0 :dirty false :loaded-at 0)))]
              (assert (= (.-content buf-rolled) src) "Rollback must restore pristine original content")
              (assert (= (.-cas-hash buf-rolled) (.-base-hash plan)) "Rollback must restore original CAS base hash")
              true))))))))

(df test-preview-token-savings [] -> Bool
  :d "Verifies preview token savings calculation across single and multiple rules."
  (let [(rules (cr/default-compaction-rules))
        (src1 "(string-starts-with? a b)")
        (s1 (rv/preview-token-savings src1 rules))
        (src2 "(string-starts-with? (vfs-normalize-path file-path) \"/root/dir\")")
        (s2 (rv/preview-token-savings src2 rules))
        (src3 "(+ a b)")
        (s3 (rv/preview-token-savings src3 rules))]
    (assert (= s1 2) "Single rule token savings must match rule specification")
    (assert (= s2 5) "Composite multi-rule token savings must equal sum of rule savings")
    (assert (= s3 0) "Unrelated expressions without verbose tokens must have 0 savings")
    true))

(df run-tests [] -> Bool
  :d "Runs all refactor VFS staging and CAS diff engine unit tests."
  (do
    (assert (test-stage-refactor-diff) "test-stage-refactor-diff must pass")
    (assert (test-commit-staged-refactor-success) "test-commit-staged-refactor-success must pass")
    (assert (test-commit-staged-refactor-conflict) "test-commit-staged-refactor-conflict must pass")
    (assert (test-rollback-staged-refactor) "test-rollback-staged-refactor must pass")
    (assert (test-preview-token-savings) "test-preview-token-savings must pass")
    true))
