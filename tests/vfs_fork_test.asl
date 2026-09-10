(module asl-mem/tests/vfs-fork-test
  :d "Unit verification suite for speculative VFS branching, snapshot forking, commit, and abort mechanics."
  :x [run-tests
      test-fork-vfs-branch
      test-speculative-write-isolation
      test-commit-branch
      test-abort-branch
      test-compensating-sagas
      test-branch-diff-and-multi-fork]
  :i [(vfs :a v)
      (vfs_fork :a vf)])

(df get-branch-buffer [(branch vf/VFSBranch) (target-path Str)] -> (Option v/VFSBuffer)
  :d "Helper retrieving buffer from branch buffer list matching path."
  (fold (fn [(acc (Option v/VFSBuffer)) (b v/VFSBuffer)] -> (Option v/VFSBuffer)
          (mt acc
            ((some found) (some found))
            ((none)
             (if (= (.-path b) target-path)
               (some b)
               (none)))))
        (none)
        (.-buffers branch)))

(df test-fork-vfs-branch [] -> Bool
  :d "Verifies branch creation, branch identifier, root parent id, active status, buffer inheritance, and buffer count."
  (let [(reg0 (v/vfs-init))
        (reg1 (v/vfs-write reg0 "src/main.asl" "(module main :x [])"))
        (reg2 (v/vfs-write reg1 "docs/readme.md" "# Readme"))
        (branch (vf/fork-vfs-branch reg2 "branch-alpha"))]
    (assert (= (.-branch-id branch) "branch-alpha") "Branch id must match fork branch-id parameter")
    (assert (= (.-parent-id branch) "root") "Parent id must be root")
    (assert (= (.-status branch) "active") "Initial branch status must be active")
    (assert (= (list-length (.-buffers branch)) 2) "Forked branch must inherit all buffers from parent registry")
    (assert (= (list-length (.-buffers branch)) (.-size reg2)) "Forked branch buffer count must match root registry size")
    (assert (= (.-created-at-ms branch) 1725793200000) "Branch created timestamp must be initialized")
    true))

(df test-speculative-write-isolation [] -> Bool
  :d "Verifies branch buffer mutations and new file creation without leaking into pristine root registry."
  (let [(reg0 (v/vfs-write (v/vfs-init) "app.asl" "version 1.0"))
        (branch0 (vf/fork-vfs-branch reg0 "spec-overlay"))
        (branch1 (vf/write-branch-buffer branch0 "app.asl" "version 2.0-speculative"))
        (branch2 (vf/write-branch-buffer branch1 "helper.asl" "helper code"))
        (root-app (option-or (v/vfs-read reg0 "app.asl") (v/VFSBuffer :path "" :content "" :base-content "" :cas-hash "" :base-hash "" :revision 0 :dirty true :loaded-at 0)))
        (buf-app (option-or (get-branch-buffer branch2 "app.asl") (v/VFSBuffer :path "" :content "" :base-content "" :cas-hash "" :base-hash "" :revision 0 :dirty false :loaded-at 0)))
        (buf-hlp (option-or (get-branch-buffer branch2 "helper.asl") (v/VFSBuffer :path "" :content "" :base-content "" :cas-hash "" :base-hash "" :revision 0 :dirty false :loaded-at 0)))]
    (assert (= (.-size reg0) 1) "Root registry size must remain 1 after branch writes")
    (assert (= (list-length (.-active-paths reg0)) 1) "Root registry active paths must not include speculative paths")
    (assert (= (.-content root-app) "version 1.0") "Root registry buffer content must remain pristine")
    (assert (is-none? (v/vfs-read reg0 "helper.asl")) "New speculative path must not exist in root registry")
    (assert (and (= (.-content buf-app) "version 2.0-speculative") (and (= (.-revision buf-app) 2) (.-dirty buf-app))) "Branch buffer must have updated content revision and dirty flag")
    (assert (and (= (.-content buf-hlp) "helper code") (.-dirty buf-hlp)) "Newly added branch buffer must have staged content and dirty flag")
    true))

(df test-commit-branch [] -> Bool
  :d "Verifies committing active branch merges dirty and new buffers into root registry and non-active branch leaves registry untouched."
  (let [(reg0 (v/vfs-write (v/vfs-init) "base.txt" "base v1"))
        (branch0 (vf/fork-vfs-branch reg0 "commit-feature"))
        (branch1 (vf/write-branch-buffer branch0 "base.txt" "base v2 committed"))
        (branch2 (vf/write-branch-buffer branch1 "new.txt" "new file payload"))
        (reg1 (vf/commit-branch! branch2 reg0))
        (buf-base (option-or (v/vfs-read reg1 "base.txt") (v/VFSBuffer :path "" :content "" :base-content "" :cas-hash "" :base-hash "" :revision 0 :dirty true :loaded-at 0)))
        (buf-new (option-or (v/vfs-read reg1 "new.txt") (v/VFSBuffer :path "" :content "" :base-content "" :cas-hash "" :base-hash "" :revision 0 :dirty true :loaded-at 0)))
        (aborted (vf/abort-branch branch2))
        (reg-noop (vf/commit-branch! aborted reg0))]
    (assert (= (.-size reg1) 2) "Committed registry size must increase to 2")
    (assert (= (list-length (.-active-paths reg1)) 2) "Committed registry active paths count must equal 2")
    (assert (= (.-content buf-base) "base v2 committed") "Committed buffer content must match branch modification")
    (assert (= (.-content buf-new) "new file payload") "New buffer from branch must be committed to registry")
    (assert (= (.-size reg-noop) 1) "Committing aborted non-active branch must leave registry unmodified")
    (assert (is-none? (v/vfs-read reg-noop "new.txt")) "Non-active branch commit must not introduce new paths to root")
    true))

(df test-abort-branch [] -> Bool
  :d "Verifies branch aborting transitions status to aborted, prunes speculative deltas, rejects writes, and ignores commit."
  (let [(reg0 (v/vfs-write (v/vfs-init) "config.asn" "(:mode :prod)"))
        (branch0 (vf/fork-vfs-branch reg0 "bad-experiment"))
        (branch1 (vf/write-branch-buffer branch0 "config.asn" "(:mode :corrupt)"))
        (branch2 (vf/write-branch-buffer branch1 "temp.log" "error log"))
        (aborted (vf/abort-branch branch2))
        (rejected-write (vf/write-branch-buffer aborted "config.asn" "(:mode :fixed)"))
        (reg-after-abort (vf/commit-branch! aborted reg0))]
    (assert (= (.-status aborted) "aborted") "Aborted branch status must be aborted")
    (assert (= (list-length (.-buffers aborted)) 0) "Aborting branch must prune all speculative buffer deltas")
    (assert (= (.-status rejected-write) "aborted") "Write to aborted branch must be rejected preserving aborted status")
    (assert (= (list-length (.-buffers rejected-write)) 0) "Write to aborted branch must not add buffers")
    (assert (= (.-size reg-after-abort) 1) "Committing aborted branch must not alter root registry size")
    true))

(df test-branch-diff-and-multi-fork [] -> Bool
  :d "Verifies cross-branch isolation across parallel forks and unified diff calculation on modified, clean, and missing paths."
  (let [(reg0 (v/vfs-write (v/vfs-write (v/vfs-init) "doc.txt" "line 1\nline 2") "static.txt" "unchanged line"))
        (branch-a0 (vf/fork-vfs-branch reg0 "branch-a"))
        (branch-b0 (vf/fork-vfs-branch reg0 "branch-b"))
        (branch-a1 (vf/write-branch-buffer branch-a0 "doc.txt" "line 1\nline 2 modified"))
        (branch-b1 (vf/write-branch-buffer branch-b0 "doc.txt" "line 1\nline 2 alternate"))
        (buf-a (option-or (get-branch-buffer branch-a1 "doc.txt") (v/VFSBuffer :path "" :content "" :base-content "" :cas-hash "" :base-hash "" :revision 0 :dirty false :loaded-at 0)))
        (buf-b (option-or (get-branch-buffer branch-b1 "doc.txt") (v/VFSBuffer :path "" :content "" :base-content "" :cas-hash "" :base-hash "" :revision 0 :dirty false :loaded-at 0)))
        (diff-a-opt (vf/branch-diff branch-a1 "doc.txt"))
        (diff-a (option-or diff-a-opt ""))
        (diff-static (option-or (vf/branch-diff branch-a1 "static.txt") "non-empty-fallback"))
        (diff-missing (vf/branch-diff branch-a1 "missing.txt"))
        (diff-b (option-or (vf/branch-diff branch-b1 "doc.txt") ""))]
    (assert (!= (.-content buf-a) (.-content buf-b)) "Independent parallel branches must maintain distinct content payloads")
    (assert (is-some? diff-a-opt) "Branch diff on modified file must return some diff string")
    (assert (string-contains? diff-a "+line 2 modified") "Branch diff must contain added line delta")
    (assert (= diff-static "") "Branch diff on clean unchanged file must emit empty string")
    (assert (is-none? diff-missing) "Branch diff on non-existent path must return None")
    (assert (string-contains? diff-b "+line 2 alternate") "Branch B diff must contain its own unique modification")
    true))

(df test-compensating-sagas [] -> Bool
  :d "Verifies registration of compensating actions and automated cleanup upon branch abort."
  (let [(reg0 (v/vfs-init))
        (branch0 (vf/fork-vfs-branch reg0 "saga-branch"))
        (saga1 (vf/CompensatingAction :action-id "saga-kill-proc" :kind "process" :target "pid:4291" :handler "procSignal:SIGKILL" :registered-at-ms 1773410000000))
        (saga2 (vf/CompensatingAction :action-id "saga-release-lock" :kind "lock" :target "flock:/tmp/engine.lock" :handler "unlink" :registered-at-ms 1773410001000))
        (branch1 (vf/register-compensation branch0 saga1))
        (branch2 (vf/register-compensation branch1 saga2))
        (aborted (vf/abort-branch branch2))]
    (assert (= (list-length (.-compensations branch0)) 0) "Initial branch must have 0 compensating actions")
    (assert (= (list-length (.-compensations branch2)) 2) "Branch2 must have 2 registered compensating actions")
    (assert (= (.-status aborted) "aborted") "Aborted branch status must be aborted")
    (assert (= (list-length (.-compensations aborted)) 0) "Aborting branch must flush all compensating actions")
    true))

(df run-tests [] -> Bool
  :d "Executes comprehensive speculative VFS branching test suite with 33 strict assertions."
  (and (test-fork-vfs-branch)
       (and (test-speculative-write-isolation)
            (and (test-commit-branch)
                 (and (test-abort-branch)
                      (and (test-compensating-sagas)
                           (test-branch-diff-and-multi-fork)))))))
