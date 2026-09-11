(module asl-mem/tests/visibility-scope-test
  :d "Unit test suite verifying 3-tier workspace, worktree and session visibility scoping rules per ADR-0080"
  :x [run-tests
      test-isolated-session-scope
      test-workspace-lineage-scope
      test-sovereign-system-scope
      test-global-anchor-predicate]
  :i [(visibility_scope :a vs)])

(df test-isolated-session-scope [] -> Bool
  :d "Verifies that unanchored session outside workspace sees only own session and zero worktrees"
  (let [(ctx (vs/make-visibility-context
               "/tmp/scratch"
               "sess-unanchored"
               "/Users/purplelephant/.asl"
               false
               ""
               ""
               (list)
               (list "/Users/purplelephant/projects/asex" "/Users/purplelephant/projects/datahub-react")
               (list)
               (list "sess-unanchored" "sess-peer-1" "sess-peer-2")
               (list "repo-asex" "repo-datahub-react")))]
    (assert (= (.-tier ctx) (vs/tier-isolated-session)) "Tier must resolve to tier-isolated-session")
    (assert (= (list-length (.-visible-worktrees ctx)) 0) "Zero worktrees must be visible in isolated session")
    (assert (= (list-length (.-visible-repos ctx)) 0) "Zero repositories must be visible in isolated session")
    (assert (= (list-length (.-visible-session-ids ctx)) 1) "Only own session must be visible")
    (assert (= (head (.-visible-session-ids ctx)) "sess-unanchored") "Visible session must match active session ID")
    true))

(df test-workspace-lineage-scope [] -> Bool
  :d "Verifies that workspace-anchored session sees workspace and child worktrees but not external repos"
  (let [(child-wts (list "/Users/purplelephant/projects/worktrees/asex-feat-1" "/Users/purplelephant/projects/worktrees/asex-hotfix"))
        (all-wts (list "/Users/purplelephant/projects/asex" "/Users/purplelephant/projects/worktrees/asex-feat-1" "/Users/purplelephant/projects/datahub-react" "/Users/purplelephant/projects/fronts/react"))
        (ws-sessions (list "sess-child-agent-1"))
        (all-sessions (list "sess-main" "sess-child-agent-1" "sess-datahub-agent"))
        (ctx (vs/make-visibility-context
               "/Users/purplelephant/projects/asex"
               "sess-main"
               "/Users/purplelephant/.asl"
               true
               "/Users/purplelephant/projects/asex"
               "repo-asex"
               child-wts
               all-wts
               ws-sessions
               all-sessions
               (list "repo-asex" "repo-datahub-react")))]
    (assert (= (.-tier ctx) (vs/tier-workspace-lineage)) "Tier must resolve to tier-workspace-lineage")
    (assert (= (.-active-workspace-root ctx) "/Users/purplelephant/projects/asex") "Active workspace root must match current workspace")
    (assert (= (list-length (.-visible-worktrees ctx)) 3) "Visible worktrees must contain workspace root and both children")
    (assert (= (list-length (.-visible-repos ctx)) 1) "Only current repository must be visible")
    (assert (= (head (.-visible-repos ctx)) "repo-asex") "Visible repository must be repo-asex")
    (assert (= (list-length (.-visible-session-ids ctx)) 2) "Only active and child workspace sessions must be visible")
    true))

(df test-sovereign-system-scope [] -> Bool
  :d "Verifies that navigating to folder containing global .asl unlocks full panoptic system visibility"
  (let [(all-wts (list "/Users/purplelephant/projects/asex" "/Users/purplelephant/projects/datahub-react" "/Users/purplelephant/projects/pcp"))
        (all-sessions (list "sess-1" "sess-2" "sess-3"))
        (all-repos (list "repo-asex" "repo-datahub-react" "repo-pcp"))
        (ctx (vs/make-visibility-context
               "/Users/purplelephant"
               "sess-sovereign-admin"
               "/Users/purplelephant/.asl"
               false
               ""
               ""
               (list)
               all-wts
               (list)
               all-sessions
               all-repos))]
    (assert (= (.-tier ctx) (vs/tier-sovereign-system)) "Tier must resolve to tier-sovereign-system")
    (assert (= (list-length (.-visible-worktrees ctx)) 3) "All 3 system worktrees must be visible")
    (assert (= (list-length (.-visible-repos ctx)) 3) "All 3 system repositories must be visible")
    (assert (= (list-length (.-visible-session-ids ctx)) 3) "All 3 system sessions must be visible")
    true))

(df test-global-anchor-predicate [] -> Bool
  :d "Verifies is-global-asl-anchor? accurately distinguishes global anchor directory from child folders"
  (assert (vs/is-global-asl-anchor? "/Users/purplelephant" "/Users/purplelephant/.asl") "Exact parent of .asl must return true")
  (assert (vs/is-global-asl-anchor? "/Users/purplelephant/" "/Users/purplelephant/.asl/") "Trailing slash variants must return true")
  (assert (not (vs/is-global-asl-anchor? "/Users/purplelephant/projects/asex" "/Users/purplelephant/.asl")) "Child workspace must return false")
  (assert (not (vs/is-global-asl-anchor? "/tmp" "/Users/purplelephant/.asl")) "Unrelated path must return false")
  true)

(df run-tests [] -> Bool
  :d "Executes all unit tests in visibility scope suite"
  (let [(t1 (test-isolated-session-scope))
        (t2 (test-workspace-lineage-scope))
        (t3 (test-sovereign-system-scope))
        (t4 (test-global-anchor-predicate))]
    (and t1 (and t2 (and t3 t4)))))
