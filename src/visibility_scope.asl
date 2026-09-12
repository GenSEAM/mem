(module asl-mem/visibility-scope
  :d "Hierarchical Workspace, Worktree and Session Visibility Scoping Engine per ADR-0080"
  :x [VisibilityTier
      VisibilityContext
      make-visibility-context
      is-global-asl-anchor?
      resolve-visibility-tier
      filter-visible-worktrees
      filter-visible-sessions
      filter-visible-repos]
  :i [])

(dfe VisibilityTier
  (:c tier-isolated-session [] "Unanchored session: only current session visible")
  (:c tier-workspace-lineage [] "Workspace-anchored: workspace and derived child worktrees")
  (:c tier-sovereign-system [] "Global .asl anchor: panoptic whole-system visibility"))

(dfs VisibilityContext
  (:f current-cwd Str "Normalized absolute current working directory")
  (:f active-session-id Str "Identity of the active user/agent session")
  (:f global-asl-dir Str "Path to sovereign global storage e.g. ~/.asl")
  (:f tier VisibilityTier "Effective resolved visibility tier")
  (:f active-workspace-root Str "Enclosing workspace root path if anchored")
  (:f visible-worktrees (List Str) "Set of accessible worktree paths")
  (:f visible-repos (List Str) "Set of accessible repository identifiers")
  (:f visible-session-ids (List Str) "Set of accessible session identifiers"))

(df is-global-asl-anchor? [(cwd Str) (global-asl-dir Str)] -> Bool
  :d "Returns true only if cwd directly contains the global .asl store directory"
  (let [(norm-cwd (if (string-ends-with? cwd "/") (option-or (string-slice cwd 0 (- (string-length cwd) 1)) "") cwd))
        (norm-global (if (string-ends-with? global-asl-dir "/") (option-or (string-slice global-asl-dir 0 (- (string-length global-asl-dir) 1)) "") global-asl-dir))
        (expected (str norm-cwd "/.asl"))]
    (= expected norm-global)))

(df resolve-visibility-tier [(cwd Str) (is-in-workspace Bool) (is-global-anchor Bool)] -> VisibilityTier
  :d "Resolves the 3-tier visibility level from directory and workspace membership state"
  (if is-global-anchor
    (tier-sovereign-system)
    (if is-in-workspace
      (tier-workspace-lineage)
      (tier-isolated-session))))

(df filter-visible-worktrees [(tier VisibilityTier) (current-ws Str) (child-wts (List Str)) (all-wts (List Str))] -> (List Str)
  :d "Filters visible worktree paths according to resolved tier"
  (mt tier
    ((tier-isolated-session) (list))
    ((tier-workspace-lineage) (cons current-ws child-wts))
    ((tier-sovereign-system) all-wts)))

(df filter-visible-sessions [(tier VisibilityTier) (active-sess Str) (ws-sessions (List Str)) (all-sessions (List Str))] -> (List Str)
  :d "Filters visible session IDs according to resolved tier"
  (mt tier
    ((tier-isolated-session) (list active-sess))
    ((tier-workspace-lineage) (cons active-sess ws-sessions))
    ((tier-sovereign-system) all-sessions)))

(df filter-visible-repos [(tier VisibilityTier) (current-repo Str) (all-repos (List Str))] -> (List Str)
  :d "Filters visible repository IDs according to resolved tier"
  (mt tier
    ((tier-isolated-session) (list))
    ((tier-workspace-lineage) (list current-repo))
    ((tier-sovereign-system) all-repos)))

(df make-visibility-context [(cwd Str) (active-sess Str) (global-asl-dir Str) (is-in-ws Bool) (current-ws Str) (current-repo Str) (child-wts (List Str)) (all-wts (List Str)) (ws-sessions (List Str)) (all-sessions (List Str)) (all-repos (List Str))] -> VisibilityContext
  :d "Constructs a resolved VisibilityContext record from system topology"
  (let [(is-anchor (is-global-asl-anchor? cwd global-asl-dir))
        (tier (resolve-visibility-tier cwd is-in-ws is-anchor))
        (vis-wts (filter-visible-worktrees tier current-ws child-wts all-wts))
        (vis-sess (filter-visible-sessions tier active-sess ws-sessions all-sessions))
        (vis-repos (filter-visible-repos tier current-repo all-repos))]
    (VisibilityContext
      :current-cwd cwd
      :active-session-id active-sess
      :global-asl-dir global-asl-dir
      :tier tier
      :active-workspace-root (mt tier ((tier-workspace-lineage) current-ws) (_ ""))
      :visible-worktrees vis-wts
      :visible-repos vis-repos
      :visible-session-ids vis-sess)))
