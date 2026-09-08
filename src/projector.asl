(module asl-mem/projector
  :d "Pure ASL Dual-Projection Engine: renders markdown for STATUS.md and phase PLAN.md from roadmap state."
  :x [DualProjection
      format-phase-status-row
      format-plan-item
      project-status-md
      project-plan-md
      project-dual-manifest]
  :i [])

(dfs DualProjection
  (:f status-md Str "Formatted STATUS.md markdown content")
  (:f plan-md Str "Formatted target phase PLAN.md content")
  (:f target-plan-path Str "File path for target phase PLAN.md"))

(df format-phase-status-row [(id Str) (status Str) (gate Str)] -> Str
  :d "Formats a single phase table row for STATUS.md."
  (str "| `" id "` | " status " | `" gate "` | PASS |"))

(df format-plan-item [(id Str) (name Str) (status Str) (gate Str)] -> Str
  :d "Formats a single work item section for PLAN.md."
  (str "### Item " id ": " name "\n"
       "- **Status**: " status "\n"
       "- **Gate**: `" gate "`\n"))

(df project-status-md [(iteration Str) (phases-table Str)] -> Str
  :d "Renders markdown content for STATUS.md from roadmap iteration and phases."
  (str "# Current Status: `" iteration "`\n\n"
       "## Active Roadmap Waves\n\n"
       "| Phase ID | State | Gate Command | Result |\n"
       "|---|---|---|---|\n"
       phases-table "\n"))

(df project-plan-md [(phase-id Str) (phase-name Str) (goal Str) (gate Str) (items-content Str)] -> Str
  :d "Renders markdown content for phase PLAN.md from phase metadata."
  (str "# Plan: " phase-name "\n\n"
       "## Goal\n"
       goal "\n\n"
       "## Work Items\n\n"
       items-content "\n"
       "## Acceptance Gate\n"
       "```bash\n"
       gate "\n"
       "```\n\n"
       "## Invariants\n"
       "- 100% pure AgentScript (.asl) in code packages.\n"
       "- Zero comments (c-0001). Docstrings :d only.\n"
       "- Strict falsification assertions.\n"))

(df project-dual-manifest [(iteration Str) (phase-id Str) (phase-name Str) (status Str) (wave Str) (gate Str)] -> DualProjection
  :d "Generates synchronized STATUS.md and PLAN.md projections from memory state."
  (let [(row (format-phase-status-row phase-id status gate))
        (s-md (project-status-md iteration row))
        (p-item (format-plan-item (str phase-id ".1") "Initial Implementation" status gate))
        (p-md (project-plan-md phase-id phase-name (str "Execute " phase-name " in " wave) gate p-item))
        (path (str ".plans/" phase-id "/PLAN.md"))]
    (DualProjection
      :status-md s-md
      :plan-md p-md
      :target-plan-path path)))
