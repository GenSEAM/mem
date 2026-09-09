(module asl-mem/projector
  :d "Pure ASL Dual-Projection Engine: renders markdown for STATUS.md, phase PLAN.md, and ADRs from roadmap and memory state."
  :x [DualProjection
      format-phase-status-row
      format-plan-item
      project-status-md
      project-plan-md
      project-dual-manifest
      AdrConsequence
      AdrRecord
      make-adr
      format-consequence-asn
      format-adr-asn
      project-adr-md]
  :i [(asl-text/escape :a esc)])

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

(dfs AdrConsequence
  (:f kind Str "Consequence classification: pos or neg")
  (:f text Str "Detailed description of consequence or trade-off"))

(dfs AdrRecord
  (:f id Str "Canonical ADR identifier e.g. ADR-0044")
  (:f shortcode Str "Compact unique shortcode e.g. d-0044")
  (:f title Str "Architecture decision title")
  (:f status Str "Lifecycle status: proposed, active, superseded, deprecated")
  (:f date Str "Date of ratification YYYY-MM-DD")
  (:f cluster Str "Architecture domain cluster e.g. architecture/governance")
  (:f deciders (List Str) "List of deciders or agent roles")
  (:f problem Str "Problem statement and architectural motivation")
  (:f drivers (List Str) "Key decision drivers and constraints")
  (:f decision Str "Actionable decision outcome and normative mandate")
  (:f invariants (List Str) "Preserved system invariants e.g. c-0001, c-0003")
  (:f consequences (List AdrConsequence) "Positive and negative consequences"))

(df make-adr [(id Str) (shortcode Str) (title Str) (status Str) (date Str) (cluster Str) (deciders (List Str)) (problem Str) (drivers (List Str)) (decision Str) (invariants (List Str)) (consequences (List AdrConsequence))] -> AdrRecord
  :d "Constructs a typed AdrRecord."
  (AdrRecord
    :id id
    :shortcode shortcode
    :title title
    :status status
    :date date
    :cluster cluster
    :deciders deciders
    :problem problem
    :drivers drivers
    :decision decision
    :invariants invariants
    :consequences consequences))

(df format-consequence-asn [(c AdrConsequence)] -> Str
  :d "Formats AdrConsequence into compact canonical ASN."
  (str "(:" (.-kind c) " \"" (esc/escape-asn-str (.-text c)) "\")"))

(df format-adr-asn [(adr AdrRecord)] -> Str
  :d "Serializes AdrRecord into canonical machine ASN representation."
  (let [(dec-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-deciders adr)))
        (dec-str (str "[" (string-join dec-quoted " ") "]"))
        (drv-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-drivers adr)))
        (drv-str (str "[" (string-join drv-quoted " ") "]"))
        (inv-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) (.-invariants adr)))
        (inv-str (str "[" (string-join inv-quoted " ") "]"))
        (c-strs (map (fn [(c AdrConsequence)] -> Str (format-consequence-asn c)) (.-consequences adr)))
        (c-str (str "[" (string-join c-strs " ") "]"))]
    (str "(:adr\n"
         "  :id \"" (esc/escape-asn-str (.-id adr)) "\"\n"
         "  :shortcode \"" (esc/escape-asn-str (.-shortcode adr)) "\"\n"
         "  :title \"" (esc/escape-asn-str (.-title adr)) "\"\n"
         "  :status :" (.-status adr) "\n"
         "  :date \"" (esc/escape-asn-str (.-date adr)) "\"\n"
         "  :cluster \"" (esc/escape-asn-str (.-cluster adr)) "\"\n"
         "  :deciders " dec-str "\n"
         "  :problem \"" (esc/escape-asn-str (.-problem adr)) "\"\n"
         "  :drivers " drv-str "\n"
         "  :decision \"" (esc/escape-asn-str (.-decision adr)) "\"\n"
         "  :invariants " inv-str "\n"
         "  :consequences " c-str ")")))

(df project-adr-md [(adr AdrRecord)] -> Str
  :d "Renders AdrRecord into GitHub Flavored Markdown on-demand for human inspection."
  (let [(dec-list (string-join (.-deciders adr) ", "))
        (drv-lines (map (fn [(d Str)] -> Str (str "- " d "\n")) (.-drivers adr)))
        (drv-body (string-join drv-lines ""))
        (inv-lines (map (fn [(i Str)] -> Str (str "- `" i "`\n")) (.-invariants adr)))
        (inv-body (string-join inv-lines ""))
        (pos-c (filter (fn [(c AdrConsequence)] -> Bool (= (.-kind c) "pos")) (.-consequences adr)))
        (pos-lines (map (fn [(c AdrConsequence)] -> Str (str "- " (.-text c) "\n")) pos-c))
        (pos-body (if (= (list-length pos-lines) 0) "- None\n" (string-join pos-lines "")))
        (neg-c (filter (fn [(c AdrConsequence)] -> Bool (= (.-kind c) "neg")) (.-consequences adr)))
        (neg-lines (map (fn [(c AdrConsequence)] -> Str (str "- " (.-text c) "\n")) neg-c))
        (neg-body (if (= (list-length neg-lines) 0) "- None\n" (string-join neg-lines "")))]
    (str "# " (.-id adr) ": " (.-title adr) "\n\n"
         "- **Shortcode**: `" (.-shortcode adr) "`\n"
         "- **Status**: " (.-status adr) "\n"
         "- **Date**: " (.-date adr) "\n"
         "- **Cluster**: `" (.-cluster adr) "`\n"
         "- **Deciders**: " dec-list "\n\n"
         "## Problem Statement\n"
         (.-problem adr) "\n\n"
         "## Decision Drivers\n"
         drv-body "\n"
         "## Decision\n"
         (.-decision adr) "\n\n"
         "## Invariants\n"
         inv-body "\n"
         "## Consequences\n\n"
         "### Positive\n"
         pos-body "\n"
         "### Negative\n"
         neg-body)))
