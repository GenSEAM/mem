(module asl-mem/notes
  :d "Pure AgentScript machine-native notes ledger engine: typed observation structs, dual-projection Markdown rendering, and query filtering."
  :x [NoteRecord
      make-note
      note-is-active?
      note-has-tag?
      note-scope-matches?
      note-matches-query?
      format-note-asn
      project-note-md
      note-promote-to-task
      note-promote-to-adr
      note-close-scope
      note-prune-bounded]
  :i [(asl-text/escape :a esc)])

(dfs NoteRecord
  (:f id Str "Canonical note identifier e.g. n1 or n-0001")
  (:f topic Str "High-level domain or subsystem topic")
  (:f author Str "Agent archetype or user role that authored the note")
  (:f session Str "Session ID in which observation occurred")
  (:f date Str "Date of recording YYYY-MM-DD")
  (:f status Str "Lifecycle status: active, promoted-to-task, promoted-to-adr, archived")
  (:f scope Any "List of affected file paths or packages, or scope lifetime")
  (:f tags (List Str) "Categorization tags e.g. tokens, density, refactor")
  (:f fact Str "Distilled empirical observation or grounded finding")
  (:f evidence Str "Physical execution citation, test run, or AST symbol evidence")
  (:f consequence Str "Architectural implication or trade-off"))

(df make-note [p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11] -> NoteRecord
  :d "Constructs a typed NoteRecord, supporting both 5-arg (id, fact, topic, tags, scope) and 11-arg forms."
  (if (= p6 null)
    (NoteRecord
      :id p1
      :topic p3
      :author "agent"
      :session "sess-current"
      :date "2026-09-12"
      :status "active"
      :scope p5
      :tags (if (= p4 null) (list) p4)
      :fact p2
      :evidence ""
      :consequence "")
    (NoteRecord
      :id p1
      :topic p2
      :author p3
      :session p4
      :date p5
      :status p6
      :scope p7
      :tags p8
      :fact p9
      :evidence p10
      :consequence p11)))

(df note-is-active? [(note NoteRecord)] -> Bool
  :d "Checks if a note is currently active."
  (= (.-status note) "active"))

(df note-has-tag? [(note NoteRecord) (tag Str)] -> Bool
  :d "Checks if a note contains a specific tag."
  (let [(matches (filter (fn [(t Str)] -> Bool (= t tag)) (.-tags note)))]
    (> (list-length matches) 0)))

(df note-scope-matches? [(note NoteRecord) (path Str)] -> Bool
  :d "Checks if a note scope matches a specific file path or scope lifecycle."
  (let [(sc (.-scope note))]
    (or (= sc path)
        (string-contains? sc path)
        (string-contains? path sc))))

(df note-matches-query? [(note NoteRecord) (query Str)] -> Bool
  :d "Evaluates case-sensitive substring matching across topic, fact, tags, and id."
  (let [(in-id (string-contains? (.-id note) query))
        (in-topic (string-contains? (.-topic note) query))
        (in-fact (string-contains? (.-fact note) query))
        (in-tag (note-has-tag? note query))]
    (or in-id (or in-topic (or in-fact in-tag)))))

(df format-note-asn [(note NoteRecord)] -> Str
  :d "Serializes NoteRecord into canonical machine ASN representation."
  (let [(sc (.-scope note))
        (scope-str (if (> (list-length sc) 0)
                     (let [(scope-quoted (map (fn [(s Str)] -> Str (str "\"" (esc/escape-asn-str s) "\"")) sc))]
                       (str "[" (string-join scope-quoted " ") "]"))
                     (str "\"" (esc/escape-asn-str sc) "\"")))
        (tags-quoted (map (fn [(t Str)] -> Str (str ":" t)) (.-tags note)))
        (tags-str (str "[" (string-join tags-quoted " ") "]"))]
    (str "(:note\n"
         "  :id \"" (esc/escape-asn-str (.-id note)) "\"\n"
         "  :topic \"" (esc/escape-asn-str (.-topic note)) "\"\n"
         "  :author \"" (esc/escape-asn-str (.-author note)) "\"\n"
         "  :session \"" (esc/escape-asn-str (.-session note)) "\"\n"
         "  :date \"" (esc/escape-asn-str (.-date note)) "\"\n"
         "  :status :" (.-status note) "\n"
         "  :scope " scope-str "\n"
         "  :tags " tags-str "\n"
         "  :fact \"" (esc/escape-asn-str (.-fact note)) "\"\n"
         "  :evidence \"" (esc/escape-asn-str (.-evidence note)) "\"\n"
         "  :consequence \"" (esc/escape-asn-str (.-consequence note)) "\")")))

(df project-note-md [(note NoteRecord)] -> Str
  :d "Renders NoteRecord into GitHub Flavored Markdown on-demand for human inspection."
  (let [(scope-lines (map (fn [(s Str)] -> Str (str "- `" s "`\n")) (.-scope note)))
        (scope-body (if (= (list-length scope-lines) 0) "- None\n" (string-join scope-lines "")))
        (tag-badges (map (fn [(t Str)] -> Str (str "`#" t "`")) (.-tags note)))
        (tag-header (string-join tag-badges " "))]
    (str "# Note [" (.-id note) "]: " (.-topic note) "\n\n"
         "- **Author**: " (.-author note) "\n"
         "- **Session**: `" (.-session note) "`\n"
         "- **Date**: " (.-date note) "\n"
         "- **Status**: " (.-status note) "\n"
         "- **Tags**: " tag-header "\n\n"
         "## Scope\n"
         scope-body "\n"
         "## Empirical Fact\n"
         (.-fact note) "\n\n"
         "## Grounded Evidence\n"
         (.-evidence note) "\n\n"
         "## Architectural Consequence\n"
         (.-consequence note))))

(df note-promote-to-task [(note NoteRecord) (task-id Str)] -> NoteRecord
  :d "Transitions note status to promoted-to-task with task citation appended to consequence."
  (NoteRecord
    :id (.-id note)
    :topic (.-topic note)
    :author (.-author note)
    :session (.-session note)
    :date (.-date note)
    :status "promoted-to-task"
    :scope (.-scope note)
    :tags (.-tags note)
    :fact (.-fact note)
    :evidence (.-evidence note)
    :consequence (str (.-consequence note) " (Promoted to task: " task-id ")")))

(df note-promote-to-adr [(note NoteRecord) (adr-id Str)] -> NoteRecord
  :d "Transitions note status to promoted-to-adr with ADR citation appended to consequence."
  (NoteRecord
    :id (.-id note)
    :topic (.-topic note)
    :author (.-author note)
    :session (.-session note)
    :date (.-date note)
    :status "promoted-to-adr"
    :scope (.-scope note)
    :tags (.-tags note)
    :fact (.-fact note)
    :evidence (.-evidence note)
    :consequence (str (.-consequence note) " (Promoted to ADR: " adr-id ")")))

(df note-close-scope [(notes (List NoteRecord)) (closing-scope Str)] -> (List NoteRecord)
  :d "Retires ephemeral notes belonging to the closed scope while preserving promoted notes and higher-tier scopes."
  (filter (fn [(n NoteRecord)] -> Bool
            (let [(sc (.-scope n))
                  (st (.-status n))]
              (if (or (= st "promoted-to-task") (= st "promoted-to-adr"))
                true
                (if (= closing-scope "step")
                  (not (= sc "step"))
                  (if (= closing-scope "task")
                    (and (not (= sc "step")) (not (= sc "task")))
                    true)))))
          notes))

(df note-prune-bounded [(notes (List NoteRecord)) (max-active Int)] -> (List NoteRecord)
  :d "Keeps the active note store strictly bounded under continuous note authoring."
  (let [(len (list-length notes))]
    (if (<= len max-active)
      notes
      (let [(excess (- len max-active))
            (rem (option-unwrap (list-slice notes excess len)))]
        rem))))

