(module asl-mem/worklist
  :d "Pure AgentScript persistent cursor-driven file worklist engine under ADR-0081."
  :x [WorklistEntry
      Worklist
      make-worklist-entry
      worklist-create
      worklist-cursor-next
      worklist-claim-batch
      worklist-mark-done
      worklist-mark-skipped
      worklist-find-entry
      worklist-is-complete?
      worklist-format-asn]
  :i [(asl-text/escape :a esc)])

(dfs WorklistEntry
  (:f path Str "Repository relative file path")
  (:f intent Str "Purpose or hypothesis for inspecting this file")
  (:f status Str "Lifecycle status: pending, reading, done, or skipped")
  (:f reason Str "Mandatory justification when status is skipped")
  (:f notes Str "Empirical findings or observations from reading")
  (:f walker-id Str "Identifier of agent walker holding current lease")
  (:f timestamp Int "Timestamp of last status transition in ms"))

(dfs Worklist
  (:f id Str "Unique worklist identifier e.g. wl-001")
  (:f cursor Int "Persistent cursor index representing progress through list")
  (:f entries (List WorklistEntry) "Ordered file worklist entries"))

(df make-worklist-entry [(path Str) (intent Str)] -> WorklistEntry
  :d "Constructs a new pending worklist entry."
  (WorklistEntry
    :path path
    :intent intent
    :status "pending"
    :reason ""
    :notes ""
    :walker-id ""
    :timestamp 1789280000000))

(df worklist-create [(id Str) (entries (List WorklistEntry))] -> Worklist
  :d "Constructs a new worklist with cursor at 0."
  (Worklist
    :id id
    :cursor 0
    :entries entries))

(df worklist-cursor-next [(wl Worklist) (batch-size Int)] -> (Map Str Any)
  :d "Yields next bounded batch of entries from current cursor and advances cursor."
  (let [(cur (.-cursor wl))
        (all (.-entries wl))
        (total (list-length all))
        (rem (if (>= cur total) 0 (- total cur)))
        (take-count (if (<= rem batch-size) rem batch-size))
        (end-pos (+ cur take-count))
        (batch (if (> take-count 0)
                 (option-unwrap (list-slice all cur end-pos))
                 (list)))
        (updated-wl (Worklist
                      :id (.-id wl)
                      :cursor end-pos
                      :entries all))]
    {:batch batch
     :cursor end-pos
     :has-more (< end-pos total)
     :worklist updated-wl}))

(df worklist-claim-batch [(wl Worklist) (walker-id Str) (batch-size Int)] -> (Map Str Any)
  :d "Claims up to batch-size pending entries for walker-id ensuring concurrent walkers never collide."
  (let [(all (.-entries wl))
        (pending (filter (fn [(e WorklistEntry)] -> Bool (= (.-status e) "pending")) all))
        (pend-len (list-length pending))
        (take-n (if (<= pend-len batch-size) pend-len batch-size))
        (to-claim (if (> take-n 0)
                    (option-unwrap (list-slice pending 0 take-n))
                    (list)))
        (claim-paths (map (fn [(e WorklistEntry)] -> Str (.-path e)) to-claim))
        (updated-entries
          (map (fn [(e WorklistEntry)] -> WorklistEntry
                 (if (list-contains? claim-paths (.-path e))
                   (WorklistEntry
                     :path (.-path e)
                     :intent (.-intent e)
                     :status "reading"
                     :reason (.-reason e)
                     :notes (.-notes e)
                     :walker-id walker-id
                     :timestamp 1789280000000)
                   e))
               all))
        (claimed-batch
          (filter (fn [(e WorklistEntry)] -> Bool
                    (and (= (.-status e) "reading") (= (.-walker-id e) walker-id)))
                  updated-entries))]
    {:claimed claimed-batch
     :count (list-length claimed-batch)
     :worklist (Worklist
                 :id (.-id wl)
                 :cursor (.-cursor wl)
                 :entries updated-entries)}))

(df worklist-mark-done [(wl Worklist) (path Str) (notes Str)] -> Worklist
  :d "Marks an entry as done and records empirical finding notes."
  (let [(updated-entries
          (map (fn [(e WorklistEntry)] -> WorklistEntry
                 (if (= (.-path e) path)
                   (WorklistEntry
                     :path (.-path e)
                     :intent (.-intent e)
                     :status "done"
                     :reason (.-reason e)
                     :notes notes
                     :walker-id (.-walker-id e)
                     :timestamp 1789280000000)
                   e))
               (.-entries wl)))]
    (Worklist
      :id (.-id wl)
      :cursor (.-cursor wl)
      :entries updated-entries)))

(df worklist-mark-skipped [(wl Worklist) (path Str) (reason Str)] -> (Result Worklist Str)
  :d "Marks an entry as skipped with mandatory non-empty reason string; rejects empty reason."
  (if (string-empty? (string-trim reason))
    (err "reason string is mandatory for skipped entries")
    (let [(updated-entries
            (map (fn [(e WorklistEntry)] -> WorklistEntry
                   (if (= (.-path e) path)
                     (WorklistEntry
                       :path (.-path e)
                       :intent (.-intent e)
                       :status "skipped"
                       :reason reason
                       :notes (.-notes e)
                       :walker-id (.-walker-id e)
                       :timestamp 1789280000000)
                     e))
                 (.-entries wl)))]
      (ok (Worklist
            :id (.-id wl)
            :cursor (.-cursor wl)
            :entries updated-entries)))))

(df worklist-find-entry [(wl Worklist) (path Str)] -> (Option WorklistEntry)
  :d "Finds an entry by path in the worklist."
  (let [(matches (filter (fn [(e WorklistEntry)] -> Bool (= (.-path e) path)) (.-entries wl)))]
    (if (> (list-length matches) 0)
      (list-get matches 0)
      (none))))

(df worklist-is-complete? [(wl Worklist)] -> Bool
  :d "Returns true if all entries are in terminal state (done or skipped)."
  (let [(pending (filter (fn [(e WorklistEntry)] -> Bool
                           (or (= (.-status e) "pending") (= (.-status e) "reading")))
                         (.-entries wl)))]
    (= (list-length pending) 0)))

(df format-entry-asn [(e WorklistEntry)] -> Str
  :d "Formats a single WorklistEntry into canonical ASN."
  (str "    (:entry"
       " :path \"" (esc/escape-asn-str (.-path e)) "\""
       " :intent \"" (esc/escape-asn-str (.-intent e)) "\""
       " :status :" (.-status e)
       " :reason \"" (esc/escape-asn-str (.-reason e)) "\""
       " :notes \"" (esc/escape-asn-str (.-notes e)) "\""
       " :walker-id \"" (esc/escape-asn-str (.-walker-id e)) "\""
       " :timestamp " (.-timestamp e) ")"))

(df worklist-format-asn [(wl Worklist)] -> Str
  :d "Serializes entire worklist into persistent ASN format."
  (let [(entry-lines (map format-entry-asn (.-entries wl)))
        (entries-body (if (= (list-length entry-lines) 0) "" (str "\n" (string-join entry-lines "\n"))))]
    (str "(:worklist\n"
         "  :id \"" (esc/escape-asn-str (.-id wl)) "\"\n"
         "  :cursor " (.-cursor wl) "\n"
         "  :entries [" entries-body "]\n"
         ")")))
