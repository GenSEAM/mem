(module asl-mem/refactor-vfs
  :d "In-memory virtual file system staged AST refactoring engine with CAS diff validation"
  :x [RefactorPlan
      stage-refactor-diff
      preview-token-savings
      commit-staged-refactor
      rollback-staged-refactor]
  :i [(vfs :a v)
      (../../asl/packages/asl-compiler/src/compact_rewrite :a cr)])

(dfs RefactorPlan
  (:f path Str "Normalized virtual file path")
  (:f base-hash Str "CAS content hash of original buffer")
  (:f staged-hash Str "CAS content hash of rewritten staged buffer")
  (:f base-content Str "Original buffer text content")
  (:f staged-content Str "Rewritten buffer text content")
  (:f diff Str "Unified diff between original and staged content")
  (:f tokens-saved I64 "Estimated token savings count")
  (:f status Str "Lifecycle status: staged, committed, rolled-back, conflict"))

(dfs CountState
  (:f cur Str "Accumulated token characters")
  (:f in-str Bool "Inside string literal")
  (:f esc Bool "Backslash escape in string")
  (:f in-cmt Bool "Inside comment")
  (:f count I64 "Match count"))

(df count-atom [(cnt I64) (atom Str) (target Str)] -> I64
  :d "Increments count if atom equals target."
  (if (= atom target) (+ cnt 1) cnt))

(df count-step [(st CountState) (ch Str) (target Str)] -> CountState
  :d "Processes one character while counting target symbol tokens."
  (if (.-in-cmt st)
    (if (= ch "\n")
      (CountState :cur "" :in-str false :esc false :in-cmt false :count (.-count st))
      (CountState :cur "" :in-str false :esc false :in-cmt true :count (.-count st)))
    (if (.-in-str st)
      (if (.-esc st)
        (CountState :cur "" :in-str true :esc false :in-cmt false :count (.-count st))
        (if (= ch "\\")
          (CountState :cur "" :in-str true :esc true :in-cmt false :count (.-count st))
          (if (= ch "\"")
            (CountState :cur "" :in-str false :esc false :in-cmt false :count (.-count st))
            (CountState :cur "" :in-str true :esc false :in-cmt false :count (.-count st)))))
      (if (= ch "\"")
        (let [(c (count-atom (.-count st) (.-cur st) target))]
          (CountState :cur "" :in-str true :esc false :in-cmt false :count c))
        (if (= ch ";")
          (let [(c (count-atom (.-count st) (.-cur st) target))]
            (CountState :cur "" :in-str false :esc false :in-cmt true :count c))
          (if (or (string-contains? "()[]{}" ch) (string-contains? " \t\n\r" ch))
            (let [(c (count-atom (.-count st) (.-cur st) target))]
              (CountState :cur "" :in-str false :esc false :in-cmt false :count c))
            (CountState :cur (str (.-cur st) ch) :in-str false :esc false :in-cmt false :count (.-count st))))))))

(df count-ast-tokens [(content Str) (target Str)] -> I64
  :d "Counts occurrences of target symbol as an AST token in content."
  (let [(chars (string-chars content))
        (init-st (CountState :cur "" :in-str false :esc false :in-cmt false :count 0))
        (final-st (fold (fn [(st CountState) (ch Str)] -> CountState
                          (count-step st ch target))
                        init-st
                        chars))]
    (count-atom (.-count final-st) (.-cur final-st) target)))

(df preview-token-savings [(content Str) (rules (List cr/CompactionRule))] -> I64
  :d "Previews estimated token savings for content under compaction rules."
  (fold (fn [(acc I64) (r cr/CompactionRule)] -> I64
          (let [(cnt (count-ast-tokens content (.-verbose r)))]
            (+ acc (* cnt (.-savings r)))))
        0
        rules))

(df stage-refactor-diff [(reg v/VFSRegistry) (path Str) (rules (List cr/CompactionRule))] -> (Option RefactorPlan)
  :d "Stages in-memory AST compaction diff against VFS buffer with CAS validation."
  (mt (v/vfs-read reg path)
    ((none) (none))
    ((some buf)
     (let [(orig-c (.-content buf))
           (staged-c (cr/rewrite-ast-tokens orig-c rules))
           (base-h (.-cas-hash buf))
           (staged-h (v/vfs-cas-hash staged-c))
           (temp-buf (v/VFSBuffer
                       :path (.-path buf)
                       :content staged-c
                       :base-content orig-c
                       :cas-hash staged-h
                       :base-hash base-h
                       :revision (+ (.-revision buf) 1)
                       :dirty (!= staged-h base-h)
                       :loaded-at (.-loaded-at buf)))
           (diff-lines (v/vfs-diff temp-buf))
           (diff-str (string-join diff-lines "\n"))
           (savings (preview-token-savings orig-c rules))]
       (some (RefactorPlan
               :path (.-path buf)
               :base-hash base-h
               :staged-hash staged-h
               :base-content orig-c
               :staged-content staged-c
               :diff diff-str
               :tokens-saved savings
               :status "staged"))))))

(df commit-staged-refactor [(reg v/VFSRegistry) (plan RefactorPlan)] -> (Option v/VFSRegistry)
  :d "Commits staged refactor to VFS buffer with CAS hash validation."
  (mt (v/vfs-read reg (.-path plan))
    ((none) (none))
    ((some cur-buf)
     (if (!= (.-cas-hash cur-buf) (.-base-hash plan))
       (none)
       (some (v/vfs-write reg (.-path plan) (.-staged-content plan)))))))

(df rollback-staged-refactor [(reg v/VFSRegistry) (plan RefactorPlan)] -> v/VFSRegistry
  :d "Rolls back staged or committed refactor in VFS buffer to pristine base content."
  (v/vfs-write reg (.-path plan) (.-base-content plan)))
