(module asl-mem/egraph
  :d "Pure ASL in-memory AST equivalence graphs and congruence closure engine"
  :x [ENode
      EClass
      EGraph
      egraph-new
      egraph-find
      egraph-add-node
      add-enode
      egraph-union
      merge-eclasses
      egraph-rebuild
      egraph-extract
      egraph-saturate]
  :i [(token_regex :a tr)])

(dfs ENode
  (:f op Str "Operator symbol or literal value")
  (:f children (List I64) "List of child e-class IDs"))

(dfs EClass
  (:f id I64 "Unique equivalence class identifier")
  (:f nodes (List ENode) "Equivalent expressions in this e-class")
  (:f parents (List I64) "Parent e-class IDs that reference this class"))

(dfs EGraph
  (:f classes (Map I64 EClass) "Mapping of class ID to EClass record")
  (:f union-find (Map I64 I64) "Disjoint-set forest mapping class ID to parent class ID")
  (:f memo (Map Str I64) "Hash-consing memo table mapping canonical node key string to class ID")
  (:f next-id I64 "Monotonically increasing next available class ID"))

(dfs Pattern
  (:f op Str "Operator symbol, literal, or variable identifier")
  (:f is-var Bool "Flag indicating if pattern node is a rewrite variable")
  (:f children (List Pattern) "Sub-pattern children"))

(dfs RuleMatch
  (:f class-id I64 "E-class ID that matched LHS pattern")
  (:f subst (Map Str I64) "Variable substitution mapping"))

(df is-var-token? [(token Str)] -> Bool
  :d "Determines whether a token symbol represents a rewrite pattern variable."
  (or (string-starts-with? token "?")
      (or (= token "x")
          (or (= token "y")
              (or (= token "a") (= token "b"))))))

(df enode-equal? [(a ENode) (b ENode)] -> Bool
  :d "Checks structural equality of two e-nodes."
  (and (= (.-op a) (.-op b))
       (= (.-children a) (.-children b))))

(df list-dedup-i64 [(items (List I64))] -> (List I64)
  :d "Deduplicates a list of 64-bit integers preserving insertion order."
  (fold (fn [(acc (List I64)) (item I64)] -> (List I64)
          (if (list-contains? acc item)
            acc
            (list-append acc (list item))))
        (list)
        items))

(df list-dedup-enodes [(nodes (List ENode))] -> (List ENode)
  :d "Deduplicates a list of e-nodes."
  (fold (fn [(acc (List ENode)) (n ENode)] -> (List ENode)
          (let [(exists (fold (fn [(found Bool) (existing ENode)] -> Bool
                                (if found true (enode-equal? n existing)))
                              false
                              acc))]
            (if exists
              acc
              (list-append acc (list n)))))
        (list)
        nodes))

(df insert-sorted-i64 [(sorted (List I64)) (val I64)] -> (List I64)
  :d "Inserts an integer into a sorted list maintaining ascending order."
  (let [(len (list-length sorted))]
    (if (= len 0)
      (list val)
      (let [(inserted (fold (fn [(acc (Pair (List I64) Bool)) (elem I64)] -> (Pair (List I64) Bool)
                              (if (.-second acc)
                                (pair (list-append (.-first acc) (list elem)) true)
                                (if (<= val elem)
                                  (pair (list-append (list-append (.-first acc) (list val)) (list elem)) true)
                                  (pair (list-append (.-first acc) (list elem)) false))))
                            (pair (list) false)
                            sorted))]
        (if (.-second inserted)
          (.-first inserted)
          (list-append (.-first inserted) (list val)))))))

(df sort-i64 [(items (List I64))] -> (List I64)
  :d "Sorts a list of integer class IDs in ascending order for deterministic iteration."
  (fold (fn [(acc (List I64)) (x I64)] -> (List I64)
          (insert-sorted-i64 acc x))
        (list)
        items))

(df format-node-key [(op Str) (children (List I64))] -> Str
  :d "Formats an operator and its canonical child IDs into a deterministic memo key."
  (let [(children-str (fold (fn [(acc Str) (c I64)] -> Str
                              (if (= acc "")
                                (string-from-int64 c)
                                (str acc "," (string-from-int64 c))))
                            ""
                            children))]
    (str op "(" children-str ")")))

(df canonicalize-node [(eg EGraph) (node ENode)] -> ENode
  :d "Canonicalizes node children by replacing each child class ID with its canonical representative root."
  (let [(canon-children (map (fn [(c I64)] -> I64 (egraph-find eg c)) (.-children node)))]
    (ENode :op (.-op node) :children canon-children)))

(df add-parent-to-class [(classes (Map I64 EClass)) (child-id I64) (parent-id I64)] -> (Map I64 EClass)
  :d "Adds a parent class ID to child class parent references if not already present."
  (mt (map-get classes child-id)
    ((none) classes)
    ((some cls)
     (if (list-contains? (.-parents cls) parent-id)
       classes
       (let [(updated (EClass :id (.-id cls)
                              :nodes (.-nodes cls)
                              :parents (list-append (.-parents cls) (list parent-id))))]
         (map-set classes child-id updated))))))

(df egraph-new [] -> EGraph
  :d "Initializes an empty e-graph with empty classes, union-find forest, memo table, and starting ID of 1."
  (EGraph
    :classes (map-empty)
    :union-find (map-empty)
    :memo (map-empty)
    :next-id 1))

(df egraph-find [(eg EGraph) (id I64)] -> I64
  :d "Resolves canonical root representative of class ID in union-find forest, defaulting to id if absent."
  (mt (map-get (.-union-find eg) id)
    ((none) id)
    ((some parent)
     (if (= parent id)
       id
       (egraph-find eg parent)))))

(df egraph-add-node [(eg EGraph) (op Str) (children (List I64))] -> (Pair EGraph I64)
  :d "Canonicalizes node children, checks memo hash-consing table, adds EClass and updates memo."
  (let [(canon-children (map (fn [(c I64)] -> I64 (egraph-find eg c)) children))
        (node (ENode :op op :children canon-children))
        (key (format-node-key op canon-children))]
    (mt (map-get (.-memo eg) key)
      ((some existing-id)
       (pair eg (egraph-find eg existing-id)))
      ((none)
       (let [(new-id (.-next-id eg))
             (new-cls (EClass :id new-id :nodes (list node) :parents (list)))
             (updated-classes (map-set (.-classes eg) new-id new-cls))
             (linked-classes (fold (fn [(acc (Map I64 EClass)) (cid I64)] -> (Map I64 EClass)
                                     (add-parent-to-class acc cid new-id))
                                   updated-classes
                                   canon-children))
             (new-uf (map-set (.-union-find eg) new-id new-id))
             (new-memo (map-set (.-memo eg) key new-id))
             (new-eg (EGraph :classes linked-classes
                             :union-find new-uf
                             :memo new-memo
                             :next-id (+ new-id 1)))]
         (pair new-eg new-id))))))

(df add-enode [(eg EGraph) (op Str) (children (List I64))] -> (Pair EGraph I64)
  :d "Convenience alias for egraph-add-node."
  (egraph-add-node eg op children))

(df egraph-union [(eg EGraph) (id1 I64) (id2 I64)] -> EGraph
  :d "Unifies two equivalence classes and links their disjoint sets."
  (let [(root1 (egraph-find eg id1))
        (root2 (egraph-find eg id2))]
    (if (= root1 root2)
      eg
      (let [(leader (if (< root1 root2) root1 root2))
            (subordinate (if (< root1 root2) root2 root1))
            (cls-leader (option-or (map-get (.-classes eg) leader)
                                   (EClass :id leader :nodes (list) :parents (list))))
            (cls-sub (option-or (map-get (.-classes eg) subordinate)
                                (EClass :id subordinate :nodes (list) :parents (list))))
            (merged-nodes (list-dedup-enodes (list-append (.-nodes cls-leader) (.-nodes cls-sub))))
            (merged-parents (list-dedup-i64 (list-append (.-parents cls-leader) (.-parents cls-sub))))
            (merged-cls (EClass :id leader :nodes merged-nodes :parents merged-parents))
            (new-uf (map-set (.-union-find eg) subordinate leader))
            (classes-with-leader (map-set (.-classes eg) leader merged-cls))
            (new-classes (map-remove classes-with-leader subordinate))]
        (EGraph :classes new-classes
                :union-find new-uf
                :memo (.-memo eg)
                :next-id (.-next-id eg))))))

(df merge-eclasses [(eg EGraph) (id1 I64) (id2 I64)] -> EGraph
  :d "Convenience alias for egraph-union."
  (egraph-union eg id1 id2))

(df rebuild-step [(eg EGraph)] -> (Pair EGraph Bool)
  :d "Executes a single pass of canonicalization, deduplication, memo reconstruction, and upward congruence propagation."
  (let [(sorted-ids (sort-i64 (map-keys (.-classes eg))))
        (canon-pass (fold (fn [(acc EGraph) (cid I64)] -> EGraph
                            (mt (map-get (.-classes acc) cid)
                              ((none) acc)
                              ((some cls)
                               (let [(canon-nodes (list-dedup-enodes
                                                    (map (fn [(n ENode)] -> ENode (canonicalize-node acc n))
                                                         (.-nodes cls))))
                                     (canon-parents (list-dedup-i64
                                                      (map (fn [(p I64)] -> I64 (egraph-find acc p))
                                                           (.-parents cls))))
                                     (updated-cls (EClass :id cid :nodes canon-nodes :parents canon-parents))]
                                 (EGraph :classes (map-set (.-classes acc) cid updated-cls)
                                         :union-find (.-union-find acc)
                                         :memo (.-memo acc)
                                         :next-id (.-next-id acc))))))
                          eg
                          sorted-ids))]
    (rebuild-memo-and-congruence canon-pass)))

(df rebuild-memo-and-congruence [(eg EGraph)] -> (Pair EGraph Bool)
  :d "Rebuilds the memo table and detects congruence merges across equivalent parent nodes."
  (let [(sorted-ids (sort-i64 (map-keys (.-classes eg))))]
    (fold (fn [(acc (Pair EGraph Bool)) (cid I64)] -> (Pair EGraph Bool)
            (let [(cur-eg (.-first acc))
                  (any-merged (.-second acc))]
              (mt (map-get (.-classes cur-eg) cid)
                ((none) acc)
                ((some cls)
                 (fold (fn [(inner (Pair EGraph Bool)) (node ENode)] -> (Pair EGraph Bool)
                         (let [(in-eg (.-first inner))
                               (in-merged (.-second inner))
                               (key (format-node-key (.-op node) (.-children node)))]
                           (mt (map-get (.-memo in-eg) key)
                             ((none)
                              (let [(up-memo (map-set (.-memo in-eg) key (egraph-find in-eg cid)))
                                    (up-eg (EGraph :classes (.-classes in-eg)
                                                   :union-find (.-union-find in-eg)
                                                   :memo up-memo
                                                   :next-id (.-next-id in-eg)))]
                                (pair up-eg in-merged)))
                             ((some existing-id)
                              (let [(root1 (egraph-find in-eg existing-id))
                                    (root2 (egraph-find in-eg cid))]
                                (if (!= root1 root2)
                                  (let [(united (egraph-union in-eg root1 root2))]
                                    (pair united true))
                                  (pair in-eg in-merged)))))))
                       acc
                       (.-nodes cls))))))
          (pair (EGraph :classes (.-classes eg)
                        :union-find (.-union-find eg)
                        :memo (map-empty)
                        :next-id (.-next-id eg))
                false)
          sorted-ids)))

(df egraph-rebuild-loop [(eg EGraph) (fuel I64)] -> EGraph
  :d "Iteratively executes rebuild passes until fixpoint convergence or fuel exhaustion."
  (if (<= fuel 0)
    eg
    (let [(step (rebuild-step eg))]
      (if (.-second step)
        (egraph-rebuild-loop (.-first step) (- fuel 1))
        (.-first step)))))

(df egraph-rebuild [(eg EGraph)] -> EGraph
  :d "Restores congruence closure invariants, deduplicating equivalent terms and propagating equalities to parent nodes."
  (egraph-rebuild-loop eg 20))

(df extract-node [(eg EGraph) (node ENode) (visited (List I64))] -> (Pair I64 Str)
  :d "Computes the cost and canonical S-expression string for an individual e-node."
  (if (list-empty? (.-children node))
    (pair 1 (.-op node))
    (let [(children (.-children node))
          (extracted (fold (fn [(acc (Pair I64 (List Str))) (cid I64)] -> (Pair I64 (List Str))
                             (let [(sub (extract-class eg cid visited))
                                   (cost (+ (.-first acc) (.-first sub)))
                                   (exprs (list-append (.-second acc) (list (.-second sub))))]
                               (pair cost exprs)))
                           (pair 1 (list))
                           children))
          (total-cost (.-first extracted))
          (child-exprs (.-second extracted))
          (joined (fold (fn [(acc Str) (s Str)] -> Str
                          (if (= acc "") s (str acc " " s)))
                        ""
                        child-exprs))
          (expr (str "(" (.-op node) " " joined ")"))]
      (pair total-cost expr))))

(df extract-class [(eg EGraph) (class-id I64) (visited (List I64))] -> (Pair I64 Str)
  :d "Finds the minimal-cost canonical AST representation within an equivalence class, guarding against cycles."
  (let [(root (egraph-find eg class-id))]
    (if (list-contains? visited root)
      (pair 999999 "")
      (mt (map-get (.-classes eg) root)
        ((none) (pair 999999 ""))
        ((some cls)
         (let [(nodes (.-nodes cls))]
           (if (list-empty? nodes)
             (pair 999999 "")
             (let [(next-visited (list-append visited (list root)))]
               (fold (fn [(best (Pair I64 Str)) (n ENode)] -> (Pair I64 Str)
                       (let [(cur (extract-node eg n next-visited))]
                         (if (< (.-first cur) (.-first best))
                           cur
                           (if (and (= (.-first cur) (.-first best))
                                    (< (string-length (.-second cur)) (string-length (.-second best))))
                             cur
                             best))))
                     (pair 999999 "")
                     nodes)))))))))

(df egraph-extract [(eg EGraph) (class-id I64)] -> Str
  :d "Extracts the minimal-cost canonical S-expression for the given equivalence class."
  (let [(res (extract-class eg class-id (list)))]
    (.-second res)))

(df parse-pattern-tokens [(tokens (List Str))] -> (Pair (Option Pattern) (List Str))
  :d "Parses tokens into a single Pattern AST node and remaining unparsed tokens."
  (if (list-empty? tokens)
    (pair (none) (list))
    (let [(head (option-or (list-head tokens) ""))
          (tail (option-or (list-tail tokens) (list)))]
      (if (= head "(")
        (if (list-empty? tail)
          (pair (none) (list))
          (let [(op (option-or (list-head tail) ""))
                (after-op (option-or (list-tail tail) (list)))
                (res (parse-pattern-list after-op (list)))]
            (pair (some (Pattern :op op :is-var false :children (.-first res)))
                  (.-second res))))
        (pair (some (Pattern :op head :is-var (is-var-token? head) :children (list)))
              tail)))))

(df parse-pattern-list [(tokens (List Str)) (acc (List Pattern))] -> (Pair (List Pattern) (List Str))
  :d "Parses a sequence of child pattern tokens until a closing delimiter."
  (if (list-empty? tokens)
    (pair acc (list))
    (let [(head (option-or (list-head tokens) ""))]
      (if (= head ")")
        (pair acc (option-or (list-tail tokens) (list)))
        (let [(parsed (parse-pattern-tokens tokens))]
          (mt (.-first parsed)
            ((none) (pair acc (.-second parsed)))
            ((some pat)
             (parse-pattern-list (.-second parsed) (list-append acc (list pat))))))))))

(df parse-pattern [(s Str)] -> (Option Pattern)
  :d "Parses an S-expression pattern string into a Pattern AST."
  (let [(tokens (tr/tokenize-pattern (string-trim s)))
        (res (parse-pattern-tokens tokens))]
    (.-first res)))

(df match-child-patterns [(eg EGraph) (pats (List Pattern)) (children (List I64)) (subst (Map Str I64))] -> (Option (Map Str I64))
  :d "Matches child patterns sequentially against child class IDs."
  (if (list-empty? pats)
    (some subst)
    (let [(p-head (option-or (list-head pats) (Pattern :op "" :is-var false :children (list))))
          (c-head (option-or (list-head children) 0))
          (p-tail (option-or (list-tail pats) (list)))
          (c-tail (option-or (list-tail children) (list)))
          (matched (match-pattern-node eg p-head c-head subst))]
      (mt matched
        ((none) (none))
        ((some next-subst)
         (match-child-patterns eg p-tail c-tail next-subst))))))

(df match-pattern-node [(eg EGraph) (pat Pattern) (class-id I64) (subst (Map Str I64))] -> (Option (Map Str I64))
  :d "Matches a pattern against an equivalence class, binding variables in subst."
  (if (.-is-var pat)
    (let [(var-name (.-op pat))]
      (mt (map-get subst var-name)
        ((some bound-id)
         (if (= (egraph-find eg bound-id) (egraph-find eg class-id))
           (some subst)
           (none)))
        ((none)
         (some (map-set subst var-name (egraph-find eg class-id))))))
    (let [(root (egraph-find eg class-id))]
      (mt (map-get (.-classes eg) root)
        ((none) (none))
        ((some cls)
         (if (list-empty? (.-children pat))
           (let [(has-leaf (fold (fn [(acc Bool) (n ENode)] -> Bool
                                   (if acc true
                                     (and (= (.-op n) (.-op pat))
                                          (list-empty? (.-children n)))))
                                 false
                                 (.-nodes cls)))]
             (if has-leaf (some subst) (none)))
           (fold (fn [(acc (Option (Map Str I64))) (n ENode)] -> (Option (Map Str I64))
                   (mt acc
                     ((some s) (some s))
                     ((none)
                      (if (and (= (.-op n) (.-op pat))
                               (= (list-length (.-children n)) (list-length (.-children pat))))
                        (match-child-patterns eg (.-children pat) (.-children n) subst)
                        (none)))))
                 (none)
                 (.-nodes cls))))))))

(df find-rule-matches [(eg EGraph) (lhs Pattern)] -> (List RuleMatch)
  :d "Scans all canonical classes in the e-graph to find matches for the LHS pattern."
  (let [(sorted-ids (sort-i64 (map-keys (.-classes eg))))]
    (fold (fn [(acc (List RuleMatch)) (cid I64)] -> (List RuleMatch)
            (let [(matched (match-pattern-node eg lhs cid (map-empty)))]
              (mt matched
                ((none) acc)
                ((some subst)
                 (list-append acc (list (RuleMatch :class-id cid :subst subst)))))))
          (list)
          sorted-ids)))

(df instantiate-pattern [(eg EGraph) (pat Pattern) (subst (Map Str I64))] -> (Pair EGraph I64)
  :d "Constructs the RHS pattern AST in the e-graph, substituting bound variable classes."
  (if (.-is-var pat)
    (let [(var-name (.-op pat))]
      (mt (map-get subst var-name)
        ((some cid) (pair eg (egraph-find eg cid)))
        ((none) (egraph-add-node eg var-name (list)))))
    (if (list-empty? (.-children pat))
      (egraph-add-node eg (.-op pat) (list))
      (let [(child-step (fold (fn [(acc (Pair EGraph (List I64))) (cp Pattern)] -> (Pair EGraph (List I64))
                                (let [(cur-eg (.-first acc))
                                      (cur-ids (.-second acc))
                                      (sub (instantiate-pattern cur-eg cp subst))]
                                  (pair (.-first sub) (list-append cur-ids (list (.-second sub))))))
                              (pair eg (list))
                              (.-children pat)))]
        (egraph-add-node (.-first child-step) (.-op pat) (.-second child-step))))))

(df apply-rules-pass [(eg EGraph) (rules (List (Pair Str Str)))] -> (Pair EGraph Bool)
  :d "Executes a single round of pattern matching and instantiation for all rewrite rules."
  (let [(parsed-rules (fold (fn [(acc (List (Pair Pattern Pattern))) (r (Pair Str Str))] -> (List (Pair Pattern Pattern))
                              (let [(lhs-opt (parse-pattern (.-first r)))
                                    (rhs-opt (parse-pattern (.-second r)))]
                                (mt lhs-opt
                                  ((none) acc)
                                  ((some lhs)
                                   (mt rhs-opt
                                     ((none) acc)
                                     ((some rhs)
                                      (list-append acc (list (pair lhs rhs)))))))))
                            (list)
                            rules))]
    (fold (fn [(acc (Pair EGraph Bool)) (rule (Pair Pattern Pattern))] -> (Pair EGraph Bool)
            (let [(cur-eg (.-first acc))
                  (cur-changed (.-second acc))
                  (lhs (.-first rule))
                  (rhs (.-second rule))
                  (matches (find-rule-matches cur-eg lhs))]
              (fold (fn [(m-acc (Pair EGraph Bool)) (m RuleMatch)] -> (Pair EGraph Bool)
                      (let [(m-eg (.-first m-acc))
                            (m-changed (.-second m-acc))
                            (inst (instantiate-pattern m-eg rhs (.-subst m)))
                            (inst-eg (.-first inst))
                            (rhs-id (.-second inst))
                            (matched-id (.-class-id m))
                            (root-lhs (egraph-find inst-eg matched-id))
                            (root-rhs (egraph-find inst-eg rhs-id))]
                        (if (!= root-lhs root-rhs)
                          (let [(united (egraph-union inst-eg root-lhs root-rhs))]
                            (pair united true))
                          (pair inst-eg m-changed))))
                    (pair cur-eg cur-changed)
                    matches)))
          (pair eg false)
          parsed-rules)))

(df egraph-saturate-loop [(eg EGraph) (rules (List (Pair Str Str))) (fuel I64)] -> EGraph
  :d "Iteratively applies rewrite rules and restores congruence closure until saturation fixpoint."
  (if (<= fuel 0)
    eg
    (let [(pass (apply-rules-pass eg rules))
          (new-eg (.-first pass))
          (changed (.-second pass))]
      (if changed
        (let [(rebuilt (egraph-rebuild new-eg))]
          (egraph-saturate-loop rebuilt rules (- fuel 1)))
        new-eg))))

(df egraph-saturate [(eg EGraph) (rules (List (Pair Str Str)))] -> EGraph
  :d "Applies rewrite rules iteratively until congruence fixpoint or iteration ceiling is reached."
  (egraph-saturate-loop eg rules 15))
