(module asl-mem/tests/egraph-test
  :d "Comprehensive falsifiable test suite verifying AST equivalence e-graphs, congruence closure, and equality saturation"
  :x [run-tests
      test-egraph-new
      test-egraph-add-node-leaves
      test-egraph-memo-hash-consing
      test-egraph-union-and-find
      test-egraph-rebuild-congruence
      test-egraph-extract-minimal-ast
      test-egraph-saturate-algebraic-rules]
  :i [(egraph :a e)])

(df test-egraph-new [] -> Bool
  :d "Verifies initial e-graph state: next-id starts at 1 and all internal tables are empty."
  (let [(eg (e/egraph-new))]
    (assert (= (.-next-id eg) 1) "Initial next-id must be 1")
    (assert (= (list-length (map-keys (.-classes eg))) 0) "Classes map must be initialized empty")
    (assert (= (list-length (map-keys (.-union-find eg))) 0) "Union-find map must be initialized empty")
    (assert (= (list-length (map-keys (.-memo eg))) 0) "Memo table must be initialized empty")
    true))

(df test-egraph-add-node-leaves [] -> Bool
  :d "Verifies leaf node addition, monotonic ID allocation, class creation, and identity canonical roots."
  (let [(eg0 (e/egraph-new))
        (res1 (e/egraph-add-node eg0 "x" (list)))
        (eg1 (.-first res1))
        (id1 (.-second res1))
        (res2 (e/egraph-add-node eg1 "y" (list)))
        (eg2 (.-first res2))
        (id2 (.-second res2))]
    (assert (= id1 1) "First added node must receive class ID 1")
    (assert (= id2 2) "Second distinct leaf must receive class ID 2")
    (assert (= (.-next-id eg2) 3) "next-id must increment to 3 after two distinct node additions")
    (assert (= (e/egraph-find eg2 id1) 1) "Unmerged class 1 must resolve to canonical root 1")
    (assert (= (e/egraph-find eg2 id2) 2) "Unmerged class 2 must resolve to canonical root 2")
    true))

(df test-egraph-memo-hash-consing [] -> Bool
  :d "Verifies hash-consing memo table hits, class reuse, and prevention of redundant class allocations."
  (let [(eg0 (e/egraph-new))
        (res1 (e/egraph-add-node eg0 "a" (list)))
        (eg1 (.-first res1))
        (id1 (.-second res1))
        (res2 (e/add-enode eg1 "a" (list)))
        (eg2 (.-first res2))
        (id2 (.-second res2))
        (res3 (e/egraph-add-node eg2 "b" (list)))
        (eg3 (.-first res3))
        (id3 (.-second res3))
        (res4 (e/egraph-add-node eg3 "+" (list id1 id3)))
        (eg4 (.-first res4))
        (id4 (.-second res4))
        (res5 (e/egraph-add-node eg4 "+" (list id2 id3)))
        (eg5 (.-first res5))
        (id5 (.-second res5))]
    (assert (= id1 id2) "Duplicate leaf node insertion must return identical class ID via memo hit")
    (assert (= (.-next-id eg2) 2) "Memo hit must not increment next-id")
    (assert (= id4 id5) "Compound node with equivalent child IDs must hit memo and reuse class ID")
    (assert (= (.-next-id eg5) 4) "Only three unique classes must be allocated")
    (assert (= (list-length (map-keys (.-classes eg5))) 3) "Class registry must contain exactly 3 classes")
    true))

(df test-egraph-union-and-find [] -> Bool
  :d "Verifies disjoint-set union, path resolution, and multi-hop transitive equivalence chains."
  (let [(eg0 (e/egraph-new))
        (r1 (e/egraph-add-node eg0 "1" (list)))
        (eg1 (.-first r1)) (id1 (.-second r1))
        (r2 (e/egraph-add-node eg1 "2" (list)))
        (eg2 (.-first r2)) (id2 (.-second r2))
        (r3 (e/egraph-add-node eg2 "3" (list)))
        (eg3 (.-first r3)) (id3 (.-second r3))
        (r4 (e/egraph-add-node eg3 "4" (list)))
        (eg4 (.-first r4)) (id4 (.-second r4))
        (eg-u1 (e/egraph-union eg4 id1 id2))
        (eg-u2 (e/egraph-union eg-u1 id3 id4))
        (eg-u3 (e/merge-eclasses eg-u2 id2 id3))]
    (assert (= (e/egraph-find eg-u1 id1) (e/egraph-find eg-u1 id2)) "Union of id1 and id2 must share canonical root")
    (assert (!= (e/egraph-find eg-u1 id1) (e/egraph-find eg-u1 id3)) "Disjoint classes id1 and id3 must remain unmerged before union")
    (assert (= (e/egraph-find eg-u2 id3) (e/egraph-find eg-u2 id4)) "Union of id3 and id4 must share canonical root")
    (assert (= (e/egraph-find eg-u3 id1) (e/egraph-find eg-u3 id4)) "Transitive merge of chains 1-2 and 3-4 via 2-3 must unify 1 and 4")
    (assert (= (e/egraph-find eg-u3 999) 999) "egraph-find on unknown ID must safely default to returning ID itself")
    true))

(df test-egraph-rebuild-congruence [] -> Bool
  :d "Verifies congruence closure, upward equivalence propagation to parents, and class unification."
  (let [(eg0 (e/egraph-new))
        (r-a (e/egraph-add-node eg0 "a" (list)))
        (eg1 (.-first r-a)) (id-a (.-second r-a))
        (r-b (e/egraph-add-node eg1 "b" (list)))
        (eg2 (.-first r-b)) (id-b (.-second r-b))
        (r-fa (e/egraph-add-node eg2 "f" (list id-a)))
        (eg3 (.-first r-fa)) (id-fa (.-second r-fa))
        (r-fb (e/egraph-add-node eg3 "f" (list id-b)))
        (eg4 (.-first r-fb)) (id-fb (.-second r-fb))
        (eg-unioned (e/egraph-union eg4 id-a id-b))
        (root-fa-before (e/egraph-find eg-unioned id-fa))
        (root-fb-before (e/egraph-find eg-unioned id-fb))
        (eg-rebuilt (e/egraph-rebuild eg-unioned))
        (root-fa-after (e/egraph-find eg-rebuilt id-fa))
        (root-fb-after (e/egraph-find eg-rebuilt id-fb))]
    (assert (!= id-fa id-fb) "Initially f(a) and f(b) must be allocated distinct class IDs")
    (assert (!= root-fa-before root-fb-before) "Before rebuild, parent nodes f(a) and f(b) must not be unified")
    (assert (= (e/egraph-find eg-rebuilt id-a) (e/egraph-find eg-rebuilt id-b)) "Leaves a and b must remain unified")
    (assert (= root-fa-after root-fb-after) "Congruence closure in egraph-rebuild must unify f(a) and f(b) to same root")
    (assert (< (list-length (map-keys (.-classes eg-rebuilt))) (list-length (map-keys (.-classes eg4)))) "Total class count must decrease after congruence rebuild")
    true))

(df test-egraph-extract-minimal-ast [] -> Bool
  :d "Verifies cost-based AST extraction selecting minimal-cost representation between compound and leaf forms."
  (let [(eg0 (e/egraph-new))
        (r-x (e/egraph-add-node eg0 "x" (list)))
        (eg1 (.-first r-x)) (id-x (.-second r-x))
        (r-0 (e/egraph-add-node eg1 "0" (list)))
        (eg2 (.-first r-0)) (id-0 (.-second r-0))
        (r-plus (e/egraph-add-node eg2 "+" (list id-x id-0)))
        (eg3 (.-first r-plus)) (id-plus (.-second r-plus))
        (expr-before (e/egraph-extract eg3 id-plus))
        (eg-merged (e/egraph-union eg3 id-plus id-x))
        (eg-rebuilt (e/egraph-rebuild eg-merged))
        (expr-after (e/egraph-extract eg-rebuilt id-plus))
        (expr-leaf (e/egraph-extract eg-rebuilt id-x))]
    (assert (= expr-before "(+ x 0)") "Before union, compound class id-plus must extract to (+ x 0)")
    (assert (= expr-after "x") "Cost-based extraction must select minimal-cost leaf x over compound (+ x 0)")
    (assert (= expr-leaf "x") "Leaf class id-x must extract to x")
    (assert (= (e/egraph-extract eg-rebuilt id-0) "0") "Leaf zero must extract to 0")
    true))

(df test-egraph-saturate-algebraic-rules [] -> Bool
  :d "Verifies algebraic rewrite rule saturation, fixpoint convergence, and canonical expression simplification."
  (let [(eg0 (e/egraph-new))
        (r-x (e/egraph-add-node eg0 "x" (list)))
        (eg1 (.-first r-x)) (id-x (.-second r-x))
        (r-0 (e/egraph-add-node eg1 "0" (list)))
        (eg2 (.-first r-0)) (id-0 (.-second r-0))
        (r-plus (e/egraph-add-node eg2 "+" (list id-x id-0)))
        (eg3 (.-first r-plus)) (id-plus (.-second r-plus))
        (rules (list (pair "(+ ?x 0)" "?x")))
        (eg-sat (e/egraph-saturate eg3 rules))
        (root-plus (e/egraph-find eg-sat id-plus))
        (root-x (e/egraph-find eg-sat id-x))
        (extracted (e/egraph-extract eg-sat id-plus))]
    (assert (= root-plus root-x) "Rule saturation must unify (+ x 0) with x")
    (assert (= extracted "x") "Minimal extraction after saturation must emit canonical irreducible expression x")
    (assert (= (e/egraph-extract eg-sat id-x) "x") "Class id-x extraction must remain x")
    (assert (= (e/egraph-find eg-sat id-plus) (e/egraph-find eg-sat id-x)) "Transitive roots must be identical in saturated graph")
    true))

(df run-tests [] -> Bool
  :d "Executes complete e-graph verification suite spanning 32 assertions across 7 test cases."
  (and (test-egraph-new)
       (and (test-egraph-add-node-leaves)
            (and (test-egraph-memo-hash-consing)
                 (and (test-egraph-union-and-find)
                      (and (test-egraph-rebuild-congruence)
                           (and (test-egraph-extract-minimal-ast)
                                (test-egraph-saturate-algebraic-rules))))))))
