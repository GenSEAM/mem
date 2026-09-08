(module asl-mem/tests/vfs-test
  :d "Unit verification suite for VFSBuffer, VFSRegistry, CAS hashing, overwrites, and diffing."
  :x [run-tests
      test-vfs-init
      test-buffer-creation-and-read
      test-cas-content-addressing
      test-in-memory-overwrites
      test-dirty-buffer-diffing]
  :i [(vfs :a v)])

(df test-vfs-init [] -> Bool
  :d "Verifies initial empty registry state, buffer count, active paths list, and non-existent lookups."
  (let [(reg (v/vfs-init))
        (lookup (v/vfs-read reg "nonexistent.asl"))]
    (assert (= (.-size reg) 0) "Initial registry size must equal 0")
    (assert (= (list-length (.-active-paths reg)) 0) "Initial active paths list must be empty")
    (assert (is-none? lookup) "Lookup of non-existent buffer must return None")
    (assert (= (map-size (.-buffers reg)) 0) "Initial buffer map size must be 0")
    (assert (= (list-length (map-keys (.-buffers reg))) 0) "Initial buffer map keys count must be 0")
    true))

(df test-buffer-creation-and-read [] -> Bool
  :d "Verifies buffer creation, path normalization, content retention, revision initialization, and dirty status."
  (let [(reg0 (v/vfs-init))
        (reg1 (v/vfs-write reg0 "./src/core/model.asl" "(module model :x [])"))
        (lookup (v/vfs-read reg1 "src/core/model.asl"))]
    (assert (is-some? lookup) "Newly written buffer must be retrievable by normalized path")
    (let [(buf (option-or lookup (v/VFSBuffer :path "" :content "" :base-content "" :cas-hash "" :base-hash "" :revision 0 :dirty true :loaded-at 0)))]
      (assert (= (.-path buf) "src/core/model.asl") "Path must be normalized by stripping leading dot-slash")
      (assert (= (.-content buf) "(module model :x [])") "Buffer payload must match written content")
      (assert (= (.-revision buf) 1) "Initial buffer revision counter must equal 1")
      (assert (not (.-dirty buf)) "Freshly staged buffer must not be dirty relative to its base")
      true)))

(df test-cas-content-addressing [] -> Bool
  :d "Verifies deterministic CAS hashing, sensitivity to changes, and base-hash tracking."
  (let [(c1 "alpha payload")
        (c2 "alpha payload")
        (c3 "beta payload")
        (h1 (v/vfs-cas-hash c1))
        (h2 (v/vfs-cas-hash c2))
        (h3 (v/vfs-cas-hash c3))
        (reg (v/vfs-write (v/vfs-init) "pkg/test.asl" c1))
        (buf (option-or (v/vfs-read reg "pkg/test.asl") (v/VFSBuffer :path "" :content "" :base-content "" :cas-hash "" :base-hash "" :revision 0 :dirty true :loaded-at 0)))]
    (assert (= h1 h2) "Identical content payloads must produce identical CAS hashes")
    (assert (!= h1 h3) "Different content payloads must produce distinct CAS hashes")
    (assert (= (string-length h1) 64) "CAS hash digest must be a 64-character hexadecimal string")
    (assert (= (.-cas-hash buf) h1) "Buffer cas-hash field must match hash of current content")
    (assert (= (.-base-hash buf) h1) "Buffer base-hash field must match hash of pristine base content")
    true))

(df test-in-memory-overwrites [] -> Bool
  :d "Verifies monotonic revision increments, dirty state transitions, base reset, and path registry stability."
  (let [(reg0 (v/vfs-init))
        (base-text "(module state-v1 :x [run])")
        (edit-text "(module state-v2 :x [run stop])")
        (reg1 (v/vfs-write reg0 "app/state.asl" base-text))
        (reg2 (v/vfs-write reg1 "app/state.asl" edit-text))
        (buf2 (option-or (v/vfs-read reg2 "app/state.asl") (v/VFSBuffer :path "" :content "" :base-content "" :cas-hash "" :base-hash "" :revision 0 :dirty false :loaded-at 0)))
        (reg3 (v/vfs-write reg2 "app/state.asl" base-text))
        (buf3 (option-or (v/vfs-read reg3 "app/state.asl") (v/VFSBuffer :path "" :content "" :base-content "" :cas-hash "" :base-hash "" :revision 0 :dirty false :loaded-at 0)))]
    (assert (= (.-revision buf2) 2) "Second write to same path must increment revision to 2")
    (assert (.-dirty buf2) "Mutating content away from base version must set dirty flag to true")
    (assert (= (.-size reg2) 1) "Overwriting existing buffer must not duplicate registry entries")
    (assert (= (.-revision buf3) 3) "Third write reverting content must increment revision to 3")
    (assert (not (.-dirty buf3)) "Reverting content back to base version must clear dirty flag to false")
    true))

(df test-dirty-buffer-diffing [] -> Bool
  :d "Verifies unified diff computation on clean buffers, dirty buffers, line changes, and context lines."
  (let [(reg0 (v/vfs-init))
        (base-text "line 1: prelude\nline 2: body\nline 3: closing")
        (mod-text "line 1: prelude\nline 2: modified body\nline 3: closing\nline 4: footer")
        (reg1 (v/vfs-write reg0 "doc.txt" base-text))
        (clean-buf (option-or (v/vfs-read reg1 "doc.txt") (v/VFSBuffer :path "" :content "" :base-content "" :cas-hash "" :base-hash "" :revision 0 :dirty true :loaded-at 0)))
        (clean-diff (v/vfs-diff clean-buf))
        (reg2 (v/vfs-write reg1 "doc.txt" mod-text))
        (dirty-buf (option-or (v/vfs-read reg2 "doc.txt") (v/VFSBuffer :path "" :content "" :base-content "" :cas-hash "" :base-hash "" :revision 0 :dirty false :loaded-at 0)))
        (dirty-diff (v/vfs-diff dirty-buf))]
    (assert (= (list-length clean-diff) 0) "Diff on clean buffer must return empty list of delta lines")
    (assert (> (list-length dirty-diff) 0) "Diff on dirty buffer must return non-empty delta lines")
    (assert (list-contains? dirty-diff " line 1: prelude") "Diff must preserve unchanged line 1 as context")
    (assert (list-contains? dirty-diff "-line 2: body") "Diff must flag deleted or replaced base line with minus prefix")
    (assert (list-contains? dirty-diff "+line 2: modified body") "Diff must flag added or modified current line with plus prefix")
    (assert (list-contains? dirty-diff "+line 4: footer") "Diff must flag newly appended line with plus prefix")
    true))

(df run-tests [] -> Bool
  :d "Executes comprehensive VFS and CAS source editor test suite with 26 strict assertions."
  (and (test-vfs-init)
       (and (test-buffer-creation-and-read)
            (and (test-cas-content-addressing)
                 (and (test-in-memory-overwrites)
                      (test-dirty-buffer-diffing))))))
