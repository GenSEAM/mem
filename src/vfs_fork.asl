(module asl-mem/vfs-fork
  :d "In-memory speculative VFS branching, snapshot forking, and transaction commit/abort engine"
  :x [VFSBranch
      fork-vfs-branch
      write-branch-buffer
      commit-branch!
      abort-branch
      branch-diff]
  :i [(vfs :a v)])

(dfs VFSBranch
  (:f branch-id Str "Unique speculative branch identifier")
  (:f parent-id Str "Parent snapshot or registry identifier e.g. root")
  (:f created-at-ms I64 "Epoch millisecond timestamp of branch creation")
  (:f buffers (List v/VFSBuffer) "List of isolated speculative VFS buffers")
  (:f status Str "Lifecycle status of branch: active, committed, or aborted"))

(df normalize-branch-path [(path Str)] -> Str
  :d "Normalizes virtual file path by trimming whitespace and stripping leading dot-slash or slash prefixes."
  (let [(trimmed (string-trim path))]
    (if (string-starts-with? trimmed "./")
      (option-or (string-slice trimmed 2 (string-length trimmed)) "")
      (if (string-starts-with? trimmed "/")
        (option-or (string-slice trimmed 1 (string-length trimmed)) "")
        trimmed))))

(df find-branch-buffer [(buffers (List v/VFSBuffer)) (target-path Str)] -> (Option v/VFSBuffer)
  :d "Finds buffer in list matching canonical target path."
  (fold (fn [(acc (Option v/VFSBuffer)) (buf v/VFSBuffer)] -> (Option v/VFSBuffer)
          (mt acc
            ((some b) (some b))
            ((none)
             (if (= (.-path buf) target-path)
               (some buf)
               (none)))))
        (none)
        buffers))

(df update-or-append-buffer [(buffers (List v/VFSBuffer)) (target-path Str) (new-buf v/VFSBuffer)] -> (List v/VFSBuffer)
  :d "Replaces matching buffer in list or appends if absent."
  (let [(found (fold (fn [(acc Bool) (b v/VFSBuffer)] -> Bool
                       (if acc true (= (.-path b) target-path)))
                     false
                     buffers))]
    (if found
      (fold (fn [(acc (List v/VFSBuffer)) (b v/VFSBuffer)] -> (List v/VFSBuffer)
              (if (= (.-path b) target-path)
                (list-append acc (list new-buf))
                (list-append acc (list b))))
            (list)
            buffers)
      (list-append buffers (list new-buf)))))

(df fork-vfs-branch [(registry v/VFSRegistry) (branch-id Str)] -> VFSBranch
  :d "Forks an isolated speculative VFS branch overlay from a base registry."
  (let [(paths (.-active-paths registry))
        (bufs-map (.-buffers registry))
        (branch-buffers (fold (fn [(acc (List v/VFSBuffer)) (p Str)] -> (List v/VFSBuffer)
                                (mt (map-get bufs-map p)
                                  ((none) acc)
                                  ((some b) (list-append acc (list b)))))
                              (list)
                              paths))]
    (VFSBranch
      :branch-id branch-id
      :parent-id "root"
      :created-at-ms 1725793200000
      :buffers branch-buffers
      :status "active")))

(df write-branch-buffer [(branch VFSBranch) (path Str) (content Str)] -> VFSBranch
  :d "Stages content mutations into a speculative branch buffer overlay without mutating root registry."
  (if (!= (.-status branch) "active")
    branch
    (let [(norm (normalize-branch-path path))
          (new-hash (v/vfs-cas-hash content))
          (existing-opt (find-branch-buffer (.-buffers branch) norm))
          (new-buf (mt existing-opt
                     ((none)
                      (v/VFSBuffer
                        :path norm
                        :content content
                        :base-content ""
                        :cas-hash new-hash
                        :base-hash ""
                        :revision 1
                        :dirty true
                        :loaded-at 0))
                     ((some prev)
                      (let [(base-h (.-base-hash prev))
                            (base-c (.-base-content prev))
                            (is-dirty (!= new-hash base-h))]
                        (v/VFSBuffer
                          :path norm
                          :content content
                          :base-content base-c
                          :cas-hash new-hash
                          :base-hash base-h
                          :revision (+ (.-revision prev) 1)
                          :dirty is-dirty
                          :loaded-at (.-loaded-at prev))))))
          (updated-buffers (update-or-append-buffer (.-buffers branch) norm new-buf))]
      (VFSBranch
        :branch-id (.-branch-id branch)
        :parent-id (.-parent-id branch)
        :created-at-ms (.-created-at-ms branch)
        :buffers updated-buffers
        :status (.-status branch)))))

(df commit-branch! [(branch VFSBranch) (registry v/VFSRegistry)] -> v/VFSRegistry
  :d "Merges modified and newly added branch buffers into root registry if branch is active."
  (if (!= (.-status branch) "active")
    registry
    (fold (fn [(acc-reg v/VFSRegistry) (b v/VFSBuffer)] -> v/VFSRegistry
            (if (or (.-dirty b) (is-none? (v/vfs-read acc-reg (.-path b))))
              (v/vfs-write acc-reg (.-path b) (.-content b))
              acc-reg))
          registry
          (.-buffers branch))))

(df abort-branch [(branch VFSBranch)] -> VFSBranch
  :d "Aborts speculative branch by setting status to aborted and discarding pending buffers."
  (VFSBranch
    :branch-id (.-branch-id branch)
    :parent-id (.-parent-id branch)
    :created-at-ms (.-created-at-ms branch)
    :buffers (list)
    :status "aborted"))

(df branch-diff [(branch VFSBranch) (path Str)] -> (Option Str)
  :d "Computes unified diff string for specified path in branch relative to base content."
  (let [(norm (normalize-branch-path path))
        (buf-opt (find-branch-buffer (.-buffers branch) norm))]
    (mt buf-opt
      ((none) (none))
      ((some b)
       (if (not (.-dirty b))
         (some "")
         (let [(diff-lines (v/vfs-diff b))]
           (some (string-join diff-lines "\n"))))))))
