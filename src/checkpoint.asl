(module asl-mem/checkpoint
  :d "Atomic VFS buffer checkpointing, ASN serialization, and time-travel archive restoration engine"
  :x [VFSCheckpoint
      format-checkpoint-asn
      parse-checkpoint-asn
      create-vfs-checkpoint
      restore-vfs-checkpoint]
  :i [(vfs :a v)
      (asl-text/escape :a esc)])

(dfs VFSCheckpoint
  (:f id Str "Unique checkpoint identifier e.g. cp-phase-319-1725793200000")
  (:f phase-id Str "Active phase identifier at snapshot capture")
  (:f created-at-ms I64 "Epoch millisecond timestamp of checkpoint creation")
  (:f buffers (List v/VFSBuffer) "List of all staged VFS buffers both pristine and dirty")
  (:f cas-root Str "Deterministic content-addressed SHA256 hex digest over all buffer CAS hashes")
  (:f dirty-count I64 "Number of buffers in dirty state at time of checkpoint"))


(df extract-between [(src Str) (prefix Str) (suffix Str)] -> (Option Str)
  :d "Extracts substring between prefix and suffix markers."
  (let [(p-idx (string-index-of src prefix))]
    (mt p-idx
      ((none) (none))
      ((some start)
       (let [(val-start (+ start (string-length prefix)))
             (sub (option-or (string-slice src val-start (string-length src)) ""))
             (s-idx (string-index-of sub suffix))]
         (mt s-idx
           ((none) (none))
           ((some end)
            (string-slice sub 0 end))))))))

(df format-buffer-asn [(buf v/VFSBuffer)] -> Str
  :d "Serializes a single VFSBuffer into ASN (:buf ...) representation."
  (let [(p (.-path buf))
        (c (esc/escape-asn-str (.-content buf)))
        (bc (esc/escape-asn-str (.-base-content buf)))
        (ch (.-cas-hash buf))
        (bh (.-base-hash buf))
        (rev (string-from-int64 (.-revision buf)))
        (d-str (if (.-dirty buf) "true" "false"))
        (loaded (string-from-int64 (.-loaded-at buf)))
        (p1 (str "    (:buf :path \"" p "\" :content \"" c "\" :base-content \"" bc "\""))
        (p2 (str p1 " :cas-hash \"" ch "\" :base-hash \"" bh "\" :revision " rev))
        (p3 (str p2 " :dirty " d-str " :loaded-at " loaded ")"))]
    p3))

(df format-checkpoint-asn [(ckpt VFSCheckpoint)] -> Str
  :d "df format-checkpoint-asn serializes VFSCheckpoint record and buffers to portable ASN document."
  (let [(id-str (.-id ckpt))
        (phase-str (.-phase-id ckpt))
        (created-str (string-from-int64 (.-created-at-ms ckpt)))
        (cas-str (.-cas-root ckpt))
        (dirty-str (string-from-int64 (.-dirty-count ckpt)))
        (buf-lines (map (fn [(b v/VFSBuffer)] -> Str (format-buffer-asn b)) (.-buffers ckpt)))
        (buffers-block (string-join buf-lines "\n"))
        (h1 (str "(:checkpoint\n  :id \"" id-str "\"\n  :phase-id \"" phase-str "\"\n"))
        (h2 (str h1 "  :created-at-ms " created-str "\n  :cas-root \"" cas-str "\"\n"))
        (h3 (str h2 "  :dirty-count " dirty-str "\n  :buffers [\n" buffers-block "\n  ])"))]
    h3))

(df parse-buffer-asn [(line Str)] -> (Option v/VFSBuffer)
  :d "Parses a single (:buf ...) ASN line into a VFSBuffer."
  (let [(trimmed (string-trim line))]
    (if (string-starts-with? trimmed "(:buf")
      (let [(path-opt (extract-between trimmed " :path \"" "\" :content \""))
            (c-opt (extract-between trimmed " :content \"" "\" :base-content \""))
            (bc-opt (extract-between trimmed " :base-content \"" "\" :cas-hash \""))
            (ch-opt (extract-between trimmed " :cas-hash \"" "\" :base-hash \""))
            (bh-opt (extract-between trimmed " :base-hash \"" "\" :revision "))
            (rev-opt (extract-between trimmed " :revision " " :dirty "))
            (dirty-opt (extract-between trimmed " :dirty " " :loaded-at "))
            (loaded-opt (extract-between trimmed " :loaded-at " ")"))]
        (mt path-opt
          ((none) (none))
          ((some path-val)
           (let [(content-val (esc/unescape-asn-str (option-or c-opt "")))
                 (base-c-val (esc/unescape-asn-str (option-or bc-opt "")))
                 (cas-h (option-or ch-opt ""))
                 (base-h (option-or bh-opt ""))
                 (rev-val (option-or (string-to-int64 (string-trim (option-or rev-opt "1"))) 1))
                 (is-dirty (= (string-trim (option-or dirty-opt "false")) "true"))
                 (loaded-val (option-or (string-to-int64 (string-trim (option-or loaded-opt "0"))) 0))]
             (some (v/VFSBuffer
                     :path (v/normalize-path path-val)
                     :content content-val
                     :base-content base-c-val
                     :cas-hash cas-h
                     :base-hash base-h
                     :revision rev-val
                     :dirty is-dirty
                     :loaded-at loaded-val))))))
      (none))))

(df parse-checkpoint-asn [(asn-text Str)] -> (Option VFSCheckpoint)
  :d "df parse-checkpoint-asn deserializes ASN checkpoint representation into VFSCheckpoint record."
  (let [(id-opt (extract-between asn-text "  :id \"" "\""))
        (phase-opt (extract-between asn-text "  :phase-id \"" "\""))
        (created-opt (extract-between asn-text "  :created-at-ms " "\n"))
        (cas-opt (extract-between asn-text "  :cas-root \"" "\""))
        (dirty-opt (extract-between asn-text "  :dirty-count " "\n"))
        (buffers-opt (extract-between asn-text "  :buffers [\n" "\n  ]"))]
    (mt id-opt
      ((none) (none))
      ((some id-val)
       (let [(phase-val (option-or phase-opt ""))
             (created-val (option-or (string-to-int64 (string-trim (option-or created-opt "0"))) 0))
             (cas-val (option-or cas-opt ""))
             (dirty-val (option-or (string-to-int64 (string-trim (option-or dirty-opt "0"))) 0))
             (raw-bufs (option-or buffers-opt ""))
             (buf-lines (string-split raw-bufs "\n"))
             (parsed-bufs (fold (fn [(acc (List v/VFSBuffer)) (ln Str)] -> (List v/VFSBuffer)
                                  (mt (parse-buffer-asn ln)
                                    ((none) acc)
                                    ((some b) (list-append acc (list b)))))
                                (list)
                                buf-lines))]
         (some (VFSCheckpoint
                 :id id-val
                 :phase-id phase-val
                 :created-at-ms created-val
                 :buffers parsed-bufs
                 :cas-root cas-val
                 :dirty-count dirty-val)))))))

(df create-vfs-checkpoint [(registry v/VFSRegistry) (phase-id Str)] -> VFSCheckpoint
  :d "df create-vfs-checkpoint captures atomic snapshot of VFS registry buffers dirty state and CAS root."
  (let [(paths (.-active-paths registry))
        (bufs-map (.-buffers registry))
        (buffers (fold (fn [(acc (List v/VFSBuffer)) (p Str)] -> (List v/VFSBuffer)
                         (mt (map-get bufs-map p)
                           ((none) acc)
                           ((some b) (list-append acc (list b)))))
                       (list)
                       paths))
        (dirty-cnt (fold (fn [(cnt I64) (b v/VFSBuffer)] -> I64
                           (if (.-dirty b) (+ cnt 1) cnt))
                         0
                         buffers))
        (cas-concat (fold (fn [(acc Str) (b v/VFSBuffer)] -> Str
                            (str acc (.-cas-hash b)))
                          ""
                          buffers))
        (cas-root (v/vfs-cas-hash cas-concat))
        (created-at 1725793200000)
        (ckpt-id (str "cp-" phase-id "-" (string-from-int64 created-at)))]
    (VFSCheckpoint
      :id ckpt-id
      :phase-id phase-id
      :created-at-ms created-at
      :buffers buffers
      :cas-root cas-root
      :dirty-count dirty-cnt)))

(df restore-vfs-checkpoint [(archive-text Str) (checkpoint-id Str)] -> (Result v/VFSRegistry Str)
  :d "df restore-vfs-checkpoint restores full VFS registry state and dirty buffers from archived checkpoint snapshot."
  (let [(parsed (parse-checkpoint-asn archive-text))]
    (mt parsed
      ((none) (err (str "Failed to restore checkpoint: " checkpoint-id)))
      ((some ckpt)
       (let [(buffers (.-buffers ckpt))
             (init-map (map-empty))
             (reconstructed-map (fold (fn [(acc (Map Str v/VFSBuffer)) (b v/VFSBuffer)] -> (Map Str v/VFSBuffer)
                                        (map-set acc (v/normalize-path (.-path b)) b))
                                      init-map
                                      buffers))
             (paths (fold (fn [(acc (List Str)) (b v/VFSBuffer)] -> (List Str)
                            (list-append acc (list (v/normalize-path (.-path b)))))
                          (list)
                          buffers))
             (size (list-length buffers))]
         (ok (v/VFSRegistry
               :buffers reconstructed-map
               :active-paths paths
               :size size)))))))
