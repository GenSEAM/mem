(module asl-mem/tests/checkpoint-test
  :d "Unit verification suite for telemetry streaming, VFS checkpointing, ASN serialization, and dirty buffer restoration fidelity."
  :x [run-tests
      test-telemetry-record-and-format
      test-telemetry-streaming-persistence
      test-vfs-checkpoint-creation-and-asn-serialization
      test-checkpoint-archive-persistence-and-lookup
      test-dirty-buffer-restoration-fidelity]
  :i [(vfs :a v)
      (telemetry :a t)
      (checkpoint :a c)])

(df test-telemetry-record-and-format [] -> Bool
  :d "Verifies TelemetryRecord field values, ND-ASN formatting, and round-trip parsing fidelity."
  (let [(record (t/TelemetryRecord
                  :timestamp-ms 1725793200123
                  :phase-id "phase-319"
                  :tokens-consumed 4250
                  :duration-ms 1820
                  :rss-mb 64
                  :dirty-buffer-count 2))
        (line (t/format-telemetry-entry record))
        (parsed-opt (t/parse-telemetry-entry line))
        (parsed (option-or parsed-opt (t/TelemetryRecord :timestamp-ms 0 :phase-id "" :tokens-consumed 0 :duration-ms 0 :rss-mb 0 :dirty-buffer-count 0)))]
    (assert (string-starts-with? line "(:telem") "Formatted line must start with (:telem")
    (assert (string-contains? line ":phase \"phase-319\"") "Formatted line must contain phase identifier")
    (assert (string-contains? line ":tokens 4250") "Formatted line must contain tokens consumed")
    (assert (is-some? parsed-opt) "Telemetry line must parse successfully into record")
    (assert (and (= (.-timestamp-ms parsed) 1725793200123)
                 (and (= (.-phase-id parsed) "phase-319")
                      (and (= (.-tokens-consumed parsed) 4250)
                           (and (= (.-duration-ms parsed) 1820)
                                (and (= (.-rss-mb parsed) 64)
                                     (= (.-dirty-buffer-count parsed) 2))))))
            "Parsed telemetry record must match original field values exactly")
    true))

(df test-telemetry-streaming-persistence [] -> Bool
  :d "Verifies in-flight telemetry streaming accumulation, sequential ND-ASN appending, and ledger updates."
  (let [(r1 (t/TelemetryRecord
              :timestamp-ms 1725793200000
              :phase-id "phase-319"
              :tokens-consumed 1200
              :duration-ms 450
              :rss-mb 48
              :dirty-buffer-count 0))
        (r2 (t/TelemetryRecord
              :timestamp-ms 1725793205000
              :phase-id "phase-319"
              :tokens-consumed 2800
              :duration-ms 890
              :rss-mb 56
              :dirty-buffer-count 1))
        (log0 "")
        (log1 (t/stream-step-telemetry r1 log0))
        (log2 (t/stream-step-telemetry r2 log1))
        (entry1-parsed (t/parse-telemetry-entry log1))
        (split-entries (string-split log2 "\n"))
        (entry2-parsed (t/parse-telemetry-entry (option-or (list-get split-entries 1) "")))]
    (assert (string-starts-with? log1 "(:telem :ts 1725793200000") "First streamed entry must start with timestamp receipt")
    (assert (string-contains? log1 ":dirty 0") "First stream step must record zero dirty buffers")
    (assert (= (list-length split-entries) 2) "Updated telemetry stream log must accumulate exactly two entries")
    (assert (string-contains? log2 ":tokens 2800") "Second streamed entry in accumulated ledger must record updated token count")
    (assert (and (is-some? entry1-parsed) (is-some? entry2-parsed)) "Both entries in accumulated stream log must parse into valid TelemetryRecords")
    true))

(df test-vfs-checkpoint-creation-and-asn-serialization [] -> Bool
  :d "Verifies atomic snapshot creation, CAS root digest calculation, dirty buffer counting, and ASN serialization round-trip."
  (let [(reg0 (v/vfs-init))
        (reg1 (v/vfs-write reg0 "src/clean.asl" "(module clean :x [])"))
        (reg2 (v/vfs-write reg1 "src/dirty1.asl" "(module dirty1 :v 1)"))
        (reg3 (v/vfs-write reg2 "src/dirty2.asl" "(module dirty2 :v 1)"))
        (reg4 (v/vfs-write reg3 "src/dirty1.asl" "(module dirty1 :v 2)"))
        (reg5 (v/vfs-write reg4 "src/dirty2.asl" "(module dirty2 :v 2)"))
        (ckpt (c/create-vfs-checkpoint reg5 "phase-319"))
        (asn-doc (c/format-checkpoint-asn ckpt))
        (parsed-opt (c/parse-checkpoint-asn asn-doc))
        (parsed (option-or parsed-opt (c/VFSCheckpoint :id "" :phase-id "" :created-at-ms 0 :buffers (list) :cas-root "" :dirty-count 0)))]
    (assert (string-starts-with? (.-id ckpt) "cp-phase-319-") "Checkpoint ID must start with cp-phase-319-")
    (assert (= (string-length (.-cas-root ckpt)) 64) "Checkpoint cas-root must be a 64-character hex digest")
    (assert (= (.-dirty-count ckpt) 2) "Checkpoint must count exactly 2 dirty buffers")
    (assert (= (list-length (.-buffers ckpt)) 3) "Checkpoint must capture all 3 staged buffers")
    (assert (and (= (.-id parsed) (.-id ckpt))
                 (and (= (.-phase-id parsed) (.-phase-id ckpt))
                      (and (= (.-cas-root parsed) (.-cas-root ckpt))
                           (and (= (.-dirty-count parsed) (.-dirty-count ckpt))
                                (= (list-length (.-buffers parsed)) (list-length (.-buffers ckpt)))))))
            "Parsed checkpoint metadata and buffer count must match serialized checkpoint exactly")
    true))

(df test-checkpoint-archive-persistence-and-lookup [] -> Bool
  :d "Verifies checkpoint archive document structure, restoration lookup, and restored registry topology."
  (let [(reg0 (v/vfs-init))
        (reg1 (v/vfs-write reg0 "pkg/a.asl" "alpha text"))
        (reg2 (v/vfs-write reg1 "pkg/b.asl" "beta base text"))
        (reg3 (v/vfs-write reg2 "pkg/c.asl" "gamma base text"))
        (reg4 (v/vfs-write reg3 "pkg/b.asl" "beta modified text"))
        (reg5 (v/vfs-write reg4 "pkg/c.asl" "gamma modified text"))
        (ckpt (c/create-vfs-checkpoint reg5 "phase-319"))
        (archive-doc (c/format-checkpoint-asn ckpt))
        (restored-res (c/restore-vfs-checkpoint archive-doc (.-id ckpt)))
        (restored-reg (result-or restored-res (v/vfs-init)))
        (lookup-a (v/vfs-read restored-reg "pkg/a.asl"))
        (lookup-b (v/vfs-read restored-reg "pkg/b.asl"))]
    (assert (string-contains? archive-doc "(:checkpoint") "Serialized checkpoint archive must contain :checkpoint root form")
    (assert (string-contains? archive-doc (.-cas-root ckpt)) "Serialized archive must embed exact computed cas-root hash")
    (assert (is-ok? restored-res) "Restoring from serialized checkpoint archive must return ok result")
    (assert (= (.-size restored-reg) 3) "Restored registry size must equal 3")
    (assert (and (is-some? lookup-a) (is-some? lookup-b)) "Restored registry must allow successful lookup of original buffer paths")
    true))

(df test-dirty-buffer-restoration-fidelity [] -> Bool
  :d "Verifies zero-data-loss dirty buffer fidelity, content preservation, and identical unified diff deltas."
  (let [(reg0 (v/vfs-init))
        (clean-text "line 1\nline 2\nline 3")
        (base-text "fn start [] -> Unit\n  println \"hello\"\n  ret")
        (dirty-text "fn start [] -> Unit\n  println \"hello world\"\n  eprintln \"log\"\n  ret")
        (reg1 (v/vfs-write reg0 "src/clean.asl" clean-text))
        (reg2 (v/vfs-write reg1 "src/dirty.asl" base-text))
        (reg3 (v/vfs-write reg2 "src/dirty.asl" dirty-text))
        (orig-dirty-buf (option-or (v/vfs-read reg3 "src/dirty.asl") (v/VFSBuffer :path "" :content "" :base-content "" :cas-hash "" :base-hash "" :revision 0 :dirty false :loaded-at 0)))
        (orig-diff (v/vfs-diff orig-dirty-buf))
        (ckpt (c/create-vfs-checkpoint reg3 "phase-319"))
        (archive-text (c/format-checkpoint-asn ckpt))
        (restored-reg (result-or (c/restore-vfs-checkpoint archive-text (.-id ckpt)) (v/vfs-init)))
        (clean-buf (option-or (v/vfs-read restored-reg "src/clean.asl") (v/VFSBuffer :path "" :content "" :base-content "" :cas-hash "" :base-hash "" :revision 0 :dirty true :loaded-at 0)))
        (dirty-buf (option-or (v/vfs-read restored-reg "src/dirty.asl") (v/VFSBuffer :path "" :content "" :base-content "" :cas-hash "" :base-hash "" :revision 0 :dirty false :loaded-at 0)))
        (restored-diff (v/vfs-diff dirty-buf))]
    (assert (and (not (.-dirty clean-buf)) (= (.-content clean-buf) (.-base-content clean-buf)))
            "Restored clean buffer must retain dirty: false and identical content and base-content")
    (assert (and (.-dirty dirty-buf) (!= (.-content dirty-buf) (.-base-content dirty-buf)))
            "Restored dirty buffer must retain dirty: true and distinct uncommitted content")
    (assert (and (= (.-cas-hash dirty-buf) (.-cas-hash orig-dirty-buf))
                 (= (.-base-hash dirty-buf) (.-base-hash orig-dirty-buf)))
            "Restored dirty buffer must preserve exact current and base CAS hash digests")
    (assert (= (.-revision dirty-buf) (.-revision orig-dirty-buf))
            "Restored dirty buffer must preserve monotonic revision counter")
    (assert (and (> (list-length restored-diff) 0)
                 (= (string-join restored-diff "\n") (string-join orig-diff "\n")))
            "Unified diff on restored dirty buffer must match pre-checkpoint diff deltas with exact fidelity")
    true))

(df run-tests [] -> Bool
  :d "Executes comprehensive VFS checkpointing and telemetry test suite with 25 strict assertions."
  (and (test-telemetry-record-and-format)
       (and (test-telemetry-streaming-persistence)
            (and (test-vfs-checkpoint-creation-and-asn-serialization)
                 (and (test-checkpoint-archive-persistence-and-lookup)
                      (test-dirty-buffer-restoration-fidelity))))))
