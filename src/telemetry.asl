(module asl-mem/telemetry
  :d "In-flight step telemetry persistence and ND-ASN streaming engine"
  :x [TelemetryRecord
      format-telemetry-entry
      parse-telemetry-entry
      stream-step-telemetry]
  :i [])

(dfs TelemetryRecord
  (:f timestamp-ms I64 "Epoch timestamp in milliseconds when step executed")
  (:f phase-id Str "Active phase identifier e.g. phase-319")
  (:f tokens-consumed I64 "Total tokens consumed across harness step execution")
  (:f duration-ms I64 "Wall-clock execution duration in milliseconds")
  (:f rss-mb I64 "Resident set size memory footprint in megabytes")
  (:f dirty-buffer-count I64 "Count of open VFS buffers carrying uncommitted dirty edits"))

(df format-telemetry-entry [(entry TelemetryRecord)] -> Str
  :d "Serializes a TelemetryRecord into a single ND-ASN line."
  (let [(ts (string-from-int64 (.-timestamp-ms entry)))
        (ph (.-phase-id entry))
        (tok (string-from-int64 (.-tokens-consumed entry)))
        (dur (string-from-int64 (.-duration-ms entry)))
        (rss (string-from-int64 (.-rss-mb entry)))
        (dirty (string-from-int64 (.-dirty-buffer-count entry)))
        (p1 (str "(:telem :ts " ts " :phase \"" ph "\" :tokens " tok))
        (p2 (str " :duration " dur " :rss " rss " :dirty " dirty ")"))]
    (str p1 p2)))

(df extract-between [(text Str) (prefix Str) (suffix Str)] -> (Option Str)
  :d "Extracts substring between prefix and first subsequent suffix."
  (mt (string-index-of text prefix)
    ((none) (none))
    ((some p-idx)
     (let [(start (+ p-idx (string-length prefix)))
           (sub (option-or (string-slice text start (string-length text)) ""))]
       (mt (string-index-of sub suffix)
         ((none) (none))
         ((some s-idx)
          (string-slice sub 0 s-idx)))))))

(df parse-telemetry-entry [(line Str)] -> (Option TelemetryRecord)
  :d "Parses a single ND-ASN line into a TelemetryRecord."
  (let [(trimmed (string-trim line))]
    (if (and (string-starts-with? trimmed "(:telem")
             (string-ends-with? trimmed ")"))
      (let [(ts-str (extract-between trimmed ":ts " " "))
            (phase-str (extract-between trimmed ":phase \"" "\""))
            (tok-str (extract-between trimmed ":tokens " " "))
            (dur-str (extract-between trimmed ":duration " " "))
            (rss-str (extract-between trimmed ":rss " " "))
            (dirty-str (extract-between trimmed ":dirty " ")"))]
        (mt phase-str
          ((none) (none))
          ((some ph)
           (some (TelemetryRecord
                   :timestamp-ms (option-or (string-to-int64 (string-trim (option-or ts-str "0"))) 0)
                   :phase-id ph
                   :tokens-consumed (option-or (string-to-int64 (string-trim (option-or tok-str "0"))) 0)
                   :duration-ms (option-or (string-to-int64 (string-trim (option-or dur-str "0"))) 0)
                   :rss-mb (option-or (string-to-int64 (string-trim (option-or rss-str "0"))) 0)
                   :dirty-buffer-count (option-or (string-to-int64 (string-trim (option-or dirty-str "0"))) 0))))))
      (none))))

(df stream-step-telemetry [(entry TelemetryRecord) (stream-log Str)] -> Str
  :d "df stream-step-telemetry formats the entry and appends it to the in-flight telemetry stream, returning the updated stream log."
  (let [(formatted (format-telemetry-entry entry))]
    (if (string-empty? (string-trim stream-log))
      formatted
      (str stream-log "\n" formatted))))
