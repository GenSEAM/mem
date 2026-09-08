(module asl-mem/pointer
  :d "Perceptual Pointers and Blob Offloading: pure ASL zero-copy multimodal/DOM context management."
  :x [BlobPointer
      PerceptualFact
      make-blob-pointer
      format-pointer-token
      extract-scalar-fact
      format-perceptual-fact
      format-pointer-descriptor
      should-offload?]
  :i [])

(dfs BlobPointer
  (:f id Str "Unique content-addressed identifier for the offloaded blob")
  (:f kind Str "MIME or perceptual category (e.g. dom, audio, image, pdf)")
  (:f bytes I64 "Payload size in bytes")
  (:f tokens-saved I64 "Estimated token count saved by offloading")
  (:f summary Str "Lightweight semantic summary for coordinator attention"))

(dfs PerceptualFact
  (:f key Str "Target scalar attribute key")
  (:f val Str "Extracted scalar attribute value")
  (:f confidence F64 "Grounded extraction confidence score")
  (:f source-id Str "Grounded pointer identifier"))

(df make-blob-pointer [(id Str) (kind Str) (raw-content Str) (summary Str)] -> BlobPointer
  :d "Constructs a content-addressed BlobPointer estimating token savings and byte footprint."
  (let [(len (string-length raw-content))]
    (BlobPointer
      :id id
      :kind kind
      :bytes len
      :tokens-saved (/ len 4)
      :summary summary)))

(df format-pointer-token [(ptr BlobPointer)] -> Str
  :d "Formats a BlobPointer into a canonical S-expression pointer token."
  (str "(:ptr :id \"" (.-id ptr)
       "\" :kind \"" (.-kind ptr)
       "\" :tokens-saved " (string-from-int64 (.-tokens-saved ptr))
       " :summary \"" (.-summary ptr) "\")"))

(df extract-scalar-fact [(ptr BlobPointer) (key Str) (val Str)] -> PerceptualFact
  :d "Simulates a sterile perception subagent dereferencing a pointer to extract a grounded scalar fact."
  (PerceptualFact
    :key key
    :val val
    :confidence 1.0
    :source-id (.-id ptr)))

(df format-perceptual-fact [(fact PerceptualFact)] -> Str
  :d "Formats a PerceptualFact into a canonical S-expression representation."
  (str "(:fact :key \"" (.-key fact)
       "\" :val \"" (.-val fact)
       "\" :confidence " (string-from-float64 (.-confidence fact))
       " :source-id \"" (.-source-id fact) "\")"))

(df format-pointer-descriptor [(id Str) (action Str) (bytes I64) (tokens-saved I64) (summary Str)] -> Str
  :d "Formats an offload or dereference descriptor for prompt context management."
  (str "(:ptr :id \"" id
       "\" :action \"" action
       "\" :bytes " (string-from-int64 bytes)
       " :tokens-saved " (string-from-int64 tokens-saved)
       " :summary \"" summary "\")"))

(df should-offload? [(bytes I64)] -> Bool
  :d "Determines if a payload exceeds the 500 byte offload threshold."
  (> bytes 500))

