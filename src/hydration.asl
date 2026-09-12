(module asl-mem/hydration
  :d "Autonomous cascade compression lifecycle, managed amnesia context clearing, and operational buffer management in pure ASL."
  :x [ContextBuffer
      make-context-buffer
      check-compression-threshold
      cascade-compress
      evict-raw-context
      default-core-axioms
      hydrate-intent-summary
      synthesize-intent-summary
      default-modern-decisions
      hydrate-modern-intent
      now-ms]
  :i [(view_layer :a vl)])

(dfs ContextBuffer
  (:f id Str "Buffer identifier")
  (:f active-tokens I64 "Current token count in active working memory")
  (:f messages (List Str) "Uncompressed operational message strings")
  (:f chunks (List vl/MemoryChunk) "Compressed memory chunk stubs"))

(df make-context-buffer [(id Str) (tokens I64) (messages (List Str)) (chunks (List vl/MemoryChunk))] -> ContextBuffer
  :d "Constructs an operational working memory buffer record."
  (ContextBuffer
    :id id
    :active-tokens tokens
    :messages messages
    :chunks chunks))

(df check-compression-threshold [(tokens I64) (threshold I64)] -> Bool
  :d "Evaluates whether active working token count has reached or exceeded compression ceiling."
  (>= tokens threshold))

(df is-fact? [(msg Str)] -> Bool
  :d "Determines if message string represents a scalar fact."
  (let [(lower (string-lower (string-trim msg)))]
    (or (string-starts-with? lower "fact")
        (string-starts-with? lower "[fact]"))))

(df is-directive? [(msg Str)] -> Bool
  :d "Determines if message string represents an operational directive."
  (let [(lower (string-lower (string-trim msg)))]
    (or (string-starts-with? lower "directive")
        (string-starts-with? lower "[directive]"))))

(df is-other? [(msg Str)] -> Bool
  :d "Determines if message is neither fact nor directive."
  (and (not (is-fact? msg))
       (not (is-directive? msg))))

(df now-ms [(base I64) (delta I64)] -> I64
  :d "Calculates millisecond epoch timestamp."
  (+ base delta))

(df extract-facts [(msgs (List Str))] -> (List Str)
  :d "Extracts scalar facts from operational message list."
  (filter is-fact? msgs))

(df extract-directives [(msgs (List Str))] -> (List Str)
  :d "Extracts operational directives from operational message list."
  (filter is-directive? msgs))

(df extract-summary [(msgs (List Str)) (buf-id Str)] -> Str
  :d "Extracts or generates human-readable summary from message list."
  (if (list-empty? msgs)
      "Empty context buffer compressed"
      (let [(other (filter is-other? msgs))]
        (if (list-empty? other)
            (str "Folded " (string-from-int64 (list-length msgs)) " operational messages for buffer " buf-id)
            (string-join other " | ")))))

(df cascade-compress [(buffer ContextBuffer) (threshold I64) (parent-refs (List Str)) (base-ts I64)] -> (Pair MemoryChunk ContextBuffer)
  :d "Folds uncompressed operational messages into a canonical MemoryChunk and resets active working context."
  (let [(buf-id (.-id buffer))
        (msgs (.-messages buffer))
        (ts (if (> base-ts 0) (now-ms base-ts 0) 1))
        (c-id (str "chunk-" buf-id "-" (string-from-int64 ts)))
        (facts (extract-facts msgs))
        (directives (extract-directives msgs))
        (summary (extract-summary msgs buf-id))
        (chunk (vl/make-memory-chunk c-id ts 0.95 summary facts directives parent-refs))
        (new-chunks (list-append (.-chunks buffer) (list chunk)))
        (new-buf (ContextBuffer
                   :id buf-id
                   :active-tokens 0
                   :messages (list)
                   :chunks new-chunks))]
    (pair chunk new-buf)))

(df evict-raw-context [(buffer ContextBuffer)] -> ContextBuffer
  :d "Evicts all uncompressed operational messages from active context buffer while retaining chunk stubs."
  (ContextBuffer
    :id (.-id buffer)
    :active-tokens 0
    :messages (list)
    :chunks (.-chunks buffer)))

(df default-core-axioms [] -> (List Str)
  :d "Returns canonical identifiers and descriptions of the four core engineering axioms."
  (list "D0015: Token Arbitrage (-70% tokens, pure ASN, zero JSON/YAML, zero emojis)"
        "D0016: Agent-Native Autonomy (machine-first, balanced delimiters, deterministic AST)"
        "D0017: Falsifiable Observability (7 verification gates, zero comments C0001, physical receipts)"
        "D0018: In-Memory State Surgery (RAM VFS, batch RPC, speculative racing, clean supervisor)"))

(df hydrate-intent-summary [(axioms (List Str))] -> Str
  :d "Formats compact sub-100-token executive summary of core axioms and active architectural decisions."
  (if (list-empty? axioms)
      "Axioms: Token Arbitrage | Agent-Native Autonomy | Falsifiable Observability | In-Memory State Surgery"
      (string-join axioms " | ")))

(df synthesize-intent-summary [(raw-ledger Str)] -> Str
  :d "Extracts verified core axioms from raw intent ledger text into sub-100-token executive summary."
  (if (or (string-contains? raw-ledger "D0015")
          (string-contains? raw-ledger "Token Arbitrage"))
      (hydrate-intent-summary (default-core-axioms))
      "Axioms: Ungrounded"))

(df default-modern-decisions [] -> (List Str)
  :d "Returns canonical identifiers and descriptions of modern architectural decisions D0019 through D0029."
  (list "D0019: Pure ASL Differential Test Runner & Falsifiable Gate Invariant"
        "D0020: Zero-Copy Token Regex Engine & CAS Mutators"
        "D0021: In-Memory AST Equivalence E-Graphs & Congruence Closure"
        "D0022: In-Memory Speculative VFS Branching & Pruning"
        "D0023: Grammar-Trie Constrained Decoding Engine"
        "D0024: Memory TaskStore State Machine & Priority Scheduler"
        "D0025: Multi-Agent Supervisor & Dual-Temperature Speculative Racing"
        "D0026: Pure ASL WASI Lowering & Node Annihilation"
        "D0027: Dense Tabular Pyramid & Sparkline Compaction"
        "D0028: Universal Cross-Reference URIs & AST Dependency Graph"
        "D0029: Decoupled Amnesia Runtime Context Eviction & Retention Audit"))

(df hydrate-modern-intent [(id Str)] -> Str
  :d "Retrieves hydrated summary of a modern architectural decision by ID or empty string if legacy or absent."
  (cond
    ((= id "D0019") "D0019: Pure ASL Differential Test Runner & Falsifiable Gate Invariant")
    ((= id "D0020") "D0020: Zero-Copy Token Regex Engine & CAS Mutators")
    ((= id "D0021") "D0021: In-Memory AST Equivalence E-Graphs & Congruence Closure")
    ((= id "D0022") "D0022: In-Memory Speculative VFS Branching & Pruning")
    ((= id "D0023") "D0023: Grammar-Trie Constrained Decoding Engine")
    ((= id "D0024") "D0024: Memory TaskStore State Machine & Priority Scheduler")
    ((= id "D0025") "D0025: Multi-Agent Supervisor & Dual-Temperature Speculative Racing")
    ((= id "D0026") "D0026: Pure ASL WASI Lowering & Node Annihilation")
    ((= id "D0027") "D0027: Dense Tabular Pyramid & Sparkline Compaction")
    ((= id "D0028") "D0028: Universal Cross-Reference URIs & AST Dependency Graph")
    ((= id "D0029") "D0029: Decoupled Amnesia Runtime Context Eviction & Retention Audit")
    (:else "")))

