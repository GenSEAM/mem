(module asl-mem/ring
  :d "Universal high-throughput Ring Buffer with configurable overflow policies: overwrite oldest, spill to segment, middle-evict, and backpressure telemetry."
  :x [OverflowPolicy
      RingBuffer
      RingPushResult
      make-ring-buffer
      ring-push
      ring-peek-recent
      ring-peek-oldest
      ring-drain-all
      ring-is-full?
      ring-utilization
      ring-evicted-count])

(dfe OverflowPolicy
  (:c policy-overwrite-oldest [] "Overwrites oldest entry when capacity is exceeded (circular FIFO)")
  (:c policy-spill [] "Spills oldest batch to persistent storage callback or segment")
  (:c policy-middle-evict [] "Retains head and tail windows, evicting the middle")
  (:c policy-reject [] "Rejects new entries when buffer is at 100% capacity"))

(dfs RingBuffer
  (:f capacity I64 "Maximum number of slots")
  (:f items (List Str) "Stored entries in buffer")
  (:f total-pushed I64 "Monotonically increasing counter of all items pushed")
  (:f evicted-count I64 "Count of entries evicted or overwritten")
  (:f policy OverflowPolicy "Active overflow handling policy"))

(dfs RingPushResult
  (:f buffer RingBuffer "Updated ring buffer state")
  (:f was-evicted Bool "True if an entry was overwritten or evicted")
  (:f evicted-item (Option Str) "The evicted entry if policy triggered an eviction")
  (:f rejected Bool "True if item was rejected by backpressure policy"))

(df make-ring-buffer [(cap I64) (pol OverflowPolicy)] -> RingBuffer
  :d "Constructs an empty RingBuffer with the given capacity and overflow policy."
  (RingBuffer
    :capacity (if (<= cap 0) 1 cap)
    :items (list)
    :total-pushed 0
    :evicted-count 0
    :policy pol))

(df ring-is-full? [(rb RingBuffer)] -> Bool
  :d "Checks if the ring buffer has reached its maximum slot capacity."
  (>= (list-length (.-items rb)) (.-capacity rb)))

(df ring-evicted-count [(rb RingBuffer)] -> I64
  :d "Returns total count of elements dropped or spilled due to overflow."
  (.-evicted-count rb))

(df ring-utilization [(rb RingBuffer)] -> I64
  :d "Returns buffer capacity utilization percentage (0 to 100)."
  (let [(cnt (list-length (.-items rb)))
        (cap (.-capacity rb))]
    (if (<= cap 0)
      0
      (/ (* cnt 100) cap))))

(df ring-peek-oldest [(rb RingBuffer)] -> (Option Str)
  :d "Returns the oldest entry in the ring buffer without removing it."
  (list-head (.-items rb)))

(df ring-peek-recent [(rb RingBuffer) (n I64)] -> (List Str)
  :d "Returns the latest N entries in chronological order."
  (let [(items (.-items rb))
        (len (list-length items))]
    (if (<= len n)
      items
      (let [(start (- len n))]
        (option-or (list-slice items start len) (list))))))

(df ring-drain-all [(rb RingBuffer)] -> (Pair RingBuffer (List Str))
  :d "Drains all buffered items, returning an empty buffer with preserved telemetry alongside drained items."
  (let [(drained (.-items rb))
        (cleared (RingBuffer
                   :capacity (.-capacity rb)
                   :items (list)
                   :total-pushed (.-total-pushed rb)
                   :evicted-count (.-evicted-count rb)
                   :policy (.-policy rb)))]
    (pair cleared drained)))

(df drop-oldest [(items (List Str))] -> (List Str)
  :d "Drops the head item from list."
  (mt (list-tail items)
    ((some tl) tl)
    ((none) (list))))

(df ring-push [(rb RingBuffer) (item Str)] -> RingPushResult
  :d "Pushes an entry into the RingBuffer respecting the configured overflow policy."
  (let [(cap (.-capacity rb))
        (curr-items (.-items rb))
        (curr-len (list-length curr-items))
        (new-pushed (+ (.-total-pushed rb) 1))]
    (if (< curr-len cap)
      (let [(next-buf (RingBuffer
                        :capacity cap
                        :items (list-append curr-items (list item))
                        :total-pushed new-pushed
                        :evicted-count (.-evicted-count rb)
                        :policy (.-policy rb)))]
        (RingPushResult
          :buffer next-buf
          :was-evicted false
          :evicted-item (none)
          :rejected false))
      (mt (.-policy rb)
        ((policy-reject)
         (RingPushResult
           :buffer rb
           :was-evicted false
           :evicted-item (none)
           :rejected true))
        ((policy-overwrite-oldest)
         (let [(oldest (list-head curr-items))
               (retained (drop-oldest curr-items))
               (next-items (list-append retained (list item)))
               (next-buf (RingBuffer
                           :capacity cap
                           :items next-items
                           :total-pushed new-pushed
                           :evicted-count (+ (.-evicted-count rb) 1)
                           :policy (.-policy rb)))]
           (RingPushResult
             :buffer next-buf
             :was-evicted true
             :evicted-item oldest
             :rejected false)))
        ((policy-spill)
         (let [(spilled (list-head curr-items))
               (retained (drop-oldest curr-items))
               (next-items (list-append retained (list item)))
               (next-buf (RingBuffer
                           :capacity cap
                           :items next-items
                           :total-pushed new-pushed
                           :evicted-count (+ (.-evicted-count rb) 1)
                           :policy (.-policy rb)))]
           (RingPushResult
             :buffer next-buf
             :was-evicted true
             :evicted-item spilled
             :rejected false)))
        ((policy-middle-evict)
         (let [(head-size (/ cap 4))
               (head-part (if (> head-size 0) (option-or (list-slice curr-items 0 head-size) (list)) (list)))
               (tail-start (+ head-size 1))
               (tail-part (option-or (list-slice curr-items tail-start curr-len) (list)))
               (next-items (list-append (list-append head-part tail-part) (list item)))
               (next-buf (RingBuffer
                           :capacity cap
                           :items next-items
                           :total-pushed new-pushed
                           :evicted-count (+ (.-evicted-count rb) 1)
                           :policy (.-policy rb)))]
           (RingPushResult
             :buffer next-buf
             :was-evicted true
             :evicted-item (none)
             :rejected false)))))))
