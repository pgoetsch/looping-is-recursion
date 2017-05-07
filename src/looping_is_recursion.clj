(ns looping-is-recursion)

(defn power [base exp]
    (let [helper (fn [acc n]
                   (if (zero? n)
                     acc
                     (recur (* acc base) (dec n))))]
      (if (zero? exp)
        1
        (helper 1 exp))
      ))

(defn last-element [a-seq]
    (cond
        (empty? a-seq) nil
        (= 1 (count a-seq)) (first a-seq)
        :else (last-element (rest a-seq))
    ))

(defn seq= [seq1 seq2]
    (if (and (empty? seq1) (empty? seq2))
    true
    (cond
        (or (not= (count seq1) (count seq2)) (and (= 1 (count seq1)) (not= (first seq1) (first seq2))) (empty? seq1) (empty? seq2)) false ; if uneven sizes, return false ; if last elements and not equal, return false
        (and (= 1 (count seq1)) (= (first seq1) (first seq2))) true ; if last elements and equal, return true
        :else (seq= (rest seq1) (rest seq2)) ; more to go, recurse...
    )))

(defn find-first-index [pred a-seq]
    (if (empty? a-seq) nil
        (loop [index 0
               new-seq a-seq]
          (if (pred (first new-seq))
                index ; if pred = true, return current index
                (if (empty? (rest new-seq)) ; else-if set is empty
                    nil
                    (recur (inc index) (rest new-seq))))))) ; else recurse down

; https://math.stackexchange.com/questions/445076/continuously-averaging-a-stream-of-numbers
(defn avg [a-seq] ; this would have been alot easier simply using map
    (loop [running-avg (first a-seq)
           numbers-seen 1
           new-seq (rest a-seq)]
           (if (empty? new-seq)
           running-avg
           (recur (/ (+ (first new-seq) (* running-avg numbers-seen)) (inc numbers-seen)) (inc numbers-seen) (rest new-seq)))))

; borrowed from recursion excersizes
(defn my-frequencies-helper [freqs a-seq]
(if (empty? a-seq)
    freqs ; empty, done with recursion
    (let [first-item (first a-seq) ; not empty, still more recursion to do
          new-freqs freqs]
        (if (contains? freqs first-item) ; check if passed in map contains next element in seq
            (my-frequencies-helper (assoc new-freqs first-item (inc (get freqs first-item))) (rest a-seq)); contains already, so increment its value
            (my-frequencies-helper (assoc new-freqs first-item 1) (rest a-seq)); does not contain, add new
        )
    ))
)

; borrowed from recursion excersizes
(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn parity [a-seq]
  (let [freqs (my-frequencies a-seq)  ; use my-frequencies to get frequencies for each element
        filtered-freqs (filter (fn [seq-item] (let [[_ item-freq] seq-item](odd? item-freq))) freqs)] ; filter out even freqs
        (keys filtered-freqs))) ; grab keys since filtered-freqs returns a map

; 0 1 1 2 3 5 8 ...
(defn fast-fibo [fib-nums-to-find]
    (if (= 0 fib-nums-to-find)
        0
        (loop [n 1
               n-1 0
               fib-nums-found 1]
               (if (= fib-nums-found fib-nums-to-find)
                    n
                    (recur (+ n n-1) n (inc fib-nums-found))))))

(defn cut-at-repetition [a-seq]
  (if (empty? a-seq)
    nil
    (loop [seen-set #{}
           new-set []
           current-index 0]
           (let [next-item (get a-seq current-index)]
           (if (or (contains? seen-set next-item) (= (count seen-set) (count a-seq)))
           new-set ; found a dupe or reached end, return what we have
           (recur (conj seen-set next-item) (conj new-set next-item) (inc current-index))))))) ; not a dupe, so add to new-set and seen-set
