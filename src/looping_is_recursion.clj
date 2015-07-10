(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (<= n 0)
                     acc
                     (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc a-seq]
                 (if (empty? a-seq)
                     acc
                     (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
    (let [helper (fn [seq-1 seq-2]
                 (cond
                  (and (empty? seq-1) (empty? seq-2)) true
                  (or (empty? seq-1) (empty? seq-2)) false
                  (== (first seq-1) (first seq-2)) (recur (rest seq-1) (rest seq-2))
                  :else false))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         b-seq a-seq]
    (cond (empty? b-seq) nil
          (pred (first b-seq)) index
          :else (recur (inc index) (rest b-seq)))))

(defn avg [a-seq]
  (loop [b-seq a-seq
         acc 0]
    (if (empty? b-seq)
        (/ acc (count a-seq))
        (recur (rest b-seq) (+ acc (first b-seq))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{}
         b-seq a-seq]
    (if (empty? b-seq)
        acc
        (recur (toggle acc (first b-seq)) (rest b-seq)))))


(defn fast-fibo [n]
  (loop [f-n+1 1
         f-n 0
         index n]
    (if (<= index 0)
        f-n
        (recur (+ f-n f-n+1) f-n+1 (dec index)))))

(defn cut-at-repetition [a-seq]
  (loop [acc []
         index 1
         b-seq a-seq]
    (cond (empty? b-seq) acc
          (== (count (conj (set acc) (first b-seq))) index) (recur (conj acc (first b-seq)) (inc index) (rest b-seq))
          :else acc)))

