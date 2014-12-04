(ns looping-is-recursion)

(defn power [base exp]
  (loop [n   base
         k   exp
         res 1]
    (if (>= 0 k)
      res
      (recur n (dec k) (* n res)))))

(defn last-element [a-seq]
  (let [tail (rest a-seq)]
    (if (empty? tail)
      (first a-seq)
      (recur tail))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2))   true
   (not (= (count seq1) (count seq2))) false
   (not (= (first seq1) (first seq2))) false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         p   pred
         xs  a-seq]
    (cond
     (empty? xs) nil
     (p (first xs)) idx
     :else (recur (inc idx) p (rest xs)))))

(defn avg [a-seq]
  (loop [sum 0
         len 0
         xs  a-seq]
    (if (empty? xs)
      (/ sum len)
      (recur (+ sum (first xs)) (inc len) (rest xs)))))

(defn- toggle [a-set elem]
  (let [f (if (contains? a-set elem) disj conj)]
    (f a-set elem)))

(defn parity [a-seq]
  (loop [res #{}
         xs a-seq]
    (if (empty? xs)
      res
      (recur (toggle res (first xs)) (rest xs)))))

(defn fast-fibo [n]
  (loop [counter n
         fst     0
         snd     1]
    (if (<= counter 0)
      fst
      (recur (dec counter) snd (+ fst snd)))))

(defn cut-at-repetition [a-seq]
  (loop [s  []
         xs a-seq]
    (if (or (empty? xs) (contains? (set s) (first xs)))
      (sequence s)
      (recur (conj s (first xs)) (rest xs)))))

