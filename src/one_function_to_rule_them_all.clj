(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (cond
    (empty? a-seq) ""
    :else (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (rest
    (reduce (fn [i j] (conj i x j)) [] a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [count item]
                    (inc count))]
     (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn [reversed item]
                  (cons item reversed))]
    (reduce reverser '() a-seq)))

(defn min-max-element [a-seq]
  (let [min-max-evaluator (fn [[min max] item]
                           (cond
                             (< item min) [item max]
                             (> item max) [min item]
                             :else [min max]))]
    (reduce min-max-evaluator [(first a-seq) (first a-seq )] a-seq)))

(defn insert [sorted-seq n]
  (let [evaluator (fn [a-seq item]
                     (cond
                       (> item (first a-seq)) (cons)
                       (< item (first a-seq)) (cons item a-seq)
                       :else (recur (rest a-seq) item)))]
    (evaluator sorted-seq n)))

(defn insert [sorted-seq n]
  (cond
      (empty? sorted-seq) (cons n '())
      (< n (first sorted-seq)) (cons n sorted-seq)
      :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert () a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))


(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x & more]
   (let [counter (fn [count item]
                    (inc count))]
    (reduce counter 1 more))))



(defn my-*
  ([] 1)
  ([x] x)
  ([x & more]
   (let [multiply (fn [x y]
                    (* x y))]
    (reduce multiply x more))))

(defn test []
  (fn [x] (and (pos? x) (odd? x))))


(defn pred-and
  ([] (fn [x] true))
  ([pred] pred)
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more]
   (reduce pred-and (pred-and pred1 pred2) more)))


(defn my-map [f a-seq]
  [:-])
fn [x] :-
