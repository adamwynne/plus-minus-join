(ns plus-minus-join.core
  (:require
    [clojure.string :as str]
    [clojure.math.numeric-tower :as math]))

(defn join [x y]
  (Long/parseLong (str x y)))

(defn do-op-vec [v op start]
  (vec (concat (subvec v 0 (max start 0))
               [(op (nth v start) (nth v (+ start 1)))]
               (subvec v (min (+ start 2) (count v))))))

(defn do-ops-subset [num-seq op-seq valid-ops-set]
  (loop [num-seq num-seq
         num-index 0
         op-index 0]
    (if (>= op-index (count op-seq))
      [num-seq (vec (remove valid-ops-set op-seq))]
      (let [op (nth op-seq op-index)]
        (recur (if (valid-ops-set op)
                 (do-op-vec num-seq op num-index)
                 num-seq)
               (if (valid-ops-set op)
                 num-index
                 (+ num-index 1))
               (+ op-index 1))))))

(defn do-ops [num-seq op-seq]
  (assert (sequential? num-seq)
          "expecting sequence as first param")
  (assert (sequential? op-seq)
          "expecting sequence as second param")
  (assert (> (count num-seq) 1)
          "nums-seq should be greater than 1")
  (assert (= (count num-seq) (+ 1 (count op-seq)))
          (format "ops-seq wrong length: %d, (should be %d)" (count op-seq) (- (count num-seq) 1)))
  (let [[num-seq op-seq] (do-ops-subset num-seq op-seq #{join})
        [result _] (do-ops-subset num-seq op-seq #{+ -})]
    (first result)))

(defn permute-ops [ops-seq length]
  (let [radix (count ops-seq)]
    (map (fn [base10]
           (let [radix-n (Integer/toString base10 radix)
                 padded-radix-n (format "%s%s" (str/join (repeat (- length (count radix-n)) "0")) radix-n)]
             (vec (map (fn [c]
                         (nth ops-seq (Integer/parseInt (str c))))
                       (seq padded-radix-n)))))
         (range 0 (math/expt radix length)))))

(defn plus-minus-join [num-count num-target]
  (let [permuted-ops (permute-ops [+ - join] (- num-count 1))
        num-seq (vec (range 1 (+ num-count 1)))
        pretty {+ " + " - " - " join ""}]
    (println "num seq:" num-seq)
    (let [correct-ops (map
                        (fn [op-seq]
                          (println (str/join (interleave (map str num-seq) (concat (map pretty op-seq) [""])))))
                        (filter (fn [op-seq]
                                  (= (do-ops num-seq op-seq) num-target))
                                permuted-ops))]
      (println (format "found %d solutions from %d permutations" (count correct-ops) (count permuted-ops))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
