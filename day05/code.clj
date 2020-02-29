(ns user
  (:require [clojure.edn :as edn]))

(defn get-int-codes [file]
  (->> file
       slurp
       (re-seq #"[\d-]+")
       (map edn/read-string)
       (vec)))

(defn get-arg [int-codes pos mode]
  (condp = mode
    0 (get int-codes (get int-codes pos))
    1 (get int-codes pos)
    :else "error"))

(defn three-arg-op [f int-codes args]
  (let [map (map (fn [a] (get-arg int-codes (:pos a) (:mode a))) args)
        params (take 2 map)
        loc (get-arg int-codes (:pos (nth args 2)) 1)]
    (assoc int-codes loc (apply f params))))

(def add
  (partial three-arg-op +))

(def multiply
  (partial three-arg-op *))

(def equals
  (partial three-arg-op #(if (= %1 %2) 1 0)))

(def less-than
  (partial three-arg-op #(if (< %1 %2) 1 0)))

; master 1 arg version?
(defn input [int-codes args]
  (let [loc (get-arg int-codes (:pos (first args)) 1)]
    (assoc int-codes loc (edn/read-string (read-line)))))

(defn output [int-codes args]
  (let [loc (get-arg int-codes (:pos (first args)) 1)]
    (prn (get int-codes loc))
    int-codes))

(defn build-op-args [int-codes pos arg-count]
  (let [op (get int-codes pos)]
    (->> arg-count
         (range 0)
         (map (fn [x] {:pos (inc (+ x pos)) :power (reduce * (repeat (+ 2 x) 10))}))
         (map (fn [m] (assoc m :mode (mod (int (/ op (:power m))) 10)))))))

(defn build-op [int-codes pos]
  (let [op (mod (get int-codes pos) 100)]
    (condp = op
      1 {:op add :next-pos (+ pos 4) :args (build-op-args int-codes pos 3)}
      2 {:op multiply :next-pos (+ pos 4) :args (build-op-args int-codes pos 3)}
      3 {:op input :next-pos (+ pos 2) :args (build-op-args int-codes pos 1)}
      4 {:op output :next-pos (+ pos 2) :args (build-op-args int-codes pos 1)}
      7 {:op less-than :next-pos (+ pos 4) :args (build-op-args int-codes pos 3)}
      8 {:op equals :next-pos (+ pos 4) :args (build-op-args int-codes pos 3)}
      99 {:halts true}
      :else {:halts true :error (str "unknown code " op)})))

(defn run-int-codes [starting-int-codes]
  (loop [int-codes starting-int-codes
         pos 0]
    (let [op (build-op int-codes pos)]
      (cond
        (:halts op) int-codes
        (:op op) (recur
                  (apply (:op op) [int-codes (:args op)])
                  (:next-pos op))))))

;; testing

(def input-file (get-int-codes "./input.txt"))

(run-int-codes input-file)

(def demo0 [3,0,4,0,99])
(input demo0 0)
(run-int-codes demo0)

(def demo1 [1002,4,3,4,33])
(run-int-codes demo1)

(def demo2 [1101,100,-1,4,0])
(run-int-codes demo2)

(def demo3 [3,9,8,9,10,9,4,9,99,-1,8])
(run-int-codes demo3)

(def demo4 [3,9,7,9,10,9,4,9,99,-1,8])
(run-int-codes demo4)

(def demo5 [3,3,1108,-1,8,3,4,3,99])
(run-int-codes demo5)

(def demo6 [3,3,1107,-1,8,3,4,3,99])
(run-int-codes demo6)

(def args (build-op-args demo0 0 1))

(output demo0 arg)

(get-arg demo0 0 1)

;; 4138658 (input-02.txt)
;; 
(def input-02 (get-int-codes "./input-02.txt"))

(run-int-codes input-02)