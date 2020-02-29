(ns user
  (:require [clojure.edn :as edn]))

(defn get-int-codes [file]
  (->> file
       slurp
       (re-seq #"[\d-]+")
       (map edn/read-string)
       (vec)))

(defn get-args [int-codes pos mode]
  (condp = mode
    0 (get int-codes (get int-codes pos))
    1 (get int-codes pos)
    :else "error"))

(defn three-arg-op [f int-codes args]
  (let [map (map (fn [a] (get-args int-codes (:pos a) (:mode a))) args)
        params (take 2 map)
        loc (get-args int-codes (:pos (nth args 2)) 1)]
    (assoc int-codes loc (apply f params))))

(def add
  (partial three-arg-op +))

(def multiply
  (partial three-arg-op *))

; master 1 arg version?
(defn input [int-codes args]
  (let [loc (get-args int-codes (:pos (first args)) 1)]
    (assoc int-codes loc (edn/read-string (read-line)))))

(defn output [int-codes args]
  (let [loc (get-args int-codes (:pos (first args)) 1)]
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
      3 {:op input :next-pos (+ pos 2) :args (build-op-args int-codes pos 1)} ; TODO args
      4 {:op output :next-pos (+ pos 2) :args (build-op-args int-codes pos 1)} ; TODO args
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

(def arg (build-op-args demo0 0 1))

(output demo0 arg)

(get-args demo0 0 1)

;; 4138658 (input-02.txt)
;; 
(def input-02 (get-int-codes "./input-02.txt"))

(run-int-codes input-02)