(ns user
  (:require [clojure.edn :as edn]))

(def file "input.txt")

(defn get-intcodes [file]
  (->> file
       slurp
       (re-seq #"\d+")
       (map edn/read-string)
       (vec)))

(defn int-op [f int-codes pos]
  (let [x1 (get int-codes (get int-codes (inc pos)))
        x2 (get int-codes (get int-codes (+ 2 pos)))
        y (get int-codes (+ 3 pos))]
    (assoc int-codes y (f x1 x2))))

(def add
  (partial int-op +))

(def multiply
  (partial int-op *))

(add (get-intcodes file) 1)

(multiply (get-intcodes file) 0)

(defn run-intcodes [starting-int-codes]
  (loop [int-codes starting-int-codes
         pos 0]
    (let [next-pos (+ pos 4)
          x (get int-codes pos)]
      (condp = x
        1 (recur (add int-codes pos) next-pos)
        2 (recur (multiply int-codes pos) next-pos)
        99 int-codes
        :else (str "unknown code " x)))))

(defn run-intcodes-with-adjustment [x1 x2]
  (let [int-codes (get-intcodes file)]
    (-> int-codes
        (assoc 1 x1)
        (assoc 2 x2)
        (run-intcodes)
        (get 0))))

(defn p1 []
  (run-intcodes-with-adjustment 12 2))

(defn search-for-intcode-params [target-result]
  (loop [x 0
         y 0]
    (let [res (run-intcodes-with-adjustment x y)]
      (if (= res target-result)
        (+ (* 100 x) y)
        (let [new-y (if (= 99 x) (inc y) y)
              new-x (if (= 99 x) 0 (inc x))]
          (recur new-x new-y))))))

(defn p2 []
  (search-for-intcode-params 19690720))
