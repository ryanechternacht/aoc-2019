(ns user
  (:require [clojure.edn :as edn]))

(def file "input.txt")

(defn get-fuels [file]
  (->> file
       slurp
       (re-seq #"\d+")
       (map edn/read-string)))

(defn get-fuel-cost [x]
  (-> x
      (/ 3)
      (int)
      (- 2)))

(defn get-total-cost [cost-func fuels]
  (->> fuels
       (map cost-func)
       (reduce +)))

(defn p1-answer []
  (->> file
       get-fuels
       (get-total-cost get-fuel-cost)))

(defn get-advanced-fuel-cost [x]
  (loop [running-cost (get-fuel-cost x)
         additional-cost (get-fuel-cost running-cost)]
    (if (> additional-cost 0)
      (recur (+ running-cost additional-cost) (get-fuel-cost additional-cost))
      running-cost)))

(defn p2-answer []
  (->> file
       get-fuels
       (get-total-cost get-advanced-fuel-cost)))

