(ns user
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(defn get-connections [file]
  (->> file
       slurp
       str/split-lines
       (map #(str/split % #"\)"))))

(defn get-orbits [connections]
  (group-by first connections))

(defn get-orbit-count
  ([orbits] (get-orbit-count orbits "COM" 0))
  ([orbits object depth]
   (let [satellites (map #(second %) (get orbits object))]
     (reduce + depth (map #(get-orbit-count orbits % (inc depth)) satellites)))))

; testing

(def demo "./day06/demo.txt")
(def connections (get-connections demo))

(def orbits (get-orbits connections))

(map #(second %) (get orbits "B"))

(get-orbit-count orbits)

