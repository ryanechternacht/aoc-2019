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

(defn p1 []
  (->> "./day06/input.txt"
       get-connections
       get-orbits
       get-orbit-count))

(defn get-orbitors [connections]
  (group-by second connections))

(defn path-to-root [orbitors object]
  (loop [node object
         path [object]]
    (if (= "COM" node)
      path
      (let [parent (get-in orbitors [node 0 0])]
        (recur parent (conj path parent))))))

(defn transfer-length [orbitors node1 node2]
  (let [node1-path (path-to-root orbitors node1)
        node2-path (path-to-root orbitors node2)
        intersections (map (fn [%] {:index (.indexOf node1-path %)
                                    :node %}) node2-path)
        least-ancestor (:node (first (filter #(< 0 (:index %)) intersections)))]
    (+ (dec (.indexOf node1-path least-ancestor))
       (dec (.indexOf node2-path least-ancestor)))))

(defn p2 []
  (->> "./day06/input.txt"
       get-connections
       get-orbitors
       (#(transfer-length % "SAN" "YOU"))))

; testing
(p1)

(p2)

(transfer-length orbitors2 "YOU" "SAN")

(def demo "./day06/demo.txt")
(def connections (get-connections demo))
(def orbits (get-orbits connections))

(def demo2 "./day06/demo2.txt")
(def connections2 (get-connections demo2))
(def orbits2 (get-orbits connections2))

(def orbitors2 (get-orbitors connections2))

(def you (path-to-root orbitors2 "YOU"))
(def san (path-to-root orbitors2 "SAN"))

(def intersections (map (fn [%] {:index (.indexOf san %) :node %}) you))

(def filtered (filter #(< 0 (:index %)) intersections))

(def low-anc (:node (first filtered)))

(def you-len (dec (.indexOf you low-anc)))
(def san-len (dec (.indexOf san low-anc)))

(+ you-len san-len)