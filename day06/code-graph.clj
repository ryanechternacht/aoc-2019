(ns user
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(defn convert-line [line]
  (let [split (str/split line #"\)")]
    {:from (get split 0) :to (get split 1)}))

(defn get-connections [file]
  (->> file
       slurp
       str/split-lines
       (map convert-line)))

(defn add-connection [tree connection]
  (let [root (first (keys tree))
        from (:from connection)
        children (get tree root)]
    (cond
      (= root from) (assoc tree root (reduce conj children {(:to connection) {}}))
      (empty? children) tree
      :else (assoc tree root (reduce conj (map #(add-connection {(first %) (second %)} connection) children))))))

(defn build-tree [connections]
  (loop [tree {"COM" {}}
         [first & rest] connections]
    (if (nil? first)
      tree
      (recur (add-connection tree first) rest))))

(defn count-orbits
  ([tree] (count-orbits tree 0))
  ([tree depth]
   (let [root (first (keys tree))
         children (get tree root)]
     (reduce + depth (map (fn [[k v]] (count-orbits {k v} (inc depth))) children)))))

(defn p1 []
  (->> "./day06/input.txt"
       get-connections
       build-tree
       count-orbits))

(p1)

; testing
(convert-line "a)d")

(def demo "./day06/demo.txt")
(def demo-connections (get-connections demo))
(def tree (build-tree demo-connections))

(def demo-tree {"COM" {"A" {}}})
(add-connection demo-tree {:from "A" :to "B"})

(def tree {"COM" {"A" {} "AA" {"BB" {}, "CC" {}}}})
(def connection {:from "CC" :to "AAA"})

(map #(add-connection {(first %) (second %)} connection) children)
(->> children
     (map #(add-connection {(first %) (second %)} connection))
     (reduce conj))

(count-orbits tree)

(def tree {"COM" {"A" {"AA" {}} "B" {} "C" {}}})

(def root (first (keys tree)))
(def children (get tree root))

(map (fn [[k v]] (do (prn k) (prn v) (keys v))) tree)

{"C" {"D" {"E" {"F" {}, "J" {"K" {"L" {}}}}, "I" {}}}, "G" {"H" {}}}

(map (fn [[k v]] (count-orbits v 1)) children)

(first (keys tree))
(get tree "COM")

(p1)