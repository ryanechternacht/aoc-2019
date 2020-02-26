(ns user
  (:require [clojure.set :as set]
            [clojure.edn :as edn]
            [clojure.string :as string]))

(defn convert-line [line]
  (->> line
       (re-seq #"\w+")
       (map #(list (get % 0) (edn/read-string (subs % 1))))
       (map vec)
       vec))

(defn get-directions [file]
  (->> file
       slurp
       string/split-lines
       (map convert-line)
       vec))

(defn add-points-along-line [command x y my-points]
  (let [dir (get command 0)
        x-diff (condp = dir
                 \L -1
                 \R 1
                 \D 0
                 \U 0
                 :else 0)
        y-diff (condp = dir
                 \U 1
                 \D -1
                 \L 0
                 \R 0
                 :else 0)
        length (get command 1)]
    (loop [count 0
           new-x (+ x x-diff)
           new-y (+ y y-diff)
           new-points my-points]
      (if (= count length)
        new-points
        (recur
         (inc count)
         (+ new-x x-diff)
         (+ new-y y-diff)
         (conj new-points [new-x new-y]))))))

(defn add-all-points [commands]
  (->> commands
       (reduce (fn [l c] ; list command
                 (let [last-point (if (empty? l) [0 0] (first l))
                       x (get last-point 0)
                       y (get last-point 1)]
                   (add-points-along-line c x y l)))
               ())
       set))

(defn manhattan-distance [point]
  (let [x (get point 0)
        y (get point 1)
        abs-x (max x (- x))
        abs-y (max y (- y))]
    (+ abs-x abs-y)))

; TODO could generalize this to more lines
(defn find-closest-intersection-length [directions]
  (let [commands1 (get directions 0)
        commands2 (get directions 1)
        line1 (add-all-points commands1)
        line2 (add-all-points commands2)
        intersection (set/intersection line1 line2)]
    (->> intersection
         (map manhattan-distance)
         (reduce min))))

(defn p1 []
  (->> "./input.txt"
       get-directions
       find-closest-intersection-length))

(p1)
;;; testing

(def directions (get-directions "./demo0.txt"))

(add-points-along-line [\D 71] 0 0 ())

(add-all-points (get directions 0))

(if (empty? ()) '(0 0) 0)

(def line1 (add-all-points (get directions 0)))
(def line2 (add-all-points (get directions 1)))

(def i (set/intersection line1 line2))

(->> i
     (map manhattan-distance)
     (reduce min))

(manhattan-distance [-4 5])

(def directions1 (get-directions "./demo1.txt"))
(def directions2 (get-directions "./demo2.txt"))

(find-closest-intersection-length directions1)