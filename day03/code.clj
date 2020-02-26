(ns user
  (:require [clojure.set :as set]
            [clojure.edn :as edn]
            [clojure.string :as string]))

;; In retrospect, this should have all been written with maps

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

; TODO redo the loop with mapcat and range
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

(defn add-points-along-line-2 [command x y my-points]
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
        length (get command 1)
        steps (count my-points)]
    (loop [count 0
           new-x (+ x x-diff)
           new-y (+ y y-diff)
           new-points my-points
           new-steps (inc steps)]
      (if (= count length)
        new-points
        (recur
         (inc count)
         (+ new-x x-diff)
         (+ new-y y-diff)
         (conj new-points [new-x new-y new-steps])
         (inc new-steps))))))

(defn add-all-points-2 [commands]
  (->> commands
       (reduce (fn [l c] ; list command
                 (let [last-point (if (empty? l) [0 0] (first l))
                       x (get last-point 0)
                       y (get last-point 1)]
                   (add-points-along-line-2 c x y l)))
               ())))

(defn find-intersections [line1 line2]
  (vec
   (for [p1 line1 p2 line2
         :when (let [x1 (get p1 0)
                     y1 (get p1 1)
                     x2 (get p2 0)
                     y2 (get p2 1)]
                 (and (= x1 x2) (= y1 y2)))]
     (vec (list p1 p2)))))

(defn find-intersection-length [intersection]
  (let [dis1 (get-in intersection [0 2])
        dis2 (get-in intersection [1 2])]
    (+ dis1 dis2)))

(defn find-closest-intersection-length [directions]
  (let [commands1 (get directions 0)
        commands2 (get directions 1)
        line1 (add-all-points-2 commands1)
        line2 (add-all-points-2 commands2)
        intersections (find-intersections line1 line2)]
    (->> intersections
         (map find-intersection-length)
         (reduce min))))

(defn p2 []
  (->> "./input.txt"
       get-directions
       find-closest-intersection-length))

;;; testing
(def directions (get-directions "./demo0.txt"))

(def input (get-directions "./input.txt"))

(def line1 (add-all-points-2 (get input 0)))
(def line2 (add-all-points-2 (get input 1)))

(def intersections (find-intersections line1 line2))

line1

intersections

(map prn intersections)

(->> intersections
     (map find-intersection-length))

(find-intersection-length [[3 3] [3 3]])

(def directions1 (get-directions "./demo1.txt"))
(def directions2 (get-directions "./demo2.txt"))

(def input (get-directions "./input.txt"))

(find-closest-intersection-length directions2)

(find-closest-intersection-length input)