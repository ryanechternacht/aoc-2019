(ns user
  (:require [clojure.edn :as edn]))

(def low 273025)
(def high 767253)

; naive, should be lazy
(defn get-trials [low high]
  (let [length (count (str high))
        str-f (str "%0" length "d")]
    (->> (range low (inc high))
         (map #(format str-f %)))))

; ensures a number is in increasing order
(defn not-decreasing [number]
  (let [s (str number)
        len (count s)]
    (loop [index 0]
      (if (= index (dec len))
        true
        (let [x (edn/read-string (subs s index (inc index)))
              y (edn/read-string (subs s (inc index) (+ 2 index)))]
          (if (> x y)
            false
            (recur (inc index))))))))

(not-decreasing 111111) ; true
(not-decreasing 111123) ; true
(not-decreasing 113340) ; false
(not-decreasing 135357) ; false
(not-decreasing 635357) ; false

; ensures a number has a double in it
(defn double-found [number]
  (let [s (str number)
        len (count s)]
    (loop [index 0]
      (if (= index (dec len))
        false
        (let [x (edn/read-string (subs s index (inc index)))
              y (edn/read-string (subs s (inc index) (+ 2 index)))]
          (if (= x y)
            true
            (recur (inc index))))))))

(double-found 111111) ; true
(double-found 123445) ; true
(double-found 123540) ; false
(double-found 135357) ; false
(double-found 635355) ; true

(defn p1
  ([] (p1 low high))
  ([low high] (->> (get-trials low high)
                   (filter not-decreasing)
                   (filter double-found)
                   count)))

(defn get-digit-at [str pos]
  (let [len (count str)]
    (if (or (< pos 0) (>= pos len))
      "a"
      (edn/read-string (subs str pos (inc pos))))))

(defn only-double-found [number]
  (let [s (str number)
        last-item (dec (count s))]
    (loop [index 0]
      (if (= index last-item)
        false
        (let [before (get-digit-at s (dec index))
              a (get-digit-at s index)
              b (get-digit-at s (inc index))
              after (get-digit-at s (+ index 2))]
          (if (= a b)
            (if (and (not (= before a)) (not (= b after)))
              true
              (recur (inc index)))
            (recur (inc index))))))))

(get-digit-at "123456" 3)

(only-double-found 111111) ; false
(only-double-found 123445) ; true
(only-double-found 123444) ; false
(only-double-found 112233) ; true
(only-double-found 111223) ; true

(defn p2
  ([] (p2 low high))
  ([low high] (->> (get-trials low high)
                   (filter not-decreasing)
                   (filter only-double-found)
                   count)))

(p2 low high)

(p1)

(p2)