(ns advent-of-code.day-10
  (:require [clojure.set :refer [union]]
            [clojure.string :as str]))

(defn get-for-position [x y sketch]
  (nth (nth sketch y) x))

(defn get-start-position [sketch]
  (first (for [[y row] (map-indexed list sketch)
               [x cell] (map-indexed list row)
               :when (= \S cell)]
           {:x x :y y :value cell})))

(defn accepts-from [from-x from-y to-x to-y val]
  (let [dp [(- to-x from-x) (- to-y from-y)]]
    (case dp
      [1 0] (cond
              (some? (some #{val} "-7S")) true
              :else false)
      [-1 0] (cond
               (some? (some #{val} "-LS")) true
               :else false)
      [0 -1] (cond
               (some? (some #{val} "7|S")) true
               :else false)
      [0 1] (cond
              (some? (some #{val} "J|LS")) true
              :else false)
      [] "Tomtom")))

(defn potential-paths [val]
  (case val
    \J [[0 -1] [-1 0]]
    \7 [[0  1] [-1 0]]
    \- [[-1  0] [1 0]]
    \| [[0  1] [0 -1]]
    \L [[0  1] [1 0]]
    \S [[1 0] [-1 0] [0 -1] [0 1]]))

(defn eval-adjacents [x y sketch]
  (let [potential (potential-paths (get-for-position x y sketch))]
    (filter #(accepts-from x y (+ x (first %)) (+ y (second %)) (get-for-position (+ x (first %)) (+ y (second %)) sketch)) potential)))


(defn create-position [x y dx dy]
  [(+ x dx) (+ y dy)])

(defn part-1
  "Day 10 Part 1"
  [input]
  (let [sketch (str/split-lines input)
        start (get-start-position sketch)]
    (map #(create-position (:x start) (:y start) (first %) (second %)) (eval-adjacents (:x start) (:y start) sketch))))

  (defn part-2
    "Day 10 Part 2"
    [input]
    input)
