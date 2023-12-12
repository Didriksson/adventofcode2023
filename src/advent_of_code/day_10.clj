(ns advent-of-code.day-10
  (:require [clojure.set :refer [union]]
            [clojure.string :as str]))


(defrecord Path [point visited])

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
              (some? (some #{val} "-7SJ")) true
              :else false)
      [-1 0] (cond
               (some? (some #{val} "-LSF")) true
               :else false)
      [0 -1] (cond
               (some? (some #{val} "7|SF")) true
               :else false)
      [0 1] (cond
              (some? (some #{val} "J|LS")) true
              :else false))))

(defn potential-paths [val]
  (case val
    \| [[0  1] [0 -1]]
    \- [[-1  0] [1 0]]
    \L [[0  -1] [1 0]]
    \J [[0  -1] [-1 0]]
    \7 [[0   1] [-1 0]]
    \F [[0   1] [1 0]]
    \. []
    \S [[1 0] [-1 0] [0 -1] [0 1]]))

(defn eval-adjacents [x y sketch]
  (let [max-y (count sketch)
        max-x (count (first sketch))
        potential (filter #(and (<= 0 (+ x (first %)) (- max-x 1)) (<= 0 (+ y (second %)) (- max-y 1))) (potential-paths (get-for-position x y sketch)))]
    (filter #(accepts-from x y (+ x (first %)) (+ y (second %)) (get-for-position (+ x (first %)) (+ y (second %)) sketch)) potential)))


(defn create-position [x y dx dy]
  [(+ x dx) (+ y dy)])

(defn perform-eval-for-point [point visited sketch]
  (println point)
  (->>
   (eval-adjacents (first point) (second point) sketch)
   (mapv #(create-position (first point) (second point) (first %) (second %)))
   (filter #(nil? (some #{%} visited)))))

(defn find-paths [paths sketch]
  (let
   [new-points (mapcat #(perform-eval-for-point (:point %) (:visited %) sketch) paths)
    updated-visited (mapv #(->Path % (conj (:visited %)))  new-points)]
    (if (empty? updated-visited)
      paths
      (recur updated-visited sketch))))

(defn part-1
  "Day 10 Part 1"
  [input]
  (let [sketch (str/split-lines input)
        start (get-start-position sketch)
        evalutae-paths (find-paths [(->Path [(:x start) (:y start)] (cons [(:x start) (:y start)] []))]  sketch)]
    (->>
     (flatten evalutae-paths)
     (map :visited)
     (map count)
     (apply max)
     (#(/ % 2)))))

  (defn part-2
    "Day 10 Part 2"
    [input]
    input)
