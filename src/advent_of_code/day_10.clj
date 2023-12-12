(ns advent-of-code.day-10
  (:require [clojure.set :refer [difference]]
            [clojure.string :as str]))


(defrecord Path [point visited])
(defrecord Point [x y])

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
    \. [[1 0] [-1 0] [0 -1] [0 1]]
    \S [[1 0] [-1 0] [0 -1] [0 1]]))

(defn eval-adjacents [x y sketch]
  (let [max-y (count sketch)
        max-x (count (first sketch))
        potential (filter #(and (<= 0 (+ x (first %)) (- max-x 1)) (<= 0 (+ y (second %)) (- max-y 1))) (potential-paths (get-for-position x y sketch)))]
    (filter #(accepts-from x y (+ x (first %)) (+ y (second %)) (get-for-position (+ x (first %)) (+ y (second %)) sketch)) potential)))


(defn create-position [x y dx dy]
  [(+ x dx) (+ y dy)])

(defn perform-eval-for-point [point visited sketch]
  (->>
   (eval-adjacents (first point) (second point) sketch)
   (mapv #(create-position (first point) (second point) (first %) (second %)))
   (filter #(nil? (some #{%} visited)))
   (map #(->Path % (conj visited %)))))

(defn find-paths [paths sketch]
  (let
   [new-points (mapcat #(perform-eval-for-point (:point %) (:visited %) sketch) paths)]
    (if (empty? new-points)
      paths
      (recur new-points sketch))))

(defn get-loop [input]
  (let [sketch (str/split-lines input)
        start (get-start-position sketch)
        evalutae-paths (find-paths [(->Path [(:x start) (:y start)] (cons [(:x start) (:y start)] []))]  sketch)]
    (->>
     (flatten evalutae-paths)
     (map :visited))))

(defn part-1
  "Day 10 Part 1"
  [input]
  (->>
   (get-loop input)
   (map count)
   (apply max)
   (#(/ % 2))))


(defn transform-point [point]
  (let [potential #{[1 0] [-1 0] [0 -1] [0 1]}]
    (map #(vector (+ (first point) (first %)) (+ (second point) (second %))) potential)))

(defn parse-block-path [to-parse visited sketch]
  (if (empty? to-parse)
    visited
    (let [max-y (count sketch)
          max-x (count (first sketch))
          potential (filter #(and (<= 0 (first %) (- max-x 1)) (<= 0 (second %) (- max-y 1))) (mapcat transform-point to-parse))
          filtered-non-visited (filter #(nil? (some #{%} visited)) potential)
          tiles (filter #(= (get-for-position (first %) (second %) sketch) \.) filtered-non-visited)
          updated-visited (concat visited tiles)]
      (recur tiles updated-visited sketch))))


(defn atEdge [item sketch]
  (let [max-y (count sketch)
        max-x (count (first sketch))]
    (or
     (= 0 (first item))
     (= 0 (second item))
     (= (dec max-x) (first item))
     (= (dec max-y) (second item)))))


(defn isEnclosed [block sketch]
  (not (some? (first (filter #(atEdge % sketch) block)))))


(defn parse-all-tile-blocks [tiles sketch]
  (let [point-tiles (map #(->Point (first %) (second %)) tiles)]
    (loop [all-paths []]
      (let [start (first (difference (set point-tiles) (set (flatten all-paths))))]
        (if (nil? start)
          all-paths
          (recur (conj all-paths (map #(->Point (first %) (second %)) (parse-block-path [(vector (:x start) (:y start))] [(vector (:x start) (:y start))] sketch)))))))))




(defn block-to-point [row]
  (map #(vector (:x %) (:y %)) row))

(defn part-2
  "Day 10 Part 2"
  [input]
  (let [loopen (->> (get-loop input)
                    (apply max-key count))
        sketch (str/split-lines input)
        tiles   (for [[y row] (map-indexed list sketch)
                      [x cell] (map-indexed list row)
                      :when (= \. cell)]
                  [x y])]
    (->>
     (parse-all-tile-blocks tiles sketch)
     (map block-to-point)
     (filter #(isEnclosed % sketch))
     (map count)
     (reduce +))))