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

(defn potential-positions [[x y] max-x max-y]
  (into #{}
        (filter #(and
                  (<= 0 (first %) (dec max-x))
                  (<= 0 (second %) (dec max-y)))
                #{[(+ x 1), y] [(- x 1), y] [x, (+ y 1)] [x, (- y 1)]})))
        

(defn parse-blocks [to-parse parsed max-x max-y]
  (if (empty? to-parse)
    parsed
    (let [item (first to-parse)
          found-block (first (keep-indexed (fn [idx potential] (when (some? (some (potential-positions item max-x max-y) potential)) idx)) parsed))
          updated-block-list (if (nil? found-block)
                               (conj parsed #{item})
                               (update-in parsed [found-block] conj item))]
      (recur (rest to-parse) updated-block-list max-x max-y))))

(defn part-2
  "Day 10 Part 2"
  [input]
  (let [loopen (->> (get-loop input)
                    (apply max-key count))
        sketch (str/split-lines input)
        tiles   (set (for [[y row] (map-indexed list sketch)
                           [x cell] (map-indexed list row)
                           :when (= \. cell)]
                       [x y]))]
    (parse-blocks (sort-by first tiles) [#{[8 9]}] (count (first sketch)) (count sketch))))
