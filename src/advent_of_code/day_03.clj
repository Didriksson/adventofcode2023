(ns advent-of-code.day-03
  (:require [clojure.string :as str]))

(defn digit? [c]
  (. Character isDigit c))

(defn parse-number-groups-for-row [x-offset index-y row]
  (let [digits (take-while #(digit? (:value %)) (map-indexed (fn [index item] 
                                                               {:x (+ x-offset index) :y index-y :value item}) row))
        toRemove (if (empty? digits)
                   (count (take-while #(not (digit? %)) row))
                   (count digits))
        remaining (drop toRemove row)]
    (if (empty? remaining)
      [(cons digits [])]
      (let [[digits2 _] (parse-number-groups-for-row (+ toRemove x-offset) index-y remaining)]
        [(cons digits digits2) []]))))

(defn parse-number-groups [schematics]
  (apply concat (map-indexed (fn [rindex itemrow]
                               (->>
                                (parse-number-groups-for-row 0 rindex itemrow)
                                (first))) schematics)))

(defn is-a-symbol? [x y schematics]
  (when (and
         (< 0 y (count schematics))
         (< 0 x (count (first schematics))))
    (let [val (nth (nth schematics y) x)]
      (and
       (not (digit? val))
       (not (= \. val))))))


(defn check-if-adjacent [item schematics]
  (let [x (:x item)
        y (:y item)
        _ (:value item)]
    (or
     (is-a-symbol? (- x 1) (- y 1) schematics)
     (is-a-symbol? x (- y 1) schematics)
     (is-a-symbol? (+ x 1) (- y 1) schematics)
     (is-a-symbol? (- x 1) y schematics)
     (is-a-symbol? (+ x 1) y schematics)
     (is-a-symbol? (- x 1) (+ y 1) schematics)
     (is-a-symbol? x (+ y 1) schematics)
     (is-a-symbol? (+ 1 x) (+ y 1) schematics))))

(defn check-group [schematic group]
  (some true? (map #(check-if-adjacent % schematic) group)))

(defn to-number [group]
  (read-string (apply str (map #(:value %) group))))

(defn part-1
  "Day 03 Part 1"
  [input]
  (let [schematics (->
                    (str/split input #"\r\n"))]
    (->>
     (parse-number-groups schematics)
     (filter not-empty)
     (filter #(check-group schematics %))
     (map to-number)
     (reduce +))))


(defn find-gears [schematics]
  (for [[i row] (map-indexed list schematics)
        [j cell] (map-indexed list row)
        :when (= \* cell)]
    {:x j :y i :value cell}))


(defn find-in-group [gear group]
  (let [group-coordinates (map (fn [it] {:x (:x it) :y (:y it)}) group)]
    (some (fn [group]
            (and
             (<= (Math/abs (- (:x gear) (:x group))) 1)
             (<= (Math/abs (- (:y gear) (:y group))) 1))) group-coordinates)))

(defn find-in-number-groups [gear number-groups]
  (let [groups-for-gear (filter #(find-in-group gear %) number-groups)]
    [gear groups-for-gear]))

(defn product-per-gear [[gear group]]
  [gear (map to-number group)])

(defn part-2
  "Day 03 Part 2"
  [input]
  (let [schematics (->
                    (str/split input #"\r\n"))
        number-groups (->>
                       (parse-number-groups schematics)
                       (filter not-empty))
        gears (find-gears schematics)]
    (->>
     (map #(find-in-number-groups % number-groups) gears)
     (map product-per-gear)
     (filter #(>= (count (second %)) 2))
     (map #(reduce * (second %)))
     (reduce +))))

