(ns advent-of-code.day-09
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (map read-string (re-seq #"-?\d+" line)))

(defn next-in-range [readings]
  (let [differences (map #(reduce - (reverse %)) (partition 2 1 readings))]
    (if (every? zero? differences)
      (+ (last readings) 0)
      (+ (last readings) (next-in-range differences)))))

(defn previous-in-range [readings]
  (let [differences (map #(reduce - (reverse %)) (partition 2 1 readings))]
    (if (every? zero? differences)
      (- (first readings) 0)
      (- (first readings) (previous-in-range differences)))))

(defn part-1
  "Day 09 Part 1"
  [input]
  (->>
   (map parse-line (str/split-lines input))
   (map next-in-range)
   (reduce +)))

(defn part-2
  "Day 09 Part 2"
  [input]
  (->>
   (map parse-line (str/split-lines input))
   (map previous-in-range)
   (reduce +)))