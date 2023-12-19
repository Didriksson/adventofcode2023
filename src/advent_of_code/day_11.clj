(ns advent-of-code.day-11
    (:require
     [clojure.string :as str]))

(defrecord Galaxy [x y])

(defn parse-row [row-idx row]
  (filter some? (map-indexed (fn [idx col]
                              (when (= col \#)
                                [idx row-idx])) row)))

(defn part-1
  "Day 11 Part 1"
  [input]
  (->>
   (str/split-lines input)
   (->
    (map-indexed parse-row))))

(defn part-2
  "Day 11 Part 2"
  [input]
  input)
