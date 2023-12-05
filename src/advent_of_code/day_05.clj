(ns advent-of-code.day-05
  (:require [clojure.string :as str]))


(defrecord AlmanacRow [destination source range])

(defrecord Almanac
           [seeds seed-to-soil soil-to-fertilizer fertilizer-to-water water-to-light light-to-temperature temperature-to-humidity humidity-to-location])

(defn parse-map [input]
  (let [split (drop 1 (str/split input #"\r\n"))
        numbers (map #(re-seq #"\d+" %) split)
        almancrows (map #(->AlmanacRow (read-string (first %)) (read-string (second %)) (read-string (nth % 2))) numbers)]
    almancrows))

(defn parse-almanac [input]
  (let [input-lines (str/split input #"\r\n\r\n")
        seeds (map read-string (re-seq #"\d+" (first input-lines)))
        seed-to-soil (parse-map (first (drop 1 input-lines)))
        soil-to-fertilizer (parse-map (first (drop 2 input-lines)))
        fertilizer-to-water  (parse-map (first (drop 3 input-lines)))
        water-to-light (parse-map (first (drop 4 input-lines)))
        light-to-temperature (parse-map (first (drop 5 input-lines)))
        temperature-to-humidity (parse-map (first (drop 6 input-lines)))
        humidity-to-location (parse-map (first (drop 7 input-lines)))]
    (->Almanac seeds seed-to-soil soil-to-fertilizer fertilizer-to-water water-to-light light-to-temperature temperature-to-humidity humidity-to-location)))


(defn corresponds-to [sourcevalue alm-maps]
  (let [match (first (filter #(<= (:source %) sourcevalue (+ (:source %) (- (:range %) 1))) alm-maps))]
    (if (nil? match)
      sourcevalue
      (+ (:destination match) (- sourcevalue (:source match))))))
  
(defn seed-to-location [seed almanac]
  (->
   (corresponds-to seed (:seed-to-soil almanac))
   (corresponds-to (:soil-to-fertilizer almanac))
   (corresponds-to (:fertilizer-to-water almanac))
   (corresponds-to (:water-to-light almanac))
   (corresponds-to (:light-to-temperature almanac))
   (corresponds-to (:temperature-to-humidity almanac))
   (corresponds-to (:humidity-to-location almanac))))

(defn part-1
  " Day 05 Part 1 "
  [input]
  (let [almanac (parse-almanac input)]
    (->>
     (:seeds almanac)
     (map #(seed-to-location % almanac))
     (apply min))))

(defn part-2
  "Day 05 Part 2"
  [input]
  (let [almanac (parse-almanac input)
        seedgroups (partition 2 (:seeds almanac))
        minrange (apply min (map first seedgroups))
        maxrange (apply max (map #(+ (first %) (second %)) seedgroups))]
    (->>
     (range minrange maxrange)
     (map #(seed-to-location % almanac))
     (apply min))))