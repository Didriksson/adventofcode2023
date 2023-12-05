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


(defn corresponds-to-row [sourcevalue row]
  
  )

(defn corresponds-to [sourcevalue alm-maps]
  (println "Fabriksfabrikören: " alm-maps)
  (println (filter #(<= (:source %) sourcevalue (+ (:source %) (- (:range %) 1))) alm-maps))
  (let [match (first (filter #(<= (:source %) sourcevalue (+ (:range %) (- (:range %) 1))) alm-maps))]
    (if (nil? match)
      sourcevalue
      (:destination match))))
  

(defn part-1
  " Day 05 Part 1 "
  [input]
  (let [almanac (parse-almanac input)]
    (corresponds-to 98 (:seed-to-soil almanac))))

(defn part-2
  "Day 05 Part 2"
  [input]
  input)
