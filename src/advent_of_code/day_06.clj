(ns advent-of-code.day-06
  (:require [clojure.string :as str]))

(defrecord Race [time distance])


(defn parse-race [input]
  (->>
   (str/split input #"\n")
   (map #(re-seq #"\d+" %))
   (apply interleave)
   (partition 2)
   (map #(->Race (read-string (first %)) (read-string (second %))))))

(defn simulate-race [race]
  (let [time (:time race)
        to-beat (:distance race)]
    (for [hold-time (range (+ time 1))
          :let [distance-covered (* (- time hold-time) (* hold-time 1))]
          :when (> distance-covered to-beat)]
      hold-time)))

(defn merge-races [races]
  (let [time (reduce str (map #(:time %) races))
        distance-to-beat (reduce str (map #(:distance %) races))]
    (->Race (read-string time) (read-string distance-to-beat))))

(defn part-1
  "Day 06 Part 1"
  [input]
  (->> (parse-race input)
       (map simulate-race)
       (map count)
       (reduce *)))

(defn part-2
  "Day 06 Part 2"
  [input]
  (->> (parse-race input)
       (merge-races)
       (simulate-race)
       (count)))