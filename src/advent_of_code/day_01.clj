(ns advent-of-code.day-01
  (:require [clojure.string :as str]))

(defn digit? [c]
  (. Character isDigit c))


(defn parseLines [input]
  (str/split input #"\r\n"))

(defn parseCalibrationItem [numbers]
  (str (first numbers) (last numbers)))

(defn handleLine [input]
  (->>
   (filter digit? input)
   (parseCalibrationItem)
   (read-string)))

(defn parseSpelledDigitLine [line]
  (-> (clojure.string/replace line #"one" "one1one")
      (clojure.string/replace #"two" "two2two")
      (clojure.string/replace #"three" "three3three")
      (clojure.string/replace #"four" "four4four")
      (clojure.string/replace #"five" "five5five")
      (clojure.string/replace #"six" "six6six")
      (clojure.string/replace #"seven" "seven7seven")
      (clojure.string/replace #"eight" "eight8eight")
      (clojure.string/replace #"nine" "nine9nine")))

(defn part-1
  "Day 01 Part 1"
  [input]
  (->>
   (parseLines input)
   (map handleLine)
   (reduce +)))

(defn part-2
  "Day 01 Part 2"
  [input]
  (->> (parseLines input)
       (map parseSpelledDigitLine)
       (map handleLine)
       (reduce +)))
