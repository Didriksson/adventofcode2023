(ns advent-of-code.day-02
  (:require [clojure.string :as str]))

(defrecord Set [blue green red])
(defrecord Game [id set])


(defn find-and-sum-for-color [regex row]
  (reduce + (map read-string (map last (re-seq regex row)))))


(defn parse-set [set]
  (let [blue (find-and-sum-for-color #"(\d+) blue" set)
        green (find-and-sum-for-color #"(\d+) green" set)
        red (find-and-sum-for-color #"(\d+) red" set)]
    (->Set blue green red)))

(defn parse-game [row]
  (let [id (read-string (last (re-find #"Game (\d+)" row)))
        set (map parse-set (str/split row #";"))]
    (->Game id set)))

(defn isPossible? [blue green red game]
  (and
   (<= (.blue game) blue)
   (<= (.green game) green)
   (<= (.red game) red)))

(defn possible-game [game]
  (every? #(isPossible? 14 13 12 %) (.set game)))

(defn max-for-each-color [game]
  (let [blue (apply max (map #(.blue %) (.set game)))
        green (apply max (map #(.green %) (.set game)))
        red (apply max (map #(.red %) (.set game)))]
    (->Set blue green red)))

(defn part-1
  "Day 02 Part 1"
  [input]
  (->>
   (str/split input #"\r\n")
   (map parse-game)
   (filter possible-game)
   (map #(.id %))
   (reduce +)))


(defn part-2
  "Day 02 Part 2"
  [input]
  (->>
   (str/split input #"\r\n")
   (map parse-game)
   (map max-for-each-color)
   (map #(reduce * (vals %)))
   (reduce +)))
