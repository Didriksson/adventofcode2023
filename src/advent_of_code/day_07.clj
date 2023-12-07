(ns advent-of-code.day-07
  (:require [clojure.string :as str]))


(defrecord Hand [cards score eval])

(defn value-for-card [card part-b]
  (case card
    \A 14
    \K 13
    \Q 12
    \J (if (true? part-b) 1 11)
    \T 10
    (read-string (str card))))

(defn hand-strength [hand-eval]
  (case hand-eval
    :five-of-a-kind 7
    :four-of-a-kind 6
    :full-house 5
    :three-of-a-kind 4
    :two-pair 3
    :one-pair 2
    :high-card 1))

(defn parse-hand [raw]
  (let
   [cards (first raw)
    score (read-string (second raw))]
    (->Hand cards score nil)))

(defn eval-hand [hand]
  (let [top-two (take 2 (reverse (sort (vals (frequencies hand)))))
        x (first top-two)
        y (second top-two)]
    (cond
      (and (= x 5) (nil? y)) :five-of-a-kind
      (and (= x 4) (= y 1)) :four-of-a-kind
      (and (= x 3) (= y 2)) :full-house
      (and (= x 3) (= y 1)) :three-of-a-kind
      (and (= x 2) (= y 2)) :two-pair
      (and (= x 2) (= y 1)) :one-pair
      :else :high-card)))

(defn compare-by-face-value [c1 c2 part-b]
  (let [c1-face-value (map #(value-for-card % part-b) c1)
        c2-face-value (map #(value-for-card % part-b) c2)]
    (compare (vec c1-face-value) (vec c2-face-value))))

(defn hand-comparator [h1 h2]
  (let [compare-result (compare (hand-strength (:eval h1))
                                (hand-strength (:eval h2)))]
    (if (not= compare-result 0)
      compare-result
      (compare-by-face-value (:cards h1) (:cards h2) false))))

(defn hand-comparator-b [h1 h2]
  (let [compare-result (compare (hand-strength (:eval h1))
                                (hand-strength (:eval h2)))]
    (if (not= compare-result 0)
      compare-result
      (compare-by-face-value (:cards h1) (:cards h2) true))))

(defn part-1
  "Day 07 Part 1"
  [input]
  (->>
   (str/split input #"\r\n")
   (map #(str/split % #" "))
   (map parse-hand)
   (map #(assoc % :eval (eval-hand (:cards %))))
   (sort hand-comparator)
   (map-indexed (fn [idx it] (* (+ 1 idx) (:score it))))
   (reduce +)))

(defn pretender-eval [cards]
  (let [
        freqs (reverse (sort-by second (frequencies cards)))
        card-to-pretend (or (first (drop-while #(= \J %) (keys freqs))) \J)
        pretend-hand (str/replace cards #"J" (str card-to-pretend))]
    (eval-hand pretend-hand)))

(defn part-2
  "Day 07 Part 2"
  [input]
  (->>
   (str/split input #"\r\n")
   (map #(str/split % #" "))
   (map parse-hand)
   (map #(assoc % :eval (pretender-eval (:cards %))))
   (sort hand-comparator-b)
   (map-indexed (fn [idx it] (* (+ 1 idx) (:score it))))
   (reduce +)))
