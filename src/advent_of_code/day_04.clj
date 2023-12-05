(ns advent-of-code.day-04
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defrecord Game [id winning-numbers on-hand copies])

(defn parse-numbers [row]
  (map read-string (re-seq #"\d+" row)))

(defn parse-game [row]
  (let [[id-remove id] (re-find #"Card\s+(\d+):" row)
        rest (subs row (count id-remove))
        [winning-raw hand-raw] (str/split rest #"\|")]
    (->Game (read-string id) (parse-numbers winning-raw) (parse-numbers hand-raw) 1)))

(defn check-winning-hands [game]
  (set/intersection (set (:on-hand game)) (set (:winning-numbers game))))

(defn part-1
  "Day 04 Part 1"
  [input]
  (let [games (->>
               (str/split input #"\r\n")
               (map parse-game))]
    (->> games
         (map check-winning-hands)
         (filter #(> (count %) 0))
         (map #(Math/pow 2 (- (count %) 1)))
         (reduce +)
         (int))))


(defn multiplier-update [game multiplier]
  (assoc game :copies (+ multiplier (:copies game))))

(defn update-copies [multiplier toupdate restofgames]
  (let [updated (map #(multiplier-update % multiplier) toupdate)]
    (concat updated restofgames)))

(defn process-cards [processed to-scratch]
  (if (empty? to-scratch)
    processed
    (let [game (first to-scratch)
          tail-games (rest to-scratch)
          number-of-winning (count (check-winning-hands game))
          to-update (take number-of-winning tail-games)
          restofgames (drop number-of-winning tail-games)]
      (recur (cons game processed) (update-copies (:copies game) to-update restofgames)))))

(defn part-2
  "Day 04 Part 2"
  [input]
  (let [games (->>
               (str/split input #"\r\n")
               (map parse-game))]
    (reduce + (map :copies (process-cards [] games))))) 
