(ns advent-of-code.day-08 
  (:require [clojure.string :as str]))


(defrecord Step [node lv rv])
(defrecord State [current steps])
(def state (atom (->State nil nil)))

(defn parse-step [input]
  (let [[node lv rv] (drop 1 (re-find #"(.+)\s=\s[(](.+),\s(.+)[)]" input))]
    (->Step node lv rv)))

(defn find-next-node [state instruction]
  (let [current-node (first (filter #(= (:current state) (:node %)) (:steps state)))]
    (if (= instruction \L)
      (->State (:lv current-node) (:steps state))
      (->State (:rv current-node) (:steps state)))))

(defn part-1
  "Day 08 Part 1"
  [input]
  (let [[instructions elements] (str/split input #"\r\n\r\n")
        steps (map parse-step (str/split elements #"\r\n"))]
    (reset! state (->State "AAA" steps))
    (->>
     (cycle instructions)
     (map find-next-node @state)
     (reset! state)
     (take-while #(not= (:current %) "ZZZ"))
     (take 2))))

(defn part-2
  "Day 08 Part 2"
  [input]
  input)
