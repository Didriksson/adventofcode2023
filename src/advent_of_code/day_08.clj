(ns advent-of-code.day-08
  (:require [clojure.string :as str]))


(defrecord Step [node lv rv])
(defrecord State [instructions current steps visited])

(defn parse-step [input]
  (let [[node lv rv] (drop 1 (re-find #"(.+)\s=\s[(](.+),\s(.+)[)]" input))]
    (->Step node lv rv)))

(defn find-next-node [instruction current steps]
  (let [current-node (first (filter #(= current (:node %)) steps))]
    (if (= instruction \L)
      (:lv current-node)
      (:rv current-node))))

(defn find-zzz [state numberofsteps]
  (if (= (first (:current state)) "ZZZ")
    numberofsteps
    (let [instructions (:instructions state)
          finstruction (first instructions)
          restinstruciton (clojure.string/join (rest instructions))
          cyclesinstructions (str restinstruciton finstruction)
          next-node (find-next-node finstruction (first (:current state)) (:steps state))]
      (recur (->State cyclesinstructions [next-node] (:steps state) []) (inc numberofsteps)))))


(defn isAtZ [node]
  (= (last node) \Z))


(defn find-start-nodes [steps]
  (filter #(= (last %) \A) (map :node steps)))

(defn find-all-z [state numberofsteps]  
  (if (every? isAtZ (:current state))
    numberofsteps
    (let [instructions (:instructions state)
          finstruction (first instructions)
          restinstruciton (clojure.string/join (rest instructions))
          cyclesinstructions (str restinstruciton finstruction)
          next-nodes (map #(find-next-node finstruction % (:steps state)) (:current state))]
      (if (some #{next-nodes} (:visited state))
        (println next-nodes " BOOM BOOM " (:visited state))
        (recur (->State cyclesinstructions next-nodes (:steps state) (cons next-nodes (:visited state))) (inc numberofsteps))))))

(defn part-1
  "Day 08 Part 1"
  [input]
  (let [[instructions elements] (str/split input #"\r\n\r\n")
        steps (map parse-step (str/split elements #"\r\n"))
        state (->State instructions ["AAA"] steps [])]
    (find-zzz state 0)))



(defn part-2
  "Day 08 Part 2"
  [input]
  (let [[instructions elements] (str/split input #"\r\n\r\n")
        steps (map parse-step (str/split elements #"\r\n"))
        state (->State instructions (find-start-nodes steps) steps [(find-start-nodes steps)])]
    (find-all-z state 0)))
