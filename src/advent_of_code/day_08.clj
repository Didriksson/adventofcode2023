(ns advent-of-code.day-08
  (:require [clojure.set :as set]
            [clojure.string :as str]))


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

(defn find-loop-for [instructions current-node steps numberofsteps]
  (if (isAtZ current-node)
    numberofsteps
    (let [finstruction (first instructions)
          restinstruciton (clojure.string/join (rest instructions))
          cyclesinstructions (str restinstruciton finstruction)
          next-node (find-next-node finstruction current-node steps)]
      (recur cyclesinstructions next-node steps (inc numberofsteps)))))

(defn part-1
  "Day 08 Part 1"
  [input]
  (let [[instructions elements] (str/split input #"\r\n\r\n")
        steps (map parse-step (str/split elements #"\r\n"))
        state (->State instructions ["AAA"] steps [])]
    (find-zzz state 0)))

(defn divisors [n]
  (filter #(zero? (rem n %)) (range 2 (inc n))))

(defn lowest-divisor [n]
  (first (divisors n)))


(defn find-gcd [loops]
  (first (apply set/intersection (map set (map divisors loops)))))


(defn part-2
  "Day 08 Part 2"
  [input]
  (let [[instructions elements] (str/split input #"\r\n\r\n")
        steps (mapv parse-step (str/split elements #"\r\n"))
        startnodes (find-start-nodes steps)
        loops (map #(find-loop-for instructions % steps 0) startnodes)
        gcd (find-gcd loops)]
    (* gcd (reduce * (map lowest-divisor loops)))))
