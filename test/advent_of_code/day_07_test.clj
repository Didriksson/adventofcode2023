(ns advent-of-code.day-07-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code.day-07 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1
  (let [expected 6440]
    (is (= expected (part-1 (slurp (resource "day-07-example.txt")))))))

(deftest part2
  (let [expected 5905]
    (is (= expected (part-2 (slurp (resource "day-07-example.txt")))))))
