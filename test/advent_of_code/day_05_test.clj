(ns advent-of-code.day-05-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code.day-05 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1
  (let [expected 35]
    (is (= expected (part-1 (slurp (resource "day-05-example.txt")))))))

(deftest part2
  (let [expected 46]
    (is (= expected (part-2 (slurp (resource "day-05-example.txt")))))))
