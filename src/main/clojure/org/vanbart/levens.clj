(ns org.vanbart.levens
  (:use clojure.test))

(defn nextelt
  "Given two characters, the previous row, and a row we are
  building, determine out the next element fot this row."
  [char1 char2 prevrow thisrow position]
  (if (= char1 char2)
    (nth prevrow (- position 1))
    (+ 1 (min
      (nth prevrow (- position 1))
      (nth prevrow position)
      (last thisrow))))
  )

(defn nextrow
  "Based on the next character from string1 and the whole of string2
  calculate the next row. Initially thisrow contains one number."
  [char1 str2 prevrow thisrow]
  (let [char2 (first str2)
        position (count thisrow)]
    (if (= (count thisrow) (count prevrow))
      thisrow
      (recur
        char1
        (rest str2)
        prevrow
        (conj thisrow (nextelt char1 char2 prevrow thisrow position))))))

(defn levenshtein
  "Calculate the Levenshtein distance between two strings"
  [str1 str2]
  (let [char1 (first str1)]))

; test combinations
; prevrow 1 2 3 4       6 5 4 3
; thisrow 5 6 ?         4 3 ?
(deftest test-nextelt
  (is (= 2 (nextelt \a \a [1 2 3 4] [5 6] 2)))
  (is (= 3 (nextelt \a \b [1 2 3 4] [5 6] 2)))
  (is (= 4 (nextelt \a \b [6 5 4 3] [4 3] 2))))

;
(deftest test-nextrow
  (is (= [1 0 1] (nextrow \a "aa" [0 1 2] [1])))
  (is (= [2 1 1] (nextrow \b "aa" [1 0 1] [2]))))

(run-tests)