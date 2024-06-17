;;; Package contains tests for math related puzzle solutions.

(defpackage #:puzzles/math/tests
  (:use #:cl #:puzzles/math)
  (:import-from #:fiveam #:is #:test))
(in-package #:puzzles/math/tests)

(fiveam:def-suite* :puzzles)

(test twice-linear
  (is (= 3 (twice-linear 1)))
  (is (= 22 (twice-linear 10)))
  (is (= 57 (twice-linear 20))))

(test triangle-numbers
  (is (equal '(1 8 27 64 125 216 343 512 729 1000)
             (loop :for n :from 1 to 10 :collect (triangle-numbers n)))))
