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
