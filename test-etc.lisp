;;; Tests for uncategorized puzzle solutions.

(defpackage #:puzzles/etc/tests
  (:use #:cl #:puzzles/etc)
  (:import-from #:fiveam #:is #:test))
(in-package #:puzzles/etc/tests)

(fiveam:in-suite* :puzzles)

(test latest-clock
  (is (string= "21:59" (latest-clock 9 1 2 5)))
  (is (string= "19:38" (latest-clock 1 9 8 3)))
  (is (string= "19:28" (latest-clock 1 2 8 9)))
  (is (string= "00:00" (latest-clock 0 0 0 0))))
