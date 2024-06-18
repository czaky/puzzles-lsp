;;; Package contains tests for math related puzzle solutions.

(defpackage #:puzzles/tree/tests
  (:use #:cl #:puzzles/tree)
  (:import-from #:fiveam #:is #:test))
(in-package #:puzzles/tree/tests)

(fiveam:in-suite* :puzzles)

(test teknonym
  (is (string= "father of A" (teknonym #\m 1 "A")))
  (is (string= "grandmother of A" (teknonym #\f 2 "A")))
  (is (string= "great-great-grandmother of A" (teknonym #\f 4 "A"))))

(test teknonymize
  (let ((tree (make-person
	       :birth-utime 1000 :name "Abdul" :sex #\m
	       :children
	       (list (make-person
		      :birth-utime 800 :name "Sarah" :sex #\f
		      :children
		      (list (make-person :birth-utime 700 :name "Rachel" :sex #\f)))))))

    (teknonymize tree)

    (is (string= "grandfather of Rachel" (person-teknonym tree)))
    (is (string= "mother of Rachel" (person-teknonym (first (person-children tree)))))))
