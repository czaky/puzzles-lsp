;;; Package contains math related puzzles.

(defpackage #:puzzles/math
  (:use #:cl)
  (:export #:twice-linear))
(in-package #:puzzles/math)

(defun twice-linear (n)
  "Get the $n^th$ element in the sequence: S = 1, 3, 4, 7, 9, 10, 13, 15, ...

  The sequence is defined by:
      * $s_0$ = 1
      * for each $s_i$ in S both $2s_i + 1$ and $3s_i + 1$
        are also in the sequence.
  "
  (declare (fixnum n))
  ;; The idea is to just use two lists as (non-soreted) queues.
  ;; Iterate through the lists picking the smallest element,
  ;; and append $2s_i + 1$ and $3s_i + 1$ the the first and second
  ;; list respectively.
  (loop
    :with a = (list 1) :with at = a
    :with b = (list 1) :with bt = b
    :for x fixnum = (min (car a) (car b))
    :repeat n
    :do
      (nconc at (list (1+ (* 2 x))))
      (nconc bt (list (1+ (* 3 x))))
      (pop at)
      (pop bt)
      ;; Remove x from a or b (and duplicates as well).
      (if (= x (car a)) (pop a))
      (if (= x (car b)) (pop b))
    :finally (return x)))
