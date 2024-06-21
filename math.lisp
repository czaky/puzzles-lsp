;;; Package contains math related puzzles.

(defpackage #:puzzles/math
  (:use #:cl)
  (:export
   #:twice-linear
   #:triangle-numbers
   #:gray-mystery-function
   #:inverse-gray-mystery-function
   #:burner
   #:digit-power))
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

(defun triangle-numbers (n)
  "Return the sum of numbers from $n^th$ row of the following triangle.

                1
              3     5
          7     9    11
      13    15    17    19
    21    23    25    27    29
    ...
  "
  (declare (fixnum n))
  (* n n n))

(defun gray-mystery-function (n)
  "Return numbers created by reflection process.

  Each resulting number is drawn from a table $T(m)$,
  where $m$ is the number of bits in $n$. The resulting
  number is at index $n$ in that table $T(m)$.

  Table $T(m)$ is constructed by taking $T(m-1)$,
  reversing the order of the elements and
  prepending 1 in binary representation.

  Examples:
   $$ T(1) = [ 0, 1 ]$$
   $$ T(2) = [ 0, 1, 3, 2 ]$$
   $$ T(3) = [ 0, 1, 3, 2, 6, 7, 5, 4 ]$$
   `` f(6) = 5 ``
  "
  (declare (fixnum n))
  (logxor n (ash n -1)))

(defun inverse-gray-mystery-function (n)
  "Return the inverse number to the `gray-mystery-function` number above."
  (if (zerop n) 0
      (logxor n (inverse-gray-mystery-function (ash n -1)))))

(defun burner (c h o)
  "Coal `c`, hydrogen `h` and oxygen `o` are burning to water, dioxide, and methane."
  (let* ((w (min (floor h 2) o))
         (d (min c (floor (- o w) 2)))
         (m (min (- c d) (floor (- h (* 2 w)) 4))))
    (values w d m)))

(defun digit-power (n p)
  "Return the integer factor of `n` that makes it equal to
  the sum of all `n` digits raised to a consecutive power expenents
  starting at exponent `p`.

  Examples:
  ---------------
  [n = 89, p = 1]
  89 * 1 = (8^1 + 9^2) = 1 * (8 + 81)
  => 1

  [n = 694, p = 2]
  695 * 2 = (6^2 + 9^3 + 5^4) = (36 + 729 + 625)
  "
  (loop
    :with m integer = n :with r integer = 0
    :for p integer :downfrom (+ p (floor (log n 10)))
    :do (setf (values m r) (floor m 10))
    :sum (expt r p) :into pow
    :until (zerop m)
    :finally
    (return (multiple-value-bind (k r) (floor pow n)
              (if (zerop r) k -1)))))
