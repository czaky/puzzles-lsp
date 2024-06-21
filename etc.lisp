;;; Package contains random, uncategorized puzzles.

(defpackage #:puzzles/etc
  (:use #:cl)
  (:export
   #:latest-clock))
(in-package #:puzzles/etc)

(defun latest-clock (a b c d)
  "Construct a valid, latest time string of the form 'HH:MM' from digits `a b c d`."
  (let* ((digits (sort `(,a ,b ,c ,d) #'>))
         (past8 (and (member 2 digits) (<= (second digits) 5)))
         (time (loop
                :for limit :in (if past8 '(2 3 5 9) '(1 9 5 9))
                :for d = (find-if (lambda (d) (<= d limit)) digits)
                :do (setf digits (delete d digits :count 1))
                :collect d)))
  (format nil "~:@{~D~D:~D~D~}" time)))
