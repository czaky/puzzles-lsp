;;; Tree based puzzles.

(defpackage #:puzzles/tree
  (:use #:cl)
  (:export
   #:person
   #:teknonym
   #:teknonymize
   #:make-person
   #:person-teknonym
   #:person-children))
(in-package #:puzzles/tree)

;; Teknonyms

(deftype sex () '(member #\f #\m))

(defstruct person
  ;; A person in a family-tree.
  (birth-utime nil :type fixnum)
  (name nil :type string)
  ;; Input/output to `teknonymize`.
  (teknonym "" :type string)
  (sex nil :type sex)
  (children nil :type list))

(defun teknonym (sex level name)
  "Generate the teknonym for person `sex` and its descendant `level` and `name`."
  (declare (type sex sex) (fixnum level) (string name))
  (format nil "~v@{~A~:*~}~*~:[~;grand~]~:[father~;mother~] of ~A"
    (- level 2) "great-" (> level 1) (char= sex #\f) name))

(defun teknonymize (tree)
  "Add teknonyms to the family `tree`."
  (declare (type person tree))
  (labels
    ((descend (n)
      (declare (type person tree))
      (if (person-children n)
        (multiple-value-bind (level d) (descend (first (person-children n)))
          (dolist (c (cdr (person-children n)))
            (multiple-value-bind (clevel cd) (descend c)
              (when (or (< level clevel)
                        (and (= level clevel)
                             (< (person-birth-utime cd) (person-birth-utime d))))
                  (setf level clevel d cd))))
          (setf (person-teknonym n) (teknonym (person-sex n) level (person-name d)))
          (values (1+ level) d))
        (values 1 n))))
    (descend tree)
    (values)))
