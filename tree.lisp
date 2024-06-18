;;; Tree based puzzles.

(defpackage #:puzzles/tree
  (:use #:cl)
  (:export
   #:teknonymize))
(in-package #:puzzles/tree)

;; Teknonyms

(defstruct person
  ;; A person in a family-tree.
  (birth-utime nil :type fixnum)
  (name nil :type string)
  ;; Input/output to `teknonymize`.
  (teknonym "" :type string)
  (sex nil :type (member #\m #\f))
  (children nil :type list))

(defun teknonym (sex level name)
  "Generate the teknonym for person `sex` and its descendant `level` and `name`."
  (declare (type sex (member #\f #\m))
           (fixnum level)
           (name string))
  (format nil "~v@{~A~:*~}~*~:[~;grand~]~:[father~;mother~] of ~A"
    (- level 2) "great-" (> level 1) (char= sex #\f) name))

(defun teknonymize (tree)
  "Add teknonyms to the family `tree`."
  (declare (type person tree))
  (labels
      ((descend (n)
	 (declare (type person tree))
	 (if (person-children n)
	     (let (level name)
	       (setf (values level name) (descend (first (person-children n))))
	       (loop
		 :with clevel = level :with cname = name
		 :for c :in (cdr (person-children n))
		 :do (setf (values clevel cname) (descend c))
		 :when (< level clevel)
		   :do (setf level clevel
			     name cname))
	       (setf (person-teknonym n) (teknonym (person-sex n) level name))
	       (values (1+ level) name))
	     (values 1 (person-name n)))))
    (descend tree)
    (values)))
