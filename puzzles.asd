
(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *files* '("math" "tree")))

(asdf:defsystem :puzzles
  :description "Puzzle Solutions."
  :licence "MIT"
  :serial t
  :components #.(loop :for f :in *files* :collect `(:file ,f))
  :in-order-to ((asdf:test-op (asdf:test-op :puzzles/test))))

(asdf:defsystem :puzzles/test
  :description "Puzzles test suite."
  :licence "MIT"
  :depends-on (:puzzles :fiveam)
  :components
  #.(loop
      :for f :in *files*
      :collect `(:file ,(concatenate 'string "test-" f)))
  :perform (asdf:test-op (o c) (uiop:symbol-call :5am :run! :puzzles)))
