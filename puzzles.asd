
(in-package :asdf/user)

(asdf:defsystem :puzzles
  :description "Puzzle Solutions."
  :licence "MIT"
  :serial t
  :components ((:file "math") (:file "tree") (:file "etc"))
  :in-order-to ((asdf:test-op (asdf:test-op :puzzles/test))))

(asdf:defsystem :puzzles/test
  :description "Puzzles test suite."
  :licence "MIT"
  :depends-on (:puzzles :fiveam)
  :components ((:file "test-math") (:file "test-tree") (:file "test-etc"))
  :perform (asdf:test-op (o c) (uiop:symbol-call :5am :run! :puzzles)))
