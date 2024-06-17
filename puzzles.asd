(asdf:defsystem :puzzles
  :description "Puzzle Solutions."
  :licence "MIT"
  :serial t
  :components ((:file "math"))
  :in-order-to ((asdf:test-op (asdf:test-op :puzzles/test))))

(asdf:defsystem :puzzles/test
  :description "Puzzles test suite."
  :licence "MIT"
  :depends-on (:puzzles :fiveam)
  :components ((:file "test-math"))
  :perform (asdf:test-op (o c) (uiop:symbol-call :5am :run! :puzzles)))

(defun test-and-quit ()
  (uiop:quit (if (asdf:test-system :puzzles/test) 0 -1)))
