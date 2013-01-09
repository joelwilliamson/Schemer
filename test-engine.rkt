#lang racket

(provide check-expect
         check-within)

;; check-expect: Any Any -> None
;; (check-expect expression expected-value)
;; Purpose: check-expect checks if expression evaluates to expected-value.
;; If it does, print "Test passed.", otherwise print a helpful error message.

;; Example: (check-expect (+ 10 2) 12) prints "Test passed."
;;          (check-expect (+ 10 3) 12) prints "Test failed: (+ 10 3) => 13   --- Expected: 12"
(define-syntax check-expect
  (syntax-rules ()
    [(check-expect expr expected)
     (cond [(equal? (eval  #'expr) expected)
            (begin (display  "Test passed.\n"))]
           [else (begin (display "Test failed: ")
                 (display  (syntax->datum #'expr))
                 (display " => ")
                 (display expr)
                 (display "  ---   Expected: ")
                 (display expected))])]))

;; check-within: Real Real Real[>=0] -> None
;; (check-within expression expected-value tolerance)
;; Purpose: check-within checks if expression evaluates to a value that lies within tolerance
;; of expected-value. If it does, print "Test passed.", otherwise print a helpful error message.

;; Example: (check-within (sqrt 2) 1.4 0.1) prints "Test passed."
;;          (check-within (sqrt 2) 1.4 0.001) prints "Test failed: (sqrt 2) => 1.4121... --- Expected to be within 0.001 of 1.4"
(define-syntax check-within
  (syntax-rules ()
    [(check-within expr expected tolerance)
     (cond [(<= (abs (- expr expected)) tolerance)
            (begin (display  "Test passed.\n"))]
           [else (begin (display "Test failed: ")
                 (display  (syntax->datum #'expr))
                 (display " => ")
                 (display expr)
                 (display "  ---   Expected to be within ")
                 (display tolerance)
                 (display " of ")
                 (display expected))])]))