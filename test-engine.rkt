#lang racket

(provide check-expect
         check-within
         check-member-of)

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
            (begin (display  "Test passed.\n") #t)]
           [else (begin (display "Test failed: ")
                 (display  (syntax->datum #'expr))
                 (display " => ")
                 (display expr)
                 (display "  ---   Expected: ")
                 (display expected)
                 #t)])]))
