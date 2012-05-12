;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pairs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joel S. Williamson             ;;
;; A reimplementation of cons     ;;
;; using lambdas. This is largely ;;
;; taken from SICP.               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; my-empty: X -> 0
;; Purpose: my-empty consumes a value and returns 0.
;; my-value is a unique value that is only equal? to itself.
;; Examples: (equal? my-empty my-empty) => true
;;           (equal? my-empty (lambda (x) (0))) => false
(define my-empty (lambda (x) (0)))
(check-expect (equal? my-empty my-empty) #t)
(check-expect (equal? my-empty (lambda (x) (0))) #f)

;; my-empty?: X -> boolean
;; Purpose: my-empty? consumes a value and returns true if
;; the value is my-empty and false otherwise.
;; Examples: (my-empty? my-empty) => true
;;           (my-empty? 4) => false
(define (my-empty? n) (equal? n my-empty))
(check-expect (my-empty? my-empty) #t)
(check-expect (my-empty? 4) #f)

;; A mcons is a datatype that holds two values,
;; x and y. These values can be extracted using
;; my-first and my-rest. mcons is built using
;; (my-cons x y)

;; my-cons: X Y -> mcons
;; Purpose: my-cons consumes two values x,y and
;; produces a mcons containing x and y.
;; Example: (define pair (my-cons "foo" "bar))
;;          (my-first pair) => "foo"
;;          (my-rest pair) => "bar"
(define (my-cons x y)
  (lambda (n)
    (cond [(= 0 n) x]
          [(= 1 n) y]
          [else "Error -- Not a valid argument. Try using my-first/my-rest."])))

;; my-first: mcons -> X
;; Purpose: my-first consumes a mcons and produces
;; the value in the first field of the mcons.
(define (my-first mcons)
  (mcons 0))

;; my-rest: mcons -> Y
;; Purpose: my-rest consumes a mcons and produces
;; the value in the second field of the mcons.
(define (my-rest mcons)
  (mcons 1))

(define example-list (my-cons 1 (my-cons 2 (my-cons 3 my-empty))))
(check-expect (my-first example-list) 1)
(check-expect (my-first (my-rest (my-rest example-list))) 3)
(check-expect (my-empty? (my-rest (my-rest (my-rest example-list)))) #t)

