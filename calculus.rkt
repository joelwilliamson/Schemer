;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname calculus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joel S. Williamson  ;;
;; Calculus Operators  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; diff: (num -> num) num -> (num->num)
;; Purpose: diff consumes a function and a interval size,
;; and returns the derivative of that function, using 
;; the secant method of approximation.
;; Examples: (diff sqr 0.01) ~> (lambda (x) (* 2 x))
;;           (diff sqrt 0.01) ~> (lambda (x) (/ 0.5 (sqrt x)))
(define (diff f int)
  (lambda (x)
    (/ (- (f (+ x (/ int 2)))
          (f (- x (/ int 2))))
       int)))

(check-expect ((diff sqr 0.01) 4) 8)
(check-within ((diff sqrt 0.001) 4) 1/4 0.01)

;; int: (num -> num) num -> (num num -> num)
;; Purpose: int consumes a function defined for all numbers
;; and a precision and returns a function that performs a
;; Reimann sum over an interval.
;; Example: ((int (lambda (x) x) 0.01) 0 1) ~> 1/2
;;          ((int (lambda (x) (expt x)) 0.01) ~> (lambda (a b) (- (expt b) (expt a)))
(define (int f prec)
  (local
    [(define (rsum a b)
       (cond [(> a b) (+ (f b)
                         (rsum a (- b prec)))]
             [else (f a)]))]
    (lambda (a b)
      (rsum (min a b)
            (max a b)))))