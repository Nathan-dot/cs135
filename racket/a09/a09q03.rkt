;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a09q03) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Nathan Kwon (20934403)
;; CS 135 Fall 2021
;; Assignment 09, Problem 3
;; ***************************************************

;; a.)
;; (make-adder n) adds [n] to another given value [m]
;;    and produces the sum.

;; Examples:
(check-expect ((make-adder 3) 4) 7)
(check-expect ((make-adder 3.9) 4.3) 8.2)

;; make-adder: Num -> Num
(define (make-adder n)
  (lambda (m) (+ m n)))

;; Tests:
(check-expect ((make-adder 5/7) 1/7) 6/7)
(check-expect ((make-adder 15) 190) 205)
(check-expect ((make-adder 3.5) 4) 7.5)


;; b.)
;; (make-quadratic c0 c1 c2) consumes three coefficients [c0], [c1], and [c2]
;;    and produces a quadratic function that consumes one argument [x-value] and produces
;;    the result of evaluating said quadratic. 

;; Examples:
(check-expect ((make-quadratic 4 3 2) 2) (+ 4 6 8))
(check-expect ((make-quadratic 0 0 1) 0) 0)
(check-expect ((make-quadratic 0 0 1) 5) 25)
(check-expect ((make-quadratic 0 0 1) -5) 25)

;; make-quadratic: Num Num Num -> Num
(define (make-quadratic c0 c1 c2)
  (lambda (x-value) (+ c0 (* x-value c1)
                       (* (expt x-value 2) c2))))

;; Tests:
(check-expect ((make-quadratic 3.2 6.7 9.9) 3) (+ 3.2 (* 6.7 3) (* 9.9 9)))
(check-expect ((make-quadratic 4/7 3/5 2/3) 2) (+ 4/7 (* 2 3/5) (* 4 2/3)))
(check-expect ((make-quadratic 21 14 18) 6) (+ 21 84 (* 36 18)))
(check-expect ((make-quadratic 0 0 0) 2) 0)
(check-expect ((make-quadratic -4 -3 -2) 2) (+ -4 -6 -8))
(check-expect ((make-quadratic -4 3 -2) -2) (+ -4 -6 -8))
(check-within ((make-quadratic 4 3 2) 2.5) (+ 4 7.5 (* (expt 2.5 2) 2)) 0.01)
(check-within ((make-quadratic 4 3 2) 14/5) (+ 4 (* 3 14/5) (* (expt 14/5 2) 2)) 0.01)
(check-expect ((make-quadratic 0 0 -87) 2) (* -87 4))
(check-expect ((make-quadratic -3/5 -2.2 15) 10) (+ -3/5 -22 1500))


;; c.)
;; (make-polynomial coefficient-list) consumes a non-empty list of coefficients [coefficient-list]
;;     and produces a polynomial function with the given coefficients in said list.

;; Examples:
(check-expect ((make-polynomial '(4)) 0) 4) 
(check-expect ((make-polynomial '(4)) 8888) 4) 
(check-expect ((make-polynomial '(1 2)) 0) 1)
(check-expect ((make-polynomial '(1 2)) 1) 3)
(check-expect ((make-polynomial '(1 2)) -2) -3)

;; make-polynomial: (listof Num) -> (Num -> Num)
;; Requires: [coefficient-list] must be non-empty.
(define (make-polynomial coefficient-list)
  (lambda (x-values)
    (local [;; (recurse-polynomial coefficients acc) produces a polynomial, whose
            ;;    coefficients are represented by [coefficients]. It uses a previous
            ;;    function's consumed [x-values] to evaluate the polynomial for [x-values].
            ;; recurse-polynomial: (listof Num) Nat -> Num
            (define (recurse-polynomial coefficients acc)
              (cond [(empty? coefficients) 0]
                    [else (+ (* (first coefficients) (expt x-values acc))
                             (recurse-polynomial (rest coefficients) (add1 acc)))]))]
      (recurse-polynomial coefficient-list 0))))

;; Tests:
(check-expect ((make-polynomial '(5 15 -3 -8 95)) 1) (+ 5 15 -3 -8 95))
(check-expect ((make-polynomial '(-12 -4 -3 -7 -1)) -2) 24)
(check-within ((make-polynomial '(3/4 2/5 12/7 1/3 -4/3 -5/7)) -1)
              (+ 3/4 -2/5 12/7 -1/3 -4/3 5/7) 0.01)
(check-expect ((make-polynomial '(-1.1 2.6 9.8 -0.2 3.4)) 2) 96.1)
(check-expect ((make-polynomial '(0 0 0 0 0 0 0 0 0 0 )) -124) 0)
(check-within ((make-polynomial '(1.1 -2 55 4/9)) 1) (+ 1.1 -2 55 4/9) 0.01)
(check-expect ((make-polynomial '(1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 1) 105)
(check-expect ((make-polynomial '(-1 2 -3 4 -5 6 -7 8 -9 10)) -2) -9217)
(check-expect ((make-polynomial '(1 1 1 1 1)) 4) 341)
(check-expect ((make-polynomial '(-1 -2)) 31) -63)
(check-expect ((make-polynomial '(1 2 19 8)) -215) -78629154)


;; d.)
;; (make-derivative coefficient-list) produces a function that consumes a number
;;   , x, and produces the derivative of that polynomial, whose coefficients are
;;   represented by [coefficient-list], evaluated at x.

;; Examples:
(check-expect ((make-derivative (list 1 2 3 4)) 0) ((make-polynomial (list 2 6 12)) 0))
(check-expect ((make-polynomial (list 2 6 12)) 5) ((make-polynomial (list 2 6 12)) 5)) 
(check-expect ((make-polynomial (list 2 6 12)) -5) ((make-polynomial (list 2 6 12)) -5))

;; make-derivative: (listof Num) -> (Num -> Num)
(define (make-derivative coefficient-list)
  (local [;; (derivative coefficients acc) produces the list of coefficients
          ;; that a polynomial's derivative will have.
          ;; derivative: (listof Num) Nat -> (listof Num)
          (define (derivative coefficients acc)
            (cond [(empty? coefficients) empty]
                  [(= acc 0) (derivative (rest coefficients) (add1 acc))]
                  [else (cons (* (first coefficients) acc)
                              (derivative (rest coefficients) (add1 acc)))]))]
    (make-polynomial (derivative coefficient-list 0))))

;; Tests:
(check-expect ((make-derivative (list -12 3 22)) 3) ((make-polynomial (list 3 44)) 3))  
(check-expect ((make-derivative (list -3 -7 -3/4)) 3) ((make-polynomial (list -7 -6/4)) 3))
(check-expect ((make-derivative (list -2.4 -4.4 -12)) 15) ((make-polynomial
                                                            (list -4.4 -24)) 15))
(check-expect ((make-derivative (list 0 0 0 0)) 12) ((make-polynomial (list 0 0 0)) 12))  
(check-expect ((make-derivative (list -44 12/5 2/3)) 1000) ((make-polynomial (list 12/5 4/3)) 1000))  
(check-expect ((make-derivative (list 1 12 19 8)) -215) ((make-polynomial (list 12 38 24)) -215)) 
(check-expect ((make-derivative (list 1 1 1 1 1 1 1 1 1 1)) 15)
              ((make-polynomial (list 1 2 3 4 5 6 7 8 9)) 15))
(check-expect ((make-derivative '(3/4 2/5 12/7 1/3 -4/3 -5/7)) -1)
              ((make-polynomial (list 2/5 24/7 1 -16/3 -25/7)) -1)) 
(check-expect ((make-derivative (list 5 15 -3 -8 95)) 1) ((make-polynomial (list 15 -6 -24 380)) 1))
(check-expect ((make-derivative (list -1820129758989237498728953 15 -3 -8 95)) 1)
              ((make-polynomial (list 15 -6 -24 380)) 1)) 









