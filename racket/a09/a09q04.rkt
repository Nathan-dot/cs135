;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a09q04) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Nathan Kwon (20934403)
;; CS 135 Fall 2021
;; Assignment 09, Problem 4
;; ***************************************************

;; (integrate func intervals) consumes a function [func] and
;;   a positive number of intervals [intervals]. It produces
;;   a function g(a, b) that produces an approximation of the
;;   integral of f(x) between a and b using a trapezoidal
;;   Riemann sum.

;; Examples:
(check-expect ((integrate (lambda (x) (+ x 1)) 3) 0 3) 7.5)

;; integrate: (Num -> Num) Nat -> (Num Num -> Num)
(define (integrate func intervals)
  (lambda (a b)
    (local [;; (create-interval a b distance limit) does a trapezoidal Riemann
            ;;   sum to estimate the area under the given function from the interval
            ;;   [a] to the interval [b].
            ;; create-interval: Num Num Num Num -> Num
            (define (create-interval a b distance limit)
              (cond [(>= a limit) 0]
                    [else (+ (* (- b a) (/ (+ (func a) (func b)) 2))
                             (create-interval b (+ b distance) distance limit))]))]
      (create-interval a (+ a (/ (- b a) intervals)) (/ (- b a) intervals) b))))

;; Tests: (given from module.)
(define i_x^2+3 (integrate (lambda (x) (+ (sqr x) 3)) 2))
(check-expect (i_x^2+3 1 2) (+ 2.3125 3.0625))
(check-expect (i_x^2+3 3 7) (+ 40 80))

; Define an integration function where all values of f(x) are above the
;; x axis.  Repeat with different numbers of intervals; the first ones
;; can be easily calculated by hand.  One interval gives 4.5, two intervals
;; gives 4.375, four gives 4.34375 and 1000 gives 4.33333.
(define f_x^2+2 (lambda (x) (+ (sqr x) 2)))
(check-expect ((integrate f_x^2+2 1) 1 2) 4.5)
(check-expect ((integrate f_x^2+2 2) 1 2) (+ 1.8125 2.5625))
(check-expect ((integrate f_x^2+2 4) 1 2)
              (+ 0.8203125 0.9765625 1.1640625 1.3828125))
(check-within ((integrate f_x^2+2 1000) 1 2) 4.333333 1/10000)

;; All values of f(x) between a and b are below the x axis
(check-within ((integrate (lambda (x) (- (sqr x) 4)) 1000) -1 2) -9 1/10000)
;; Some values of f(x) above; some below
(check-within ((integrate (lambda (x) (* x x x)) 1000) -2 1)
              -3.75 1/10000)
              
;; For many functions it's possible to derive an equation to calculate the 
;; integral. The following check-expects make use of this:
(check-within ((integrate (lambda (x) (sin x)) 1000) 0 10)
              (- (- (cos 10)) (- (cos 0))) 0.0001)
(check-within ((integrate (lambda (x) (/ 1 x)) 1000) 1 10)
              (- (log 10) (log 1)) 0.0001)
