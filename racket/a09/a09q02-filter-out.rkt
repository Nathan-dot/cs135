;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a09q02-filter-out) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Nathan Kwon (20934403)
;; CS 135 Fall 2021
;; Assignment 09, Problem 2
;; ***************************************************

(require "a09q02-filter.rkt")

;; d.)
;; (filter-out pred? lst) produces a nested list identical to
;;     [lst] except it only contains the elements for which
;;     the given predicate [pred?] produces false.

;; Examples:
(check-expect (filter-out odd?
                            (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
              (list (list 2 (list 2 4) 6 (list 8)) 10 12))

;; filter-out: (X -> Bool) (nested-listof X) -> (nested-listof X)
(define (filter-out pred? lst)
  (filter-nlst (lambda (x) (not (pred? x))) lst))

;; Tests:
(check-expect (filter-out empty? empty) empty)
(check-expect (filter-out symbol? empty) empty)
(check-expect (filter-out symbol?
                           (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
              (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))        
(check-expect (filter-out odd?
                           (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
              (list (list 2 (list 2 4) 6 (list 8)) 10 12))
(check-expect (filter-out positive? (list -3 (list 102 99 89) 110 4/5 1 2.5 (list 5)))
              (list -3 empty empty)) 
(check-expect (filter-out char? (list #\c #\a #\b false 13 (list #\p)))
                           (list false 13 empty))
(check-expect (filter-out number? (list (list (list (list (list
                                                            (list (list (list (list 1 2 3)))))))) 4))
              (list (list (list (list (list (list (list (list empty)))))))))
(check-expect (filter-out (lambda (num)
                            (cond [(> num 5) true]
                                  [else false])) (list (list 6 1 2) (list 7 9 9) (list 10 99 1) 0 0))
              (list (list 1 2) empty (list 1) 0 0))
