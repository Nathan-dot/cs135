;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a09q02-filter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Nathan Kwon (20934403)
;; CS 135 Fall 2021
;; Assignment 09, Problem 2
;; ***************************************************

(require "a09q02-provide.rkt")
(provide filter-nlst)

;; A nested list of X (nested-listof X) is one of:
;; * empty
;; * (cons (nested-listof X) (nested-listof X))
;; * (cons X (nested-listof X))
;; Requires: (not (list? X))

;; a.)
;; (filter-nlst pred? nlist) produces a nested list identical to
;;     [nlist] except it only contains the elements for which
;;     the given predicate [pred?] produces true.

;; Examples:
(check-expect (filter-nlst even?
                           (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
              (list (list 2 (list 2 4) 6 (list 8)) 10 12))
(check-expect (filter-nlst number?
                           (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
              (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))

;; filter-nlst: (X -> Bool) (nested-listof X) -> (nested-listof X)
(define (filter-nlst pred? lst)
  (cond [(empty? lst) empty]
    [(or (cons? (first lst)) (empty? (first lst)))
         (cons (filter-nlst pred? (first lst))
               (filter-nlst pred? (rest lst)))]
    [(pred? (first lst))
     (cons (first lst) (filter-nlst pred? (rest lst)))]
    [else (filter-nlst pred? (rest lst))]))

;; Tests:
(check-expect (filter-nlst empty? empty) empty)
(check-expect (filter-nlst symbol? empty) empty)
(check-expect (filter-nlst symbol?
                           (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
              (list (list empty empty)))        
(check-expect (filter-nlst odd?
                           (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
              (list 1 (list (list 3) 5 (list 7 9)) 11))
(check-expect (filter-nlst positive? (list -3 (list 102 99 89) 110 4/5 1 2.5 (list 5)))
              (list (list 102 99 89) 110 4/5 1 2.5 (list 5)))
(check-expect (filter-nlst char? (list #\c #\a #\b false 13 (list #\p)))
                           (list #\c #\a #\b (list #\p)))
(check-expect (filter-nlst number? (list (list (list (list (list
                                                            (list (list (list (list 1 2 3)))))))) 4))
              (list (list (list (list (list (list (list (list (list 1 2 3)))))))) 4))
(check-expect (filter-nlst (lambda (num)
                            (cond [(> num 5) true]
                                  [else false])) (list (list 6 1 2) (list 7 9 9) (list 10 99 1) 0 0))
              (list (list 6) (list 7 9 9) (list 10 99)))
