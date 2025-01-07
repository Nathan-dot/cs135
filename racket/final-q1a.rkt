;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname final-q1a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; map-with-index: (X Nat -> X) (listof X) -> (listof X)
(define (map-with-index pred list1)
  (local [(define (map-with-index/list pred list1 acc)
             (cond [(empty? list1) empty]
                   [else (cons (pred (first list1) acc)
                               (map-with-index/list pred (rest list1) (+ acc 1)))]))]
    (map-with-index/list pred list1 0)))


(check-expect (map-with-index + '(1 13 5 3)) '(1 14 7 6))