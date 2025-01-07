;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Templates) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Templates:

;; bt-template: BT -> Any
#|
(define (bt-template t)
  (cond [(empty? t) ...]
        [(node? t) (... (node-key t)
                        (bt-template (node-left t))
                        (bt-template (node-right t)))]))
|#

;; nest-lst-template: (nested-listof X) -> Any
#|
(define (nest-lst-template lst)
  (cond [(empty? lst) ...]
        [(list? (first lst)) (... (nest-lst-template (first lst)) ...
                                  (nest-lst-template (rest lst)) ...)]
        [else (... (first lst) ...
                   (nest-lst-template (rest lst)) ...)]))
|#
