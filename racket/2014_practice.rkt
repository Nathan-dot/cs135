;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 2014_practice) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; interleave: (X X -> Bool) (listof X) (listof X) -> (listof X)
(define (interleave pred? lst1 lst2)
  (map (lambda (x y) (cond [(pred? x y) x]
                           [else y])) lst1 lst2))

(interleave < '(1 2 3) '(3 2 1))


(define (keep-divisors lst1 lst2)
  (interleave (lambda (x y) (cond [(= (remainder y x) 0) true]
                                  [else false])) lst1 lst2))

(keep-divisors '(2 3 4 5) '(10 10 10 10))


(define graph '((A (C D E))
                (B (E J))
                (C ())
                (D (F J))
                (E (K))
                (F (K H))
                (H ())
                (J (H))
                (K ())))

(define (sinks G)
  (filter (lambda (x) (cond [(empty? (second x)) true]
                            [else false])) G))

(sinks graph)

(define (in-neighbours n G)
  (second (first (filter (lambda (x) (cond [(symbol=? (first x) n) true]
                            [else false])) G))))

(define (sources G)
  (map (lambda (x) (first x)) (sinks G)))

(check-expect (sources graph) (list 'C 'H 'K))

(in-neighbours 'A graph)

