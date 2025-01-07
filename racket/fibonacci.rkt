;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname fibonacci) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; (fib n) Produce the n-th value in the Fibonacci sequence.
;; fib: Nat -> Nat

(define (fib/acc n val prev)
  (cond [(= n 0) prev] [else (fib/acc (- n 1) (+ val prev) val)]))

(define (fiba n)
  (fib/acc n 1 0))

(fiba 24601)