;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a09q02-local) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Nathan Kwon (20934403)
;; CS 135 Fall 2021
;; Assignment 09, Problem 2
;; ***************************************************

(require "a09q02-filter.rkt")


;; b.)
;; (ruthless symbols-nlst) produces a version of [symbols-nlst]
;;    where all instances of 'ruth have been removed.

;; Examples:
(check-expect (ruthless '(rabbit (apple pluto (ruth blue) ruth) hello))
              '(rabbit (apple pluto (blue)) hello))
(check-expect (ruthless '(rabbit (apple pluto (blue) hello)))
              '(rabbit (apple pluto (blue) hello)))

;; ruthless: (nested-listof Sym) -> (nested-listof Sym)
(define (ruthless symbols-nlst)
  (local [;; (ruth? sym) produces false if the given [sym] is 'ruth.
          ;;    Otherwise, it produces true.
          ;; ruth?: Sym -> Bool
          (define (ruth? sym)
            (cond [(symbol=? sym 'ruth) false]
                  [else true]))]
    (filter-nlst ruth? symbols-nlst)))

;; Tests:
(check-expect (ruthless empty) empty)
(check-expect (ruthless (list 'ruth 'ruth 'ruth 'apple 'ruth ' blueberry))
              (list 'apple 'blueberry))
(check-expect (ruthless (list (list 'ruth) (list 'ruth) (list 'ruth)))
              (list empty empty empty))
(check-expect (ruthless (list (list 'ruth) (list 'ruth) (list 'ruth) (list 'rudy)))
              (list empty empty empty (list 'rudy)))
(check-expect (ruthless '(RUTH (RUTH RUTH (RUTH RUTH) RUTH) Ruth))
              '(RUTH (RUTH RUTH (RUTH RUTH) RUTH) Ruth))
(check-expect (ruthless (list (list (list (list (list (list (list (list 'Ruth 'apple)))))))))
              (list (list (list (list (list (list (list (list 'Ruth 'apple)))))))))
(check-expect (ruthless (list (list (list (list (list (list (list (list 'ruth 'apple)))))))))
              (list (list (list (list (list (list (list (list 'apple)))))))))
(check-expect (ruthless (list 'three 'four 'ruth (list 'seventeen 'ninety) (list 'ruth) 'nine))
              (list 'three 'four (list 'seventeen 'ninety) empty 'nine))


;; (supersize n num-nlst) produces a list identical to the given [num-nlst]
;;    except all the numbers less than n have been removed.

;; Examples:
(check-expect (supersize 4 (list 8 1 (list 2 6 3 4) 10 1))
              (list 8 (list 6 4) 10))

;; supersize: Num (nested-listof Num) -> (nested-listof Num)
(define (supersize n num-nlst)
  (local [;; (greater-or-equal? num) produces true if the given
          ;;   [num] is greater or equal to the given [n].
          ;;   Otherwise, it produces false.
          ;; greater-or-equal?: Num -> Bool
          (define (greater-or-equal? num) 
            (cond [(>= num n) true] 
                  [else false]))]
    (filter-nlst greater-or-equal? num-nlst)))

;; Tests:
(check-expect (supersize 10 empty) empty)
(check-expect (supersize 4 (list 4 4 (list 4 4 4 4) 4 10))
              (list 4 4 (list 4 4 4 4) 4 10))
(check-expect (supersize 1000 (list 90 89 12 1 0)) empty)
(check-expect (supersize 5 (list 1 (list 2 3) (list 9 10) 51))
              (list empty (list 9 10) 51))
(check-expect (supersize 1 (list 12 45 78 9 (list 4 12 9 8) (list 3 2)))
              (list 12 45 78 9 (list 4 12 9 8) (list 3 2)))
(check-expect (supersize 5 '(1 2 3 4 5)) '(5))
(check-expect (supersize 9000 (list (list (list (list (list (list (list (list 9000 9001 5)))))))))
              (list (list (list (list (list (list (list (list 9000 9001)))))))))
(check-expect (supersize 101 (list (list 102 99 101) (list 5 89 1000) (list 99 1 0)))
              (list (list 102 101) (list 1000) empty))
(check-expect (supersize 6 '(6 7 4 (4 3 2 -5 -1000) 10 11 (6 (2 3 5 10 9))))
              '(6 7 () 10 11 (6 (10 9))))
(check-expect (supersize 13 (list 15 12 8 99 67 13.5 13.1 169/13 185/13 120/90))
              (list 15 99 67 13.5 13.1 169/13 185/13))
(check-expect (supersize 0 '(-1 -2 -3.4 -1.23 0.0001)) '(0.0001))
                         

;; (keep-between num1 num2 num-nlst) produces a list identical to the given [num-nlst]
;;    except only the numbers between [num1] and [num2] inclusive are retained.

;; Examples:
(check-expect (keep-between 5 10 '(1 3 5 (7 9 10) (8 (3 4)) 8 15))
              '(5 (7 9 10) (8 ()) 8))
(check-expect (keep-between 10 5 '(1 3 5 (7 9 10) (8 (3 4)) 8 15))
              '(5 (7 9 10) (8 ()) 8))

;; keep-between: Num Num (nested-listof Num) -> (nested-listof Num)
(define (keep-between num1 num2 num-nlst)
  (local [;; (between? num) produces true if the given [num] is
          ;;    in-between [num1] and [num2]. Otherwise, it produces false.
          ;; between?: Num -> Bool
          (define (between? num)
            (cond [(and (>= num num1) (<= num num2)) true]
                  [(and (<= num num1) (>= num num2)) true]
                  [else false]))]
    (filter-nlst between? num-nlst)))

;; Tests:
(check-expect (keep-between 10 12 empty) empty)
(check-expect (keep-between 4 4 (list 4 4 (list 4 4 4 4) 4 10))
              (list 4 4 (list 4 4 4 4) 4))
(check-expect (keep-between 1000 999 (list 90 89 12 1 0)) empty)
(check-expect (keep-between 5 51 (list 1 (list 2 3) (list 9 10) 51))
              (list empty (list 9 10) 51))
(check-expect (keep-between 1 10000 (list 12 45 78 9 (list 4 12 9 8) (list 3 2)))
              (list 12 45 78 9 (list 4 12 9 8) (list 3 2)))
(check-expect (keep-between 9000 9001
                            (list (list (list (list (list (list (list (list 9000 9001 5)))))))))
              (list (list (list (list (list (list (list (list 9000 9001)))))))))
(check-expect (keep-between 1000 101 (list (list 102 99 101) (list 5 89 1000) (list 99 1 0)))
              (list (list 102 101) (list 1000) empty))
(check-expect (keep-between 13 15 (list 15 12 8 99 67 13.5 13.1 169/13 185/13 120/90))
              (list 15 13.5 13.1 169/13 185/13))
(check-expect (keep-between 13 13.1 (list 12 13.2 13.001 13.102 14 (list 15 17.8 12.9)))
              (list 13.001 empty))
(check-expect (keep-between 10/9 2/9 (list 3/9 13/9 8/9 0.3 (list 1 90/9 5.5 1/9) (list 4/9)))
              (list 3/9 8/9 0.3 (list 1) (list 4/9)))
(check-expect (keep-between 1 2 '(0 3)) empty)



               