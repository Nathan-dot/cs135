;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exam_2018) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
(define (max-circles a r)
  (floor (/ (* a (sqrt 3)) (* 6 (expt r 2))))) 

(check-expect (floor 3.14) 3)
(check-expect (floor -4.21) -5)

;; f(x,y,z) = (x^y - 4^y) / (z^(1/2) + 1)



;; max-circles: Str Bool Nat Num Num -> Bool

(check-expect
    (attend-concert? "Drake" false 7 2000.00 100.00) false)

(check-expect
    (attend-concert? "Cher" true 5 40.00 100.00) true)

(define guaranteed-attendance "Metallica")
(define friend-budget 0.50)

(define (attend-concert? artist final-tour? friends-going ticket-price budget)
  (or (string=? artist guaranteed-attendance)
      (and (> friends-going 5) (<= ticket-price budget))
      (and (> friends-going 1) (<= ticket-price (* budget friend-budget)))
      (and final-tour? (<= ticket-price (* budget 0.20)))))

(define (attend-concert1? artist final-tour? friends-going ticket-price budget)
  (cond [(string=? artist guaranteed-attendance) true]
        [(> ticket-price budget) false]
        [(>= friends-going 5) true]
        [(> ticket-price (* budget friend-budget)) false]
        [(>= friends-going 1) true]
        [(> ticket-price (* budget 0.20)) false]
        [final-tour? true] 
        [else false]))

(check-expect
    (attend-concert1? "Drake" false 7 2000.00 100.00) false)

(check-expect
    (attend-concert1? "Cher" true 5 40.00 100.00) true)

(check-expect
    (attend-concert1? "Metallica" true 5 40.00 100.00) true)
(check-expect
    (attend-concert1? "asfjlasjflk" true 5 120.00 100.00) false)
(check-expect
    (attend-concert1? "asfjlasjflk" true 2 70.00 100.00) false)
(check-expect
    (attend-concert1? "asfjlasjflk" true 2 40.00 100.00) true)
(check-expect
    (attend-concert1? "asfjlasjflk" true 2 10.00 100.00) true)
(check-expect
    (attend-concert1? "asfjlasjflk" true 0 10.00 100.00) true)
(check-expect
    (attend-concert1? "asfjlasjflk" false 0 10.00 100.00) false)
(check-expect
    (attend-concert1? "asfjlasjflk" true 0 30.00 100.00) false)

(define (f x y z) (min (max x y) (- x z)))
(define (g x) (- x 3))
 
(f 3 7 2)
;; (min (max 3 7) (- 3 2))
;; (min 7 (-3 2))
;; (min 7 1)

(- (+ 7 (max 2 -4 3) 2 4) (max 3 -2 4))
;; (- (+ 7 3 2 4) (max 3 -2 4))
;; (- 16 (max 3 -2 4))
;; (- 16 4)

(g (g (g 4)))
;; (g (g (- 4 3)))
;; (g (g 1))
;; (g (- 1 3))

;; (cals-burned-skipping mass skips-per-min duration) produces
;;    the calories burned while skipping-rope. This value depends
;;    on [mass], [skips-per-min], and [duration] as they are multiplied
;;    together to produce a person's total calories burned.

;; Constants
(define less-than-100-spm 8.8)
(define between-100-120-spm 11.8)
(define greater-than-120-spm 12.3)
;; these are the MET values for a person who has
;; less than 100 skips per minute, between 100 and 120 skips per minute,
;; and greater than 120 skips per minute respectively.

(define (cals-burned-skipping mass skips-per-min duration)
  (cond [(< skips-per-min 100) (* less-than-100-spm duration mass)]
        [(and (>= skips-per-min 100) (<= skips-per-min 120))
         (* between-100-120-spm duration mass)]
        [(> skips-per-min 120) (* greater-than-120-spm duration mass)]))

;; Constants
(define lb-to-kg-conversion 0.45359237)
(define min-to-hr-conversion 60)

(define (cals-burned-skipping-friendly mass skips-per-min duration)
  (cals-burned-skipping (* mass lb-to-kg-conversion)
                        skips-per-min (/ duration min-to-hr-conversion)))
(define x 'carrot)


(define (blah a b c d e)
 (cond
 [(symbol=? a b)
 (cond [(> 3 d) (string-append "hot " e)]
 [(< 3 d) "lentils"])]
 [c (string-append "sour " e)]
 [(= d (string-length e)) (string-append "yummy " e)]
 [(or (= d 3) (not c)) "chilly"]
 [(symbol=? b x) (string-append "orange " e)]
 [d "radish"]))

(blah 'help 'help false 2 "goose")
(blah 'help 'scalp false 4 "tree")
(blah 'help 'scalp true 4 "phone")
(blah 'help 'scalp false 3 "alakazam")

;; hot goose
;; (blah 'help 'help false 2 goose)

;; yummy tree
;; (blah 'help 'scalp false 4 tree)

;; sour phone
;; (blah 'help 'scalp true 4 phone)

;; "chilly"
;; (blah 'help 'scalp false 3 alakazam)

;; symbol=? error
;; (blah 2 3 false 4 alakazam)

;; cond false error
;; (blah 'help 'help  false 3 "fake")

;; “cond: question result is not true or false”
;; (blah 'help 'help  null 3 "fake")

;; 9 and 10

;; blah: Sym Sym Bool Num Str -> Str
;; Requires: (or (symbol=? a b) (> d 3) (< d 3)) must be true


