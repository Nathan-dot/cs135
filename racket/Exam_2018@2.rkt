;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exam_2018@2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
(define c 299792458)

(define (kinetic-energy m v)
  (- (/ (* m (sqr c)) (sqrt (- 1 (/ (sqr v) (sqr c))))) (* m (sqr c))))

(kinetic-energy 10 15)

(define (pay-duty? duration goods alcohol? profession)
  (cond [(or (symbol=? profession 'ambassador) (symbol=? profession 'diplomat)) false]
        [(< duration 24) true]
        [(> goods 800) true]
        [(>= duration 48) false]
        [(>= goods 200) true]
        [alcohol? true]
        [else false]))

#|(define (pay-duty? duration goods alcohol? profession)
  (or (symbol=? profession 'ambassador)
      (symbol=? profession 'diplomat) (< duration 24)
      (and (>= duration 24) (< duration 48) (or (> goods 200) (alcohol?)))
      (and (>= duration 48) (> goods 800))))
|#

(define (point-location box-x box-y width height x y)
  (and (and (<= x (+ box-x width)) (>= x box-x)) (and (<= y (+ box-y height)) (>= y box-y))))

;; (mdist 3 7 4 6)
;; (+ (abs (- 3 4)) (abs (- 7 6))))
;; (+ (abs -1) (abs (- 7 6)))
;; 2

;; (mdist 6 13 4 12)
;; (+ (abs (- 6 4)) (abs (- 13 12)))
;; (+ (abs 2) (abs (- 13 12))
;; 3

;; (or (< 13 5) (> 2 3))
;; (or false (> 2 3))
;; (or (> 2 3))
;; false

;; (make-posn (posn-x (make-posn 7 3)) (posn-y (make-posn y2 (+ 3 a))))
;; (make-posn (posn-x (make-make-posn 7 3)) (posn-y (make-posn y2 (+ 3 a))))
;; (make-posn 7 (posn-y (make-posn y2 (+ 3 a))))
;; (make-make-posn 7 15)

#| (cond
[(symbol=? ’papaya ’bumblebee) 83]
[(>= x1 (+ x2 x2)) x2]
[else y1])
|#

;; (cond
;; [false 83]
;; [(>= x1 (+ x2 x2)) x2]
;; [else y1])

;; (cond
;; [(>= 13 (+ 4 4)) 4]
;; [else y1])

;; 12

(define (debounce lst)
  (cond [(empty? lst) empty]
        [(< (length lst) 2) lst]
        [(equal? (first lst) (second lst)) (debounce (cons (rest lst)))]
        [else (cons (first lst) (debounce (rest lst)))]))

(define (insert-at s1 s2 n)
  (cond [(and (> n 0) (< n (length (string->list s1)))) (string-append
                                          (list->string (first (insert-at/list s1 s2 n)))
                 (list->string (second (insert-at/list s1 s2 n)))
                 (list->string (third (insert-at/list s1 s2 n))))] 
        [else (string-append (list->string (first (insert-at/list s1 s2 n)))
                 (list->string (second (insert-at/list s1 s2 n))))])) 

(define (insert-at/list s1 s2 n)
  (cond [(= n 0) (list (string->list s2) (string->list s1))] 
        [(= n (length (string->list s1))) (list (string->list s1) (string->list s2))]
        [else (list (string->list (substring s1 0 n)) (string->list s2)
                          (string->list (substring s1 2 (length (string->list s1)))))]))      

(check-expect (insert-at "1234" "abc" 0) "abc1234")
(check-expect (insert-at "1234" "abc" 2) "12abc34")
(check-expect (insert-at "1234" "abc" 4) "1234abc")

