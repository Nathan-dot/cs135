;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Cheat Sheet|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Self-Check Stuff:

;; (cond [(false exp)]) -> (cond ...)
;; (cond [true exp]) -> exp
;; (cond [else exp]) -> exp
;; (and false ...) -> false
;; (and true ...) -> true
;; (and) -> true
;; (or true ...) -> true
;; (or false ...) -> (or ...)
;; (or) -> false

;; a local variable "name" would become instances name_0, name_1, name_n and onwards
;; header variables will update before local instances created

;; *NOTE*: Constant "defines" evaluate when the function loads so you don't have to redo them
;; i.e. (make-posn 3 (+ 1 2)) won't require a simplification step when it's used in code

;; Key Examples:

;; (rest (rest (foldr cons '(0) '(1 2))))
;; (rest (rest '(1 2 0)))
;; (rest '(2 0))
;; '(0)

#|
(((lambda (x) (lambda (y) (+ x y))) 1) (+ 1 1))
((lambda (y) (+ 1 y)) (+ 1 1))
((lambda (y) (+ 1 y)) 2)
(+ 1 2)
3
|#


#|
(filter (lambda (x) true) (map even? (build-list 3 add1)))
(filter (lambda (x) true) (map even? (list 1 2 3)))
(filter (lambda (x) true) (list false true false))
(list false true false)
|#

;; filter, build-list, foldr, foldl, map, lambda

;; reversing a list
(define (myreverse lst)
  (foldl cons '() lst))

(myreverse (list 1 2 3))

;; Using Structs:
(define-struct student (teacher classmates classes))
;; a Student is a (make-student Str (listof Str) (listof Str))
;;   Requires: ....

(define stupid (make-student "Mr. P" (list "aaron" "help") (list "eng" "sci")))
(student-teacher stupid)
(student-classmates stupid)
(student? stupid)


#| Examples of Templates:

;; student-template: Student -> Any
(define (student-template s) 
  (cond [(ustd? s) (... (ustd-template s) ...)]
        [(gstd? s) (... (gstd-template s) ...)]))
|#


(define-struct node (key left right))


;; Actual Sample Problems:

;; reversing a tree from left to right 
(define (reverse-tree t)
  (cond [(empty? t) empty] 
        [(node? t)
         (make-node (node-key t) (reverse-tree (node-right t))
                    (reverse-tree (node-left t)))])) 

(reverse-tree (make-node 10 (make-node 5 (make-node 17 empty empty) empty)
                         (make-node 16 empty empty)))

;; given n, count how many smaller nodes there are in the tree
(define (count-smaller n t)
  (cond [(empty? t) 0]
        [(= n (node-key t)) 0]
        [(> n (node-key t)) (+ 1 (count-smaller n (node-left t))
                                (count-smaller n (node-right t)))]
        [else (count-smaller n (node-left t))]))  

(define example (make-node 5 (make-node 1 empty empty)
                           (make-node 7 (make-node 6 empty empty)
                                      (make-node 14 empty empty))))
(count-smaller 8 example)
(count-smaller 1 example)
(count-smaller 100 example)


;; producing a bst from a list
(define (bst-from-list nats)
  (cond [(= (length nats) 1) (make-node (first nats) empty empty)]
        [(> (first nats) (second nats)) (make-node (first nats) (bst-from-list (rest nats)) empty)]
        [else (make-node (first nats) empty (bst-from-list (rest nats)))]))

(bst-from-list (list 5 1 7 6 14))


 
(define-struct bst (value left right)) 

;; inserting element in bst in proper place
(define (bst-insert item bst-tree)
  (cond ((null? bst-tree) (make-bst item '() '())) 
        ((= (bst-value bst-tree) item) bst-tree)
        ((< item (bst-value bst-tree))
         (make-bst (bst-value bst-tree)
                   (bst-insert item (bst-left bst-tree))
                   (bst-right bst-tree)))
        (else (make-bst (bst-value bst-tree)
                        (bst-left bst-tree)
                        (bst-insert item (bst-right bst-tree))))))


(bst-insert 5 (make-bst 8 (make-bst 3 (make-bst 1 empty empty) empty) (make-bst 9 empty empty)))

;; counting leafs of a tree
(define (leaf-count tree)
  (cond [(empty? tree) 0]
        [(and (node? tree) (empty? (node-left tree)) (empty? (node-right tree))) 1]
        [(node? tree) (+ (leaf-count (node-left tree)) (leaf-count (node-right tree)))]))

;; calculating height of a binary tree
(define (tree-height t)
  (cond
    [(empty? t) 0]
    [else (add1 (max (tree-height (node-left t))
                     (tree-height (node-right t))))]))