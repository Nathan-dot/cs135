;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname cheat-sheet) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct node (key left right))

(define (reverse-tree t)
  (cond [(empty? t) empty] 
        [(node? t)
         (make-node (node-key t) (reverse-tree (node-right t))
                    (reverse-tree (node-left t)))])) 

(reverse-tree (make-node 10 (make-node 5 (make-node 17 empty empty) empty)
                         (make-node 16 empty empty)))


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

(define (bst-from-list nats)
  (cond [(= (length nats) 1) (make-node (first nats) empty empty)]
        [(> (first nats) (second nats)) (make-node (first nats) (bst-from-list (rest nats)) empty)]
        [else (make-node (first nats) empty (bst-from-list (rest nats)))]))

(bst-from-list (list 5 1 7 6 14))

(define (print-paths tree list1)
  (cond [(and (empty? (node-left tree)) (empty? (node-right tree))) list1] 
        [(and (empty? (node-left tree)) (node? (node-right tree)))
         (list list1 (print-paths (node-right tree) (cons (node-key tree) list1)))] 
        [(and (node? (node-left tree)) (empty? (node-right tree)))
          (print-paths (node-left tree) (cons (node-key tree) list1))]  
        [else (list (print-paths (node-left tree) (cons (node-key tree) list1))
                    (print-paths (node-right tree) (cons (node-key tree) list1)))]))    

(node-key example)
 
(print-paths example empty)

(define-struct bst (value left right))

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



