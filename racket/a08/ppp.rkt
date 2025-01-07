;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ppp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Nathan Kwon (20934403)
;; CS 135 Fall 2021
;; Assignment 08, Problem 2
;; ***************************************************

;; A Binary Search Tree (BST) is one of:
;; * empty
;; * a Node

(define-struct node (key val left right))
;; A Node is a (make-node Nat Str BST BST)
;; Requires: key > every key in left BST
;;           key < every key in the right BST

(define sample-tree
  (make-node 6 "six"
             (make-node 4 "four"
                 (make-node 0 "zero" empty
                    (make-node 2 "two" empty empty))
                    empty)
             (make-node 10 "ten"
                (make-node 8 "eight" empty empty)
                (make-node 12 "twelve" empty empty))))

(define (rearrange-root tree)
  (cond [(empty? (node-left tree)) tree]
        [else (make-node (node-key (rearrange-root (node-left tree))) 
                         (node-val (rearrange-root (node-left tree)))
                         empty
                         (make-node (node-key tree) (node-val tree)
                                    empty (node-left tree)))]))
#|
(define (normal-path tree)
  (cond [(empty? (node-left tree)) tree]
        [else (make-node (node-key tree)
                         (node-val tree)
                         (normal-path (node-left tree)) empty)]))
|#


(rearrange-root sample-tree)