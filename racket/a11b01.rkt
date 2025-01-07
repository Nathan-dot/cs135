;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a11b01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Nathan Kwon (20934403)
;; CS 135 Fall 2021
;; Assignment 11, Bonus 02
;; ***************************************************
(define vegetable-plot
  (list (list 'cauliflower (list 'broccoli))
        (list 'broccoli (list 'cauliflower 'kale))
        (list 'kale (list 'cabbage 'broccoli 'rapini 'turnip 'tatsoi))
        (list 'cabbage (list 'kale))
        (list 'rapini (list 'kale 'turnip 'tatsoi))
        (list 'turnip (list 'kale 'rapini 'tatsoi 'kohlrabi))
        (list 'tatsoi (list 'kale 'rapini 'turnip))
        (list 'kohlrabi (list 'turnip 'cucumber))
        (list 'cucumber (list 'kohlrabi 'watermelon 'zucchini))
        (list 'zucchini (list 'cucumber 'watermelon))
        (list 'watermelon (list 'cucumber 'zucchini 'cantaloupe))
        (list 'cantaloupe (list 'watermelon 'squash))
        (list 'squash (list 'cantaloupe 'pumpkin))
        (list 'pumpkin (list 'squash))
        (list 'eggplant (list 'potato 'pepper))
        (list 'potato (list 'eggplant 'pepper))
        (list 'pepper (list 'tomato 'potato 'eggplant))
        (list 'tomato (list 'pepper))))

(define g
  (list (list 'eggplant (list 'potato 'pepper 'square))
        (list 'potato (list 'eggplant 'pepper 'square))
        (list 'pepper (list 'tomato 'potato 'eggplant 'square 'apple))
        (list 'square (list 'eggplant 'potato 'pepper))))

(define random-plot
  (list (list 'cauliflower (list 'broccoli))
        (list 'broccoli (list 'cauliflower 'kale))
        (list 'kale (list 'cabbage 'broccoli 'rapini 'turnip 'tatsoi))
        (list 'cabbage (list 'kale))
        (list 'rapini (list 'kale 'turnip 'tatsoi))
        (list 'turnip (list 'kale 'rapini 'tatsoi 'kohlrabi))
        (list 'tatsoi (list 'kale 'rapini 'turnip))
        (list 'kohlrabi (list 'turnip 'cucumber))
        (list 'cucumber (list 'kohlrabi 'watermelon 'zucchini))
        (list 'zucchini (list 'cucumber 'watermelon))
        (list 'watermelon (list 'cucumber 'zucchini 'cantaloupe))
        (list 'cantaloupe (list 'watermelon 'squash))
        (list 'squash (list 'cantaloupe 'pumpkin))
        (list 'pumpkin (list 'squash))
        (list 'eggplant (list 'potato 'pepper))
        (list 'potato (list 'eggplant 'pepper))
        (list 'pepper (list 'tomato 'potato 'eggplant))
        (list 'tomato (list 'pepper))
        (list 'car (list 'airplane 'bus 'elephant 'train))
        (list 'elephant (list 'bus 'car 'train 'airplane 'dan))
        (list 'airplane (list 'bus 'train 'elephant 'car))
        (list 'bus (list 'airplane 'elephant 'car 'train))
        (list 'train (list 'bus 'elephant 'airplane 'car))
        (list 'dan (list 'elephant))))

;; (find-clique/faster k nodes) produces a list of [k] Node representing a clique of size
;;   [k] in the given graph [nodes] if one exists. Note that any clique of the given
;;   size may be produced. If no clique of size [k] exists in the graph, the function
;;   produces false.

;; Examples:
(check-expect (find-clique/faster 0 vegetable-plot) empty)

;; find-clique/faster: Nat UGraph -> (anyof (listof Node) false) 
(define (find-clique/faster k nodes)
  (cond [(= k 0) empty]
        [(empty? nodes) false]
        [else (find-clique/acc k (first (first nodes))
                 nodes (second (first nodes)) (list (first (first nodes))))])) 

;; Tests:
(check-expect (find-clique/faster 1 vegetable-plot) '(cauliflower))
(check-expect (find-clique/faster 5 empty) false)
(check-expect (find-clique/faster 2 vegetable-plot) '(broccoli cauliflower))
(check-expect (find-clique/faster 3 vegetable-plot) '(turnip rapini kale))
(check-expect (find-clique/faster 4 vegetable-plot) '(tatsoi turnip rapini kale))
(check-expect (find-clique/faster 5 vegetable-plot) false)
(check-expect (find-clique/faster 4 g) (list 'square 'pepper 'potato 'eggplant))
(check-expect (find-clique/faster 2 g) (list 'potato 'eggplant))
(check-expect (find-clique/faster 1 g) (list 'eggplant))
(check-expect (find-clique/faster 5 random-plot) (list 'train 'elephant 'bus 'airplane 'car))


;; (find-clique/list k orig g neighbours clique-so-far) produces false
;;    if the combinations between [orig] and [neighbours] cannot form a
;;    clique of desired length [k]. Note that cliques are a set of nodes
;;    in an undirected graph G such that every pair in the set has an
;;    undirected edge connecting them. If there is a clique in the
;;    given undirected graph [g], it will be built up recursively
;;    in [clique-so-far] and produced.

;; Examples:
(check-expect (find-clique/list 0 'C vegetable-plot (list 'D 'E 'F) empty) empty)
(check-expect (find-clique/list 1 'cauliflower vegetable-plot (list 'broccoli) empty) '(broccoli))
(check-expect (find-clique/list 2 'cauliflower vegetable-plot (list 'broccoli) empty) false)
(check-expect (find-clique/list 2 'broccoli vegetable-plot (list 'cauliflower 'kale) empty)
              false)

;; find-clique/list: Nat Node UGraph (listof Node) (listof Node) -> (anyof false (listof Node))
(define (find-clique/list k orig g neighbours clique-so-far)
  (cond [(= (length clique-so-far) k) 
         (cond [(clique? clique-so-far g) clique-so-far]
               [else false])]
        [(empty? neighbours) false]
        [else (find-clique/list k orig g (rest neighbours) 
                               (append (list (first neighbours)) clique-so-far))]))


;; (find-clique/acc k orig g neighbours clique-so-far) produces false
;;    if the combinations between [orig] and [neighbours] cannot form a
;;    clique of desired length [k]. Note that cliques are a set of nodes
;;    in an undirected graph G such that every pair in the set has an
;;    undirected edge connecting them. If there is a clique in the
;;    given undirected graph [g], it will be built up recursively
;;    in [clique-so-far] and produced.

;; Examples:
(check-expect (find-clique/acc 0 'C vegetable-plot (list 'D 'E 'F) empty) empty)
(check-expect (find-clique/acc 1 'cauliflower vegetable-plot (list 'broccoli) empty) '(broccoli))
(check-expect (find-clique/acc 2 'cauliflower vegetable-plot (list 'broccoli) empty)
              (list 'kale 'broccoli)) 
(check-expect (find-clique/acc 2 'broccoli vegetable-plot (list 'cauliflower 'kale) empty)
              (list 'kale 'broccoli))

;; find-clique/acc: Nat Node UGraph (listof Node) (listof Node) -> (anyof false (listof Node))
(define (find-clique/acc k orig g neighbours clique-so-far)
  (cond [(and (empty? neighbours) (empty? (rest g))) false] 
        [(empty? neighbours)
         (find-clique/acc k (first (first (rest g))) 
                          (rest g) (second (first (rest g))) (list (first (first (rest g)))))]  
        [(false? (find-clique/list k orig g neighbours clique-so-far))
         (find-clique/acc k orig g (rest neighbours) clique-so-far)]
        [else (find-clique/list k orig g neighbours clique-so-far)]))


;; (clique? nodes ugraph) produces true if [nodes] makes up a clique in the given
;;   [ugraph]. Otherwise, it produces false.

;; Examples:
(check-expect (clique? (list 'cauliflower) vegetable-plot) true)

;; clique? (listof Node) UGraph -> Bool
;; Requires:
;;   [nodes] cannot contain any duplicates.
;;   All elements in [nodes] must be in the [ugraph]
(define (clique? nodes ugraph)
  (cond [(< (length nodes) 2) true]
        [else (search-nodes nodes ugraph)]))

;; Tests:
(check-expect (clique? (list ) vegetable-plot) true)
(check-expect (clique? (list 'a 'b 'c) empty) false) 
(check-expect (clique? (list 'cauliflower 'broccoli) vegetable-plot) true)
(check-expect (clique? (list 'kale 'broccoli 'cauliflower) vegetable-plot) false)
(check-expect (clique? (list 'cauliflower) vegetable-plot) true)
(check-expect (clique? (list 'turnip 'rapini 'tatsoi) vegetable-plot) true)
(check-expect (clique? (list 'cauliflower) vegetable-plot) true)
(check-expect (clique? (list 'eggplant 'potato 'pepper) vegetable-plot) true)
(check-expect (clique? (list 'eggplant 'tomato' pepper) vegetable-plot) false)
(check-expect (clique? (list 'eggplant 'potato 'pepper 'square) g) true)
(check-expect (clique? (list 'car 'elephant 'airplane 'bus 'train) random-plot) true) 
(check-expect (clique? (list 'car 'elephant 'airplane 'bus 'train 'dan) random-plot) false)


;; (search-nodes nodes ugraph) produces true if [ugraph] has the given clique
;;   [nodes]. Otherwise, it produces false.

;; Examples:
(check-expect (search-nodes (list 'kale 'turnip 'eggplant) vegetable-plot) false)
(check-expect (search-nodes (list 'kale 'turnip 'rapini) vegetable-plot) true)
(check-expect (search-nodes (list 'fruit 'veggies 'bowl) (list (list 'fruit (list 'veggies 'bowl))
                                                 (list 'veggies (list 'fruit))
                                                 (list 'bowl (list 'fruit)))) false)
(check-expect (search-nodes (list 'pepper 'eggplant 'potato) vegetable-plot) true) 

;; search-nodes: (listof Node) UGraph -> Bool
;; Requires:
;;   [nodes] cannot contain any duplicates.
;;   All elements in [nodes] must be in the [ugraph]
(define (search-nodes nodes ugraph)
  (cond [(empty? nodes) true]
        [(member? (- (length nodes) 1) (map (lambda (x) 
         (cond [(symbol=? (first nodes) (first x))
                     (foldr + 0 (map (lambda (y) (cond [(member? y nodes) 1]
                                            [else 0])) (second x)))]
               [else 0])) ugraph)) (search-nodes (rest nodes) ugraph)]
        [else false]))  
