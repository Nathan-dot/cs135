;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a11q01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Nathan Kwon (20934403)
;; CS 135 Fall 2021
;; Assignment 11, Problem 1
;; ***************************************************

;; Data Definitions:

;; A Node is a Sym 
  
;; A Graph is one of: 
;;   empty 
;;   (cons (list v (list w_1 ... w_n)) g) where g is a Graph 

;; Requires: 
;;   v, w_1, ...., w_n are Nodes 
;;   v is the in-neighbour to w_1, ... , w_n in the Graph 
;;   v does not appear as an in-neighbour in g 

;; An undirected graph (UGraph) is a graph G such 
;; that for any two nodes u and v in G, if there 
;; is an edge from u to v, there is also an edge from 
;; v to u.

;; A directed graph (DGraph) is a graph G such that
;; for any two nodes u and v in G, if there is an edge
;; from u to v, there may or may not be an edge from
;; v to u. 

;; Constants:
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

(define test-graph1
  (list
   '(C (Z X Y J))
   '(Z ())
   '(X ())
   '(Y ())
   '(J ())
   '(F (G))
   '(G (F))))

(define test-graph2 
  '((B (C))
    (C (D E))
    (F (D))
    (D (C F))
    (E ())
    (G (D))
    (X (Y))
    (Y ())))

(define test-graph3
  '((Z (Y S M))
    (X (M J))
    (Y ())
    (S (Q J))
    (M (K))
    (Q (K U))
    (U ())
    (K ())
    (J (U))))

(define test-graph4
  '((Q (W E))
    (W ())
    (E (W))
    (R (W))
    (T (L))
    (L (P))
    (P (T))))

(define test-ugraph5
  '((A (B C D F))
    (B (A C E G))
    (C (A B D E))
    (D (A C E F G))
    (E (B C F G D))
    (F (A D E G))
    (G (B E D F))))

;; a.)
;; (directed->undirected dgraph) produces the corresponding
;;    undirected graph to the given [dgraph].

;; Examples:
(check-expect (directed->undirected (list (list 'A (list 'B)) (list 'B (list))))
              (list (list 'A (list 'B)) (list 'B (list 'A))))  

;; directed->undirected: DGraph -> UGraph
(define (directed->undirected dgraph)
  (map (lambda (x) (directed->undirected/list (first x) (second x) dgraph)) dgraph))

;; Tests:
(check-expect (directed->undirected empty) empty)
(check-expect (directed->undirected (list (list 'A (list 'B 'P 'D)) (list 'B (list)) (list 'P empty)
                                          (list 'D empty)))
              (list (list 'A (list 'B 'P 'D))
                    (list 'B (list 'A))
                    (list 'P (list 'A))
                    (list 'D (list 'A)))) 
(check-expect (directed->undirected (list (list 'cauliflower (list 'broccoli))
                            (list 'broccoli (list 'cauliflower))))
              (list (list 'cauliflower (list 'broccoli))
                            (list 'broccoli (list 'cauliflower))))
(check-expect (directed->undirected (list '(A ()))) (list '(A ())))
(check-expect (directed->undirected (list '(A (B)))) (list '(A (B))))
(check-expect (directed->undirected (list '(A ()) '(B ()))) (list '(A ()) '(B ())))
(check-expect (directed->undirected (list '(A (B)) '(B ()))) (list '(A (B)) '(B (A))))
(check-expect (directed->undirected (list '(A (B)) '(B (A)))) (list '(A (B)) '(B (A))))
(check-expect (directed->undirected test-graph1)
              (list
               '(C (Z X Y J))
               '(Z (C))
               '(X (C))
               '(Y (C))
               '(J (C))
               '(F (G))
               '(G (F))))
(check-expect (directed->undirected test-graph2)
              '((B (C))
                (C (B D E))
                (F (D))
                (D (G C F)) 
                (E (C))
                (G (D))
                (X (Y))
                (Y (X))))
(check-expect (directed->undirected test-graph3)
              '((Z (Y S M))
                (X (M J))
                (Y (Z))
                (S (Z Q J))
                (M (X Z K))
                (Q (S K U))
                (U (J Q))
                (K (Q M))
                (J (S X U))))
(check-expect (directed->undirected test-graph4)
              '((Q (W E))
                (W (R E Q)) 
                (E (Q W))
                (R (W))
                (T (P L))
                (L (T P))
                (P (L T))))


;; (directed->undirected/list name list1 dgraph) produces the partial undirected graph for
;;    a particular [name] in the given [dgraph]. This will essentially produce a list of all the
;;    undirected pathways for [name]. Note that the produced list can contain duplicates

;; Examples:
(check-expect (directed->undirected/list 'C '(Z X Y J) test-graph1)
              '(C (Z X Y J)))
(check-expect (directed->undirected/list 'Z '(Z X Y J) test-graph1)
              '(Z (C Z X Y J)))
(check-expect (directed->undirected/list 'B '(C) test-graph2)
              '(B (C)))
(check-expect (directed->undirected/list 'C empty test-graph3) (list 'C empty))

;; directed->undirected/list: Node (listof Node) DGraph -> (list Node (listof Node)) 
(define (directed->undirected/list name list1 dgraph)
  (cond [(empty? dgraph) (append (list name) (list (dedup list1)))]
        [(member? name (second (first dgraph)))
         (directed->undirected/list name (append (list (first (first dgraph))) list1) (rest dgraph))] 
        [else (directed->undirected/list name list1 (rest dgraph))]))


;; (dedup list1) produces the a version of [list1] where the duplicated elements have been removed.

;; Examples:
(check-expect (dedup empty) empty)
(check-expect (dedup (list true true false false true))
              (list false true))
(check-expect (dedup (list 4/7 5.5 4 4 -3.2 -3.2))
              (list 4/7 5.5 4 -3.2))
(check-expect (dedup (list true 'a 4 "string"))
              (list true 'a 4 "string"))
(check-expect (dedup (list #\x #\x -2.2 1.2 2.2 1.0 1))
              (list #\x -2.2 1.2 2.2 1))

;; dedup: (listof Any) -> (listof Any)
(define (dedup list1)
  (foldr (lambda (x y)
           (cond [(not (member? x y)) (cons x y)]
                 [else y])) empty list1))


;; b.)
;; (ugraph-remove node ugraph) produces the [ugraph] with the given node and its edges removed.
;;    If the given [node] doesn't exist in [ugraph], the graph will be produced unchanged.

;; Examples:
(check-expect (ugraph-remove 'B
                             (list (list 'A (list 'B)) (list 'B (list 'A 'C)) (list 'C (list 'B))))
              (list (list 'A empty) (list 'C empty))) 

;; ugraph-remove: Node UGraph -> UGraph
(define (ugraph-remove node ugraph)
  (cond [(empty? ugraph) ugraph]
        [else (edge-deleter node
                            (filter (lambda (x) (not (symbol=? (first x) node))) ugraph))]))

;; Tests:
(check-expect (ugraph-remove 'pepper vegetable-plot)
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
                    (list 'eggplant (list 'potato))
                    (list 'potato (list 'eggplant))
                    (list 'tomato (list )))) 
(check-expect (ugraph-remove 'cucumber vegetable-plot)
              (list (list 'cauliflower (list 'broccoli))
                    (list 'broccoli (list 'cauliflower 'kale))
                    (list 'kale (list 'cabbage 'broccoli 'rapini 'turnip 'tatsoi))
                    (list 'cabbage (list 'kale))
                    (list 'rapini (list 'kale 'turnip 'tatsoi))
                    (list 'turnip (list 'kale 'rapini 'tatsoi 'kohlrabi))
                    (list 'tatsoi (list 'kale 'rapini 'turnip))
                    (list 'kohlrabi (list 'turnip))
                    (list 'zucchini (list 'watermelon))
                    (list 'watermelon (list 'zucchini 'cantaloupe))
                    (list 'cantaloupe (list 'watermelon 'squash))
                    (list 'squash (list 'cantaloupe 'pumpkin))
                    (list 'pumpkin (list 'squash))
                    (list 'eggplant (list 'potato 'pepper))
                    (list 'potato (list 'eggplant 'pepper))
                    (list 'pepper (list 'tomato 'potato 'eggplant))
                    (list 'tomato (list 'pepper))))
(check-expect (ugraph-remove 'X
                             (list (list 'A (list 'B)) (list 'B (list 'A 'C)) (list 'C (list 'B))))
              (list (list 'A (list 'B)) (list 'B (list 'A 'C)) (list 'C (list 'B))))
(check-expect (ugraph-remove 'N empty) empty)
(check-expect (ugraph-remove 'A (list '(A ()))) empty)
(check-expect (ugraph-remove 'A (list '(A (B)))) empty)
(check-expect (ugraph-remove 'B (list '(A ()) '(B ()))) (list '(A ())))
(check-expect (ugraph-remove 'A (list '(A (B)) '(B (A)))) (list '(B ())))
(check-expect (ugraph-remove 'B (list '(A (B)) '(B (A)))) (list '(A ())))
(check-expect (ugraph-remove 'L
              (list
               '(C (Z X Y J))
               '(Z (C))
               '(X (C))
               '(Y (C))
               '(J (C))
               '(F (G))
               '(G (F))))
              (list
               '(C (Z X Y J))
               '(Z (C))
               '(X (C))
               '(Y (C))
               '(J (C))
               '(F (G))
               '(G (F))))
(check-expect (ugraph-remove 'D 
              '((B (C))
                (C (B D E))
                (F (D))
                (D (G C F)) 
                (E (C))
                (G (D))
                (X (Y))
                (Y (X))))
              '((B (C))
                (C (B E))
                (F ())
                (E (C))
                (G ())
                (X (Y))
                (Y (X))))
(check-expect (ugraph-remove 'Z 
              '((Z (Y S M))
                (X (M J)) 
                (Y (Z))
                (S (Z Q J))
                (M (X Z K))
                (Q (S K U))
                (U (J Q))
                (K (Q M))
                (J (S X U))))
              '((X (M J)) 
                (Y ())
                (S (Q J))
                (M (X K))
                (Q (S K U))
                (U (J Q))
                (K (Q M))
                (J (S X U))))
(check-expect (ugraph-remove 'P
              '((Q (W E))
                (W (R E Q)) 
                (E (Q W))
                (R (W))
                (T (P L))
                (L (T P))
                (P (L T))))
              '((Q (W E))
                (W (R E Q)) 
                (E (Q W))
                (R (W))
                (T (L))
                (L (T))))


;; (edge-deleter node ugraph) produces a version of [ugraph] with
;;    only the edges of the given [node] deleted. Note that if [node]
;;    is not in the given [ugraph]'s edges, the same [ugraph] is produced.
;;    Additionally, if 'B is the given [node] and [ugraph] has (list (list 'B (...)...)),
;;    it will not remove the (list 'B (...)...) component provided that the
;;    [node] does not appear in the ... parts.

;; Examples:
(check-expect (edge-deleter 'B (list (list 'A (list 'B)) (list 'B (list 'A 'C)) (list 'C (list 'B))))
              (list (list 'A empty) (list 'B (list 'A 'C)) (list 'C empty)))
(check-expect (edge-deleter 'B (list (list 'A (list 'D)) (list 'C (list 'B))
                                     (list 'B (list 'C)) (list 'D (list 'A))))
              (list (list 'A (list 'D)) (list 'C empty) (list 'B (list 'C)) (list 'D (list 'A))))
(check-expect (edge-deleter 'B test-graph1) test-graph1)
(check-expect (edge-deleter 'C empty) empty)

;; edge-deleter: Node UGraph -> UGraph 
(define (edge-deleter node ugraph)
  (map (lambda (x)
         (cond [(member? node (second x))
                (list (first x) (filter (lambda (y) (not (symbol=? y node))) (second x)))]  
               [else x])) ugraph))


;; c.)
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


;; d.)
;; (find-clique k nodes) produces a list of [k] Node representing a clique of size
;;   [k] in the given graph [nodes] if one exists. Note that any clique of the given
;;   size may be produced. If no clique of size [k] exists in the graph, the function
;;   produces false.

;; Examples:
(check-expect (find-clique 0 vegetable-plot) empty)

;; find-clique: Nat UGraph -> (anyof (listof Node) false) 
(define (find-clique k nodes)
  (cond [(= k 0) empty]
        [(empty? nodes) false]
        [else (find-clique/acc k (first (first nodes))
                 nodes (second (first nodes)) (list (first (first nodes))))])) 

;; Tests:
(check-expect (find-clique 1 vegetable-plot) '(cauliflower))
(check-expect (find-clique 5 empty) false)
(check-expect (find-clique 2 vegetable-plot) '(broccoli cauliflower))
(check-expect (find-clique 3 vegetable-plot) '(turnip rapini kale))
(check-expect (find-clique 4 vegetable-plot) '(tatsoi turnip rapini kale))
(check-expect (find-clique 5 vegetable-plot) false)
(check-expect (find-clique 4 g) (list 'square 'pepper 'potato 'eggplant))
(check-expect (find-clique 2 g) (list 'potato 'eggplant))
(check-expect (find-clique 1 g) (list 'eggplant))
(check-expect (find-clique 5 random-plot) (list 'train 'elephant 'bus 'airplane 'car))
(check-expect (find-clique 3 test-ugraph5) '(C B A))
(check-expect (find-clique 4 test-ugraph5) (list 'G 'F 'E 'D))


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


#|
Declarations
---------

I have used the following sources in my solution:

- Used the 7-vertex graph from ("Clique problem", 2021) as
  one of my examples.

Bibliography
------------

Clique problem. (2021, Oct 21). In Wikipedia.
https://en.wikipedia.org/w/index.php?title=Clique_problem&oldid=1049302087

Vishwas, S. (2019, August 19). Algorithm to find cliques of a given size K in o(n^k) time complexity.
OpenGenus IQ: Computing Expertise &amp; Legacy.
https://iq.opengenus.org/algorithm-to-find-cliques-of-a-given-size-k/.

Wrath of Math. (2020, Sep 10). What are k-Cliques? | Graph Theory, Cliques
[Video]. Youtube. https://www.youtube.com/watch?v=LqPHg9uNp-o
|#