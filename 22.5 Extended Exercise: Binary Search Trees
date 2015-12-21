(require 2htdp/abstraction)
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BinaryTree (short: BT) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

; exercise 308
;; Number BT -> Boolean
;; determines whether the given number occurs in given BT
(define (contains-bt? bt n)
  (cond
    [(no-info? bt) #f]
    [(and (no-info? (node-left bt))
          (no-info? (node-right bt)))
     (= n (node-ssn bt))]
    [else (or (= (node-ssn bt) n)
              (contains-bt? (node-left bt) n)
              (contains-bt? (node-right bt) n))]))

; exercise 309
;; BT Number -> Boolean/Symbol
;; produce the name of node if n is contained in given BT, otherwise false.
(define (search-bt bt n)
  (cond
    [(not (contains-bt? bt n)) #f]
    [(= n (node-ssn bt)) (node-name bt)]
    [else (if (contains-bt? (node-left bt) n)
              (search-bt (node-left bt) n)
              (search-bt (node-right bt) n))]))

; A binary search tree (short: BST) is a BT according to the following conditions: 
; - NONE is always a BST
; - (make-node ssn0 name0 L R) is a BST if
; { L is a BST,
;   R is a BST,
;   all ssn fields in L contain numbers that are smaller than ssn0,
;   all ssn fields in R contain numbers that are larger than ssn0.}

; exercise 310
;; BST -> List-of Number
;; consumes a BST and produces list of numbers by flatten the BST.
(define (inorder bst)
  (cond
    [(no-info? bst) '()]
    [(and (no-info? (node-left bst))
          (no-info? (node-right bst)))
     (list (node-ssn bst))]
    [else (append (inorder (node-left bst))
                  (list (node-ssn bst))
                  (inorder (node-right bst)))]))

; exercise 311
;; Number BST -> String
;; consumes a number n and a BST and produces the name of a node if the node contains the same ssn as n, otherwise NONE
(define (search-bst bst n)
  (cond
    [(no-info? bst) NONE]
    [(= (node-ssn bst) n) (node-name bst)]
    [(> (node-ssn bst) n) (search-bst (node-left bst) n)]
    [(< (node-ssn bst) n) (search-bst (node-right bst) n)]))
    
; exericse 312
;; BST Number Symbol -> BST
;; creates a node by given number and symbol and then inserts it into given BST.
(define (insert-bst bst n s)
  (cond
    [(contains-bst? bst n)
     (error (append (number->string n)
                    "already exists in the BST"))]
    [(no-info? bst) (make-node n s NONE NONE)]
    [(> (node-ssn bst) n)
     (make-node (node-ssn bst)
                (node-name bst)
                (insert-bst (node-left bst) n s)
                (node-right bst))]
    [(< (node-ssn bst) n)
     (make-node (node-ssn bst)
                (node-name bst)
                (node-left bst)
                (insert-bst (node-right bst) n s))]))
;; BSt Number -> Boolean
;; determines whether the given number occurs in given BST
(define (contains-bst? bst n)
  (cond
    [(no-info? bst) #f]
    [(= (node-ssn bst) n) #t]
    [(> (node-ssn bst) n)
     (contains-bst? (node-left bst) n)]
    [(< (node-ssn bst) n)
     (contains-bst? (node-right bst) n)]))
     
; exercise 313
;; [List-of [List Number Symbol]] -> BST
;; given a List-of [List Number Symbol]], produce a BST. 
; this output of the function is a little bit different from the given example in htdp
; I don't know how to fix it...
(define (create-bst-from-list lons)
  (local ((define (compare-ssn n1 n2)
            (< (first n1) (first n2)))
          (define (middle-index l)
            (if (odd? (length l))
                (/ (sub1 (length l)) 2)
                (sub1 (/ (length l) 2))))
          (define (part-of-list l end)
            (for/list ((x l) (i end))
              x))
          (define (rest-list l start)
            (cond
              [(or (empty? l) (= start (length l))) '()]
              [else (cons (list-ref l start)
                          (rest-list l (add1 start)))]))
          (define (construct-bst l)
            (cond
              [(empty? l) NONE]
              [(empty? (rest l)) (make-node (first (first l)) (second (first l)) NONE NONE)]
              [else (local ((define middle (middle-index l))
                            (define first-half (part-of-list l middle))
                            (define second-half (rest-list l (add1 middle))))
                      (make-node
                       (first (list-ref l middle))
                       (second (list-ref l middle))
                       (construct-bst first-half)
                       (construct-bst second-half)))])))
    (construct-bst (sort lons compare-ssn))))

; test example
(define a-bst
  (make-node 63 'saj
             (make-node 29 'asd
                        (make-node 15 'cds
                                   (make-node 10 'xcz NONE NONE)
                                   (make-node 24 'rtg NONE NONE))
                        NONE)
             (make-node 89 'fev (make-node 77 'fbv NONE NONE)
                        (make-node 95 '3rf
                                   NONE
                                   (make-node 99 'fdv NONE NONE)))))
(define sample '((99 o) (77 l) (24 i) (10 h) (95 g) (15 d) (89 c) (29 b) (63 a)))
