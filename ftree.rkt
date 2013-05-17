#lang racket

(provide empty-ft ft-empty? ft-cons ft-headL ft-tailL viewL ViewL)

; 2-3 finger trees, from Hinze and Paterson 2006 JFP paper
;; implements functional, persistent sequences 
;; - amortized constant time access to either end
;; - log (in size of smaller piece) time concatentation and splitting

;; TODO: [o] = open, [x] = done
;; [o] 2013-05-15: in FTree, replace middle FTree of Node3's 
;;       with FTree of Three's


;;; A [23Treeof X] is one of:
;;; - (Zero)
;;; - (Succ [23Treeof [Nodeof X]])
;
;(struct Zero ())
;(struct Succ (tree))
;
;;; A [Nodeof X] is one of:
;;; - (Node2 x x)
;;; - (Node3 x x x)
;(struct Node2 (x y))
;(struct Node3 (x y z))

;; ***** Supported operations: *****
;; - add/remove from either end: constant time

;; An [FTreeof X] is one of:
;; - empty-ft
;; - (Single x)
;; - (Deep Digit [FTreeof Node3] Digit)

(define empty-ft null)
(struct Single (x)) ;; analogous to Okasaki's Shallow
(struct Deep (left tree right))

;; A Digit is one of:
;; - (One x)
;; - (Two x y)
;; - (Three x y z)
;; - (Four w x y z)

(struct One (x))
(struct Two (x y))
(struct Three (x y z))
(struct Four (w x y z))

;; A Node is one of:
;; - (Node2 x y)
;; - (Node3 x y z)

(struct Node2 (x y))
(struct Node3 (x y z))


;; ft-empty? : indicates if given ftree is empty
(define ft-empty? null?)

;; ft-cons: insert new element on the left of given ftree
(define (ft-cons a ft)
  (if (ft-empty? ft)                     (Single a)
      (match ft
        [(Single b)                      (Deep (One a) empty-ft (One b))]
        [(Deep (One b) mid right)        (Deep (Two a b) mid right)]
        [(Deep (Two b c) mid right)      (Deep (Three a b c) mid right)]
        [(Deep (Three b c d) mid right)  (Deep (Four a b c d) mid right)]
        [(Deep (Four b c d e) mid right) (Deep (Two a b)
                                               (ft-cons (Node3 c d e) mid)
                                               right)])))

;; ft-snoc: insert new element on the right of given ftree
(define (ft-snoc ft a)
  (if (ft-empty? ft)                    (Single a)
      (match ft
        [(Single b)                     (Deep (One b) empty-ft (One a))]
        [(Deep left mid (One b))        (Deep left mid (Two b a))]
        [(Deep left mid (Two c b))      (Deep left mid (Three c b a))]
        [(Deep left mid (Three d c b))  (Deep left mid (Four d c b a))]
        [(Deep left mid (Four e d c b)) (Deep left
                                              ;; just use Three instead of Node3?
                                              (ft-snoc mid (Node3 e d c))
                                              (Two b a))])))

;; An [FTreeViewLof X] is one of:
;; - empty-view
;; - (ViewL x [FTreeof X])

(define empty-view null)
(define view-empty? null?)
(struct ViewL (x tree)) ;; left view

;; viewL : [FTreeof X] -> [FTreeViewLof X]
(define (viewL ft)
  (if (ft-empty? ft)                     empty-view
      (match ft
        [(Single a)                      (ViewL a empty-ft)]
        [(Deep (One a) mid right)        (ViewL a (mk-deepL null mid right))]
        [(Deep (Two a b) mid right)      (ViewL a (mk-deepL (One b) mid right))]
        [(Deep (Three a b c) mid right)  (ViewL a (mk-deepL (Two b c) mid right))]
        [(Deep (Four a b c d) mid right) (ViewL a (mk-deepL (Three b c d) mid right))])))

;; constructs the appropriate Deep ftree, 
;; according to the first arg, which can be a Digit or null
(define (mk-deepL digit-or-null mid right)
  (if (null? digit-or-null)
      (let ([vl (viewL mid)])
        (if (view-empty? vl)
            (digit->ftreeL right)
            (Deep (node3->digit (ViewL-x vl)) (ViewL-tree vl) right)))
      (Deep digit-or-null mid right)))

;; converts digit to ftree
(define (digit->ftreeL dig)
  (match dig
    [(One a)        (Single a)]
    [(Two a b)      (Deep (One a) empty-ft (One b))]
    [(Three a b c)  (Deep (Two a b) empty-ft (One c))]
    [(Four a b c d) (Deep (Three a b) empty-ft (One c d))]))

(define (node3->digit n) (match n [(Node3 x y z) (Three x y z)]))
(define (ft-headL ft) (ViewL-x (viewL ft)))
(define (ft-tailL ft) (ViewL-tree (viewL ft)))

;; An [FTreeViewRof X] is one of:
;; - empty-view
;; - (ViewR [FTreeof X] x)

(struct ViewR (tree x))

;; viewL : [FTreeof X] -> [FTreeViewRof X]
(define (viewR ft)
  (if (ft-empty? ft) empty-view
      (match ft
        [(Single x)                     (ViewR empty-ft x)]
        [(Deep left mid (One x))        (ViewR (mk-deepR left mid null) x)]
        [(Deep left mid (Two y x))      (ViewR (mk-deepR left mid (One y)) x)]
        [(Deep left mid (Three z y x))  (ViewR (mk-deepR left mid (Two z y)) x)]
        [(Deep left mid (Four z y x w)) (ViewR (mk-deepR left mid (Three z y x)) w)])))

;; constructs the appropriate Deep ftree, 
;; according to the first arg, which can be a Digit or null
(define (mk-deepR left mid digit-or-null)
  (if (null? digit-or-null)
      (let ([vr (viewR mid)])
        (if (view-empty? vr)
            (digit->ftreeR left)
            (Deep left (ViewR-tree vr) (One (ViewR-x vr)))))
      (Deep left mid digit-or-null)))

;; converts digit to ftree
(define (digit->ftreeR d)
  (match d
    [(One x)        (Single x)]
    [(Two y x)      (Deep (One y) empty-ft (One x))]
    [(Three z y x)  (Deep (One z) empty-ft (Two y x))]
    [(Four z y x w) (Deep (One z) empty-ft (Three y x w))]))

(define (ft-headR ft) (ViewR-x (viewR ft)))
(define (ft-tailR ft) (ViewR-tree (viewR ft)))


;; concatenation
(define (ft-append ft1 ft2)
  (cond [(ft-empty? ft1) ft2]
        [(ft-empty? ft2) ft1]
        [else 
         (match* (ft1 ft2)
           [((Single a) _) (ft-cons a ft2)]
           [(_ (Single a)) (ft-snoc ft1 a)]
           [((Deep left1 mid1 right1) (Deep left2 mid2 right2))
            (Deep left1 (ft-append (ft-snoc mid1 right1) (ft-cons left2 mid2)) right2)])]))
  

;; A [Splitof X] is a (Split X X)
(struct Split (left right))
(struct Split3 (left x right))


      