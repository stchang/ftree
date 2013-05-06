#lang s-exp "../../NEU_Research/lazyprofile/lazy-profile.rkt"



; Finger trees, from Hinze and Paterson 2006 JFP paper

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
;; - (Deep Digit [FTreeof Node] Digit)

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
  (if (ft-empty? ft) (Single a)
      (match ft
        [(Single b)                      (Deep (One a) empty-ft (One b))]
        [(Deep (Four b c d e) mid right) (Deep (Two a b)
                                               (ft-cons (Node3 c d e) mid)
                                               right)]
        [(Deep (Three b c d) mid right)  (Deep (Four a b c d) mid right)]
        [(Deep (Two b c) mid right)      (Deep (Three a b c) mid right)]
        [(Deep (One b) mid right)        (Deep (Two a b) mid right)])))

;; ft-snoc: insert new element on the right of given ftree
(define (ft-snoc ft a)
  (if (ft-empty? ft) (Single a)
      (match ft
        [(Single b)                     (Deep (One b) empty-ft (One a))]
        [(Deep left mid (Four e d c b)) (Deep left
                                              ;; just use Three instead of Node3?
                                              (ft-snoc mid (Node3 e d c))
                                              (Two b a))]
        [(Deep left mid (Three d c b))  (Deep left mid (Four d c b a))]
        [(Deep left mid (Two c b))      (Deep left mid (Three c b a))]
        [(Deep left mid (One b))        (Deep left mid (Two b a))])))

;; An [FTreeViewLof X] is one of:
;; - empty-view
;; - (ViewL x [FTreeof X])

(define empty-view null)
(define view-empty? null?)
(struct ViewL (x tree)) ;; left view

;; viewL : [FTreeof X] -> [FTreeViewLof X]
(define (viewL ft)
  (if (ft-empty? ft) empty-view
      (match ft
        [(Single x)                      (ViewL x empty-ft)]
        [(Deep (One x) mid right)        (ViewL x (mk-deepL null mid right))]
        [(Deep (Two x y) mid right)      (ViewL x (mk-deepL (One y) mid right))]
        [(Deep (Three x y z) mid right)  (ViewL x (mk-deepL (Two y z) mid right))]
        [(Deep (Four w x y z) mid right) (ViewL w (mk-deepL (Three x y z) mid right))])))

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
(define (digit->ftreeL d)
  (match d
    [(One x)        (Single x)]
    [(Two x y)      (Deep (One x) empty-ft (One y))]
    [(Three x y z)  (Deep (Two x y) empty-ft (One z))]
    [(Four w x y z) (Deep (Three w x y) empty-ft (One z))]))

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

;; tests

;; Similar to build-list function
(define (build-queue size f)
  (let loop ([n size])
    (if (zero? n)
        empty-ft
        (let ([nsub1 (sub1 n)])
          (ft-cons (f nsub1) (loop nsub1))))))

;; add first X elements of queue
(let loop ([q (build-queue 1024 add1)] [n 0])
  (if (>= n 20)
      0 
      (let ([vl (viewL q)])
        (+ (ViewL-x vl) (loop (ViewL-tree vl) (add1 n))))))
