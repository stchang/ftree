#lang racket

(provide empty-ft ft-empty? ft-cons ft-headL ft-tailL viewL ViewL)

; 2-3 finger trees, from Hinze and Paterson 2006 JFP paper
;; implements functional, persistent sequences 
;; - amortized constant time access to either end
;; - log (in size of smaller piece) time concatentation and splitting

;; TODO: [o] = open, [x] = done
;; [o] 2013-05-15: in FTree, replace middle FTree of Node3's 
;;       with FTree of Three's
;; [o] 2013-05-17: mk-deepR/L create Deep structures with no size
;; [o] 2013-05-17: parameterize size and size combining ops


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
(struct Deep (size left tree right))

(define (mk-deep left mid right)
  (Deep (+ (size left) (size mid) (size left)) left mid right))

;; A Digit is one of:
;; - (One a)
;; - (Two a b)
;; - (Three a b c)
;; - (Four a b c d)

(struct One (a))
(struct Two (a b))
(struct Three (a b c))
(struct Four (a b c d))

;; A Node is one of:
;; - (Node2 x y)
;; - (Node3 x y z)

(struct Node2 (x y))
(struct Node3 (size x y z))

(define (mk-node3 x y z) (Node3 (+ (size x) (size y) (size z)) x y z))

(define (size x)
  (if (ft-empty? x) 0
      (match x
        [(Single x) (size x)]
        [(Deep sz _ _ _) sz]
        [(Node3 sz _ _ _) sz]
        [(One a) (size a)]
        [(Two a b) (+ (size a) (size b))]
        [(Three a b c) (+ (size a) (size b) (size c))]
        [(Four a b c d) (+ (size a) (size b) (size c) (size d))]
        [_ 1])))
            

;; ft-empty? : indicates if given ftree is empty
(define ft-empty? null?)

;; ft-cons: insert new element on the left of given ftree
(define (ft-cons a ft)
  (if (ft-empty? ft)                     
      (Single a)
      (match ft
        [(Single b)                     
         (Deep (+ (size a) (size b)) (One a) empty-ft (One b))]
        [(Deep sz (One b) mid right)  
         (Deep (+ (size a) sz) (Two a b) mid right)]
        [(Deep sz (Two b c) mid right)
         (Deep (+ (size a) sz) (Three a b c) mid right)]
        [(Deep sz (Three b c d) mid right)
         (Deep (+ (size a) sz) (Four a b c d) mid right)]
        [(Deep sz (Four b c d e) mid right)
         (Deep (+ (size a) sz) (Two a b) (ft-cons (mk-node3 c d e) mid) right)])))

;; ft-snoc: insert new element on the right of given ftree
(define (ft-snoc ft a)
  (if (ft-empty? ft)                    
      (Single a)
      (match ft
        [(Single b)  
         (Deep (+ (size a) (size b)) (One b) empty-ft (One a))]
        [(Deep sz left mid (One b))
         (Deep (+ (size a) sz) left mid (Two b a))]
        [(Deep sz left mid (Two c b))
         (Deep (+ (size a) sz) left mid (Three c b a))]
        [(Deep sz left mid (Three d c b))
         (Deep (+ (size a) sz) left mid (Four d c b a))]
        [(Deep sz left mid (Four e d c b))
         (Deep (+ (size a) sz) left (ft-snoc mid (mk-node3 e d c)) (Two b a))])))

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
        [(Deep _ (One a) mid right)        (ViewL a (mk-deepL null mid right))]
        [(Deep _ (Two a b) mid right)      (ViewL a (mk-deepL (One b) mid right))]
        [(Deep _ (Three a b c) mid right)  (ViewL a (mk-deepL (Two b c) mid right))]
        [(Deep _ (Four a b c d) mid right) (ViewL a (mk-deepL (Three b c d) mid right))])))

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

(define (node3->digit n) (match n [(Node3 _ x y z) (Three x y z)]))
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
        [(Deep _ left mid (One x))        (ViewR (mk-deepR left mid null) x)]
        [(Deep _ left mid (Two y x))      (ViewR (mk-deepR left mid (One y)) x)]
        [(Deep _ left mid (Three z y x))  (ViewR (mk-deepR left mid (Two z y)) x)]
        [(Deep _ left mid (Four z y x w)) (ViewR (mk-deepR left mid (Three z y x)) w)])))

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
           [((Deep sz1 left1 mid1 right1) (Deep sz2 left2 mid2 right2))
            (Deep (+ sz1 sz2) left1 
                  (ft-append (ft-snoc mid1 right1) (ft-cons left2 mid2))
                  right2)])]))
  

;; A [Splitof X] is a (Split X X)
(struct Split (left right))
(struct Split3 (left x right))

(define (ft-split p? ft)
  (cond
    [(ft-empty? ft) (Split empty-ft empty-ft)]
    [(p? (size ft))
     (match-define (Split3 l x r) (ft-split-tree p? 0 ft))
     (Split l (ft-cons r))]
    [else (ft empty-ft)]))
      
;; splits non-empty ftree
;; returns Split3
(define (ft-split-tree p? init ft)
  (match ft
    [(Single x) (Split empty-ft x empty-ft)]
    [(Deep _ left mid right)
     (let* ([left-size (+ init (size left))]
            [left+mid-size (+ left-size (size mid))])
       (cond 
         [(p? left-size)
          (match-define (Split3 l x r) (split-digit p? init left))
          (Split3 (digit->ftreeL l) x (mk-deepL r mid right))]
         [(p? left+mid-size)
          (match-define (Split3 ml mxs mr) (ft-split-tree p? left-size mid))
          (match-define (Split3 l x r) (split-digit p? (+ left-size (size ml)) (node3->digit mxs)))
          (Split3 (mk-deepR left ml l) x (mk-deepL r mr right))]
         [else
          (match-define (Split3 l x r) (split-digit p? left+mid-size right))
          (Split3 (mk-deepR left mid l) x (digit->ftreeR r))]
         ))]))

;; 
(define (digit-hd d)
  (match d
    [(One a) a]
    [(Two a _) a]
    [(Three a _ _) a]
    [(Four a _ _ _) a]))
(define (digit-tl d)
  (match d
    [(One _) null]
    [(Two _ a) (One a)]
    [(Three _ a b) (Two a b)]
    [(Four _ a b c) (Three a b c)]))
(define (digit-hd+tl d)
  (match d
    [(One a) (values a null)]
    [(Two a b) (values a (One b))]
    [(Three a b c) (values a (Two b c))]
    [(Four a b c d) (values a (Three b c d))]))
;; digit can't be Four
(define (digit-cons a digit)
  (match digit
    [(One b) (Two a b)]
    [(Two b c) (Three a b c)]
    [(Three b c d) (Four a b c d)]))
;; what is an "empty" digit? --- using null for now
(define (split-digit p? init digit)
  (match digit
    [(One a) (Split3 null a null)]
    [_
     (define-values (hd tl) (digit-hd+tl digit))
     (if (p? (+ init (size hd))) 
         (Split null hd tl)
         (match-let ([(Split3 l x r) (split-digit p? (+ init (size hd)) tl)])
           (Split3 (digit-cons hd l) x r)))]))