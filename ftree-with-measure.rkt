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


;; A Measure is a value representing some cached property of an ftree
;; examples include position, max/min element

;; A Digit is one of:
;; - (One   Measure a)
;; - (Two   Measure a b)
;; - (Three Measure a b c)
;; - (Four  Measure a b c d)
(struct One   (v a))
(struct Two   (v a b))
(struct Three (v a b c))
(struct Four  (v a b c d))

;; An FTREE is one of:
;; - Empty
;; - Digit
;; - (Deep Digit [FTREE Three] Digit)
(struct Empty ())
;; ft-empty? : indicates if given ftree is empty
(struct Deep (v left mid right))
#;(define (mk-deep left mid right)
  (Deep (+ (size left) (size mid) (size left)) left mid right))


;; An [FTreeof X] is an (ftree init op FTREE)
;; ∅ is the "measure" for empty-ft
;; (sz t) returns the "measure" for FTREE t
;; ⊕ is associative and combines "measures"
(struct ftree (∅ sz ⊕ tree))
;; (elem-sz x) returns the "measure" for element x
(define (mk-ftree ∅ elem-sz ⊕) 
  ;; sz : FTREE -> Measure
  (define (sz t)
    (match t
      [(Empty)           ∅]
      [(One   v _)       v]
      [(Two   v _ _)     v]
      [(Three v _ _)     v]
      [(Four  v _ _ _ _) v]
      [(Deep  v _ _ _)   v]
      [x       (elem-sz x)])) ; measure individual element according to elem-sz
  (ftree ∅ sz ⊕ (Empty)))
(define (ft-empty? ft) (Empty? (ftree-tree ft)))


;; ft-cons: insert new element on the left of given ftree
(define (ft-consL a ft)
  (match-define (ftree _ sz ⊕ t) ft)
  (define va (sz a))
  (match t
    [(Empty) (One va a)]
    [(One v b)   (Two (⊕ va v) a b)]
    [(Two v b c) (Three (⊕ va v) a b c)]
    [(Three v b c d) (Four (⊕ va v) a b c d)]
    [(Four _ b c d e)
     (Deep (Two (⊕ va (sz b)) a b) (Empty) (Three (⊕ (sz c) (sz d) (sz e))))]
    [(Deep v (One vb b) mid right)
     (Deep (⊕ va v) (Two (⊕ va vb) a b) mid right)]
    [(Deep v (Two vbc b c) mid right)
     (Deep (⊕ va v) (Three (⊕ va vbc) a b c) mid right)]
    [(Deep v (Three vbcd b c d) mid right)
     (Deep (⊕ va v) (Four (⊕ va vbcd) a b c d) mid right)]
    [(Deep v (Four vbcde b c d e) mid right)
     (Deep (⊕ va v) 
           (Two (⊕ va (sz b)) a b) 
           (ft-consL (Three (⊕ (sz c) (sz d) (sz e)) c d e) mid)
           right)]))

;; ft-consR: insert new element on the right of given ftree
(define (ft-consR ft a)
  (match-define (ftree _ sz ⊕ t) ft)
  (define va (sz a))
  (match t
    [(Empty) (One va a)]
    [(One vb b)
     (Deep (⊕ vb va) (One vb b) (Empty) (One va a))]
    [(Two v c b) (Three (⊕ v va) c b a)]
    [(Three v d c b) (Four (⊕ v va) d c b a)]
    [(Four v e d c b)
     (Deep (Three (⊕ (sz e) (sz d) (sz c))) (Empty) (Two (⊕ (sz b) va) b a))]
    [(Deep v left mid (One vb b))  
     (Deep (⊕ v va) left mid (Two (⊕ vb va) b a))]
    [(Deep v left mid (Two vbc c b))
     (Deep (⊕ v va) left mid (Three (⊕ vbc va) c b a))]
    [(Deep v left mid (Three vbcd d c b))
     (Deep (⊕ v va) left mid (Four (⊕ vbcd va) d c b a))]
    [(Deep v left mid (Four vbcde e d c b))
     (Deep (⊕ v va) 
           left 
           (ft-consR (Three (⊕ (sz e) (sz d) (sz c)) e d c) mid)
           (Two (⊕ (sz b) va) b a))]))


;; returns left element plus rest of ftree
;; ft-hd+tlL : [FTreeof X] -> (values x FTree)
(define (ft-hd+tlL ft)
  (match-define (ftree ∅ sz ⊕ t) ft)
  (define (hd+tl t)
    (match t
      [(Empty) (error 'viewL "empty tree")]
      [(One _ a) (values a (Empty))]
      [(Two _ a b) (values a (One (sz b) b))]
      [(Three _ a b c) (values a (Two (⊕ (sz b) (sz c)) b c))]
      [(Four _ a b c d) (values a (Three (⊕ (sz b) (sz c) (sz d)) b c d))]
      [(Deep _ (One _ a) mid right)        
       (define tree
         (if (ft-empty? mid)
             right
             (let-values ([(x xs) (hd+tl mid)])
               (Deep x xs right))))
       (values a tree)]
      [(Deep _ (Two _ a b) mid right)      
       (values a  (Deep (⊕ (sz b) (sz mid) (sz right)) (One (sz b) b) mid right))]
      [(Deep _ (Three _ a b c) mid right)  
       (values a (Deep (⊕ (sz b) (sz c) (sz mid) (sz right))
                       (Two (⊕ (sz b) (sz c)) b c) 
                       mid right))]
      [(Deep _ (Four _ a b c d) mid right) 
       (values a (Deep (⊕ (sz b) (sz c) (sz d) (sz e))
                       (Three (⊕ (sz b) (sz c) (sz d)) b c d)
                       mid right))]))
  (if (ft-empty? t) (Empty)
      (let-values ([(x xs) (hd+tl t)])
        (values x (ftree ∅ sz ⊕ xs)))))

(define (ft-hdL ft)
  (when (ft-empty? ft) (error 'ft-hdL "empty tree"))
  (define-values (x xs) (ft-hd+tlL ft))
  x)
(define (ft-tlL ft) 
  (when (ft-empty? ft) (error 'ft-tlL "empty tree"))
  (define-values (x xs) (ft-hd+tlL ft))
  xs)


;; returns right element plus rest of ftree
;; ft-hd+tlR : [FTreeof X] -> (values x FTree)
(define (ft-hd+tlR ft)
  (match-define (ftree ∅ sz ⊕ t) ft)
  (define (hd+tl t)
    (match t
      [(Empty) (error 'viewL "empty tree")]
      [(One _ a) (values a (Empty))]
      [(Two _ b a) (values a (One (sz b) b))]
      [(Three _ c d a) (values a (Two (⊕ (sz c) (sz b)) c b))]
      [(Four _ d c b a) (values a (Three (⊕ (sz d) (sz c) (sz b)) d c b))]
      [(Deep _ left mid (One _ a))        
       (define tltree
         (if (ft-empty? mid)
             left
             (let-values ([(x xs) (hd+tl mid)])
               (Deep left xs x))))
       (values a tltree)]
      [(Deep _ left mid (Two _ b a))      
       (values a (Deep (⊕ (sz left) (sz mid) (sz b))
                       left mid
                       (One (sz b) b)))]
      [(Deep _ left mid (Three _ c b a))
       (values a (Deep (⊕ (sz left) (sz mid) (sz c) (sz b))
                       left
                       mid
                       (Two (⊕ (sz c) (sz b)) c b)))]
      [(Deep _ left mid (Four _ d c b a))
       (values a (Deep (⊕ (sz left) (sz mid) (sz e) (sz d) (sz c) (sz b))
                       left
                       mid 
                       (Three (⊕ (sz d) (sz c) (sz b)) d c b)))]))
  (if (ft-empty? t) (Empty)
      (let-values ([(x xs) (hd+tl t)])
        (values x (ftree ∅ sz ⊕ xs)))))

(define (ft-hdR ft)
  (when (ft-empty? ft) (error 'ft-hdR "empty tree"))
  (define-values (x xs) (ft-hd+tlR ft))
  x)
(define (ft-tlR ft) 
  (when (ft-empty? ft) (error 'ft-tlR "empty tree"))
  (define-values (x xs) (ft-hd+tlR ft))
  xs)


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