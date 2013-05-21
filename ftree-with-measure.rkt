#lang racket

(provide empty-ft ft-empty? 
         ft-consL ft-consR 
         ft-hd+tlL ft-hd+tlR ft-hdL ft-tlL ft-hdR ft-tlR
         ft-split)

; 2-3 finger trees, from Hinze and Paterson 2006 JFP paper
;; implements functional, persistent sequences 
;; - amortized constant time access to either end
;; - log (in size of smaller piece) time concatentation and splitting

;; TODO: [o] = open, [x] = done
;; [o] 2013-05-15: in FTree, replace middle FTree of Node3's 
;;       with FTree of Three's
;; [o] 2013-05-17: mk-deepR/L create Deep structures with no size
;; [o] 2013-05-17: parameterize size and size combining ops


;; A Measure is a value representing a cached property of an ftree
;; examples include position, max/min element

;; A Digit is one of:
;; - (One   a)
;; - (Two   a b)
;; - (Three a b c)
;; - (Four  a b c d)
(struct One   (a))
(struct Two   (a b))
(struct Three (a b c))
(struct Four  (a b c d))
(define empty-digit null) ; sometimes needed as intermediate internal structure
(define digit-empty? null?)

;; An [FTREEof X] is one of:
;; - Empty
;; - (Single x)
;; - (Deep Digit [FTREEof Node] Digit)
(struct Empty ())
(define empty-ft (Empty))
(struct Single (x))
(struct Node (v x y z))
(struct Deep (v left mid right))
#;(define (mk-deep left mid right)
  (Deep (+ (size left) (size mid) (size left)) left mid right))


;; An [FTreeof X] is an (ftree init sz op [FTREEof X])
;; ∅ is the "measure" for an empty-ft
;; (sz t) returns the "measure" for FTREE t
;; ⊕ is associative and combines "measures"
(struct ftree (∅ sz ⊕ FT))
;; (elem-sz x) returns the "measure" for element x
(define (mk-ftree ∅ elem-sz ⊕) 
  ;; sz : FTREE -> Measure
  (define (sz t)
    (match t
      [(Empty)            ∅]
      [(Single x)     (sz x)]
      [(One a)        (sz a)]
      [(Two a b)      (⊕ (sz a) (sz b))]
      [(Three a b c)  (⊕ (sz a) (sz b) (sz c))]
      [(Four a b c d) (⊕ (sz a) (sz b) (sz c))]
      [(Node v _ _ _)      v]
      [(Deep v _ _ _)      v]
      [x       (elem-sz x)])) ; measure individual element according to elem-sz
  (ftree ∅ sz ⊕ empty-ft))
;; ft-empty? : indicates if given ftree is empty
(define (ft-empty? ft) (eq? ft empty-ft) #;(Empty? (ftree-tree ft)))


;; cons ----------------------------------------

;; ft-cons: insert new element on the left of given ftree
(define (ft-consL a ft)
  (match-define (ftree _ sz ⊕ FT) ft)
  (define va (sz a))
  (match FT
    [(Empty) (Single a)]
    [(Single b)                  (Deep (⊕ va (sz b)) (One a) empty-ft (One b))]
    [(Deep v (One b) mid right)       (Deep (⊕ va v) (Two a b) mid right)]
    [(Deep v (Two b c) mid right)     (Deep (⊕ va v) (Three a b c) mid right)]
    [(Deep v (Three b c d) mid right) (Deep (⊕ va v) (Four a b c d) mid right)]
    [(Deep v (Four b c d e) mid right)
     (Deep (⊕ va v)
           (Two a b) 
           (ft-consL (Node (⊕ (sz c) (sz d) (sz e)) c d e) mid)
           right)]))

;; ft-consR: insert new element on the right of given ftree
(define (ft-consR ft a)
  (match-define (ftree _ sz ⊕ FT) ft)
  (define va (sz a))
  (match FT
    [(Empty) (Single a)]
    [(Single b) (Deep (⊕ (sz b) va) (One b) empty-ft (One a))]
    [(Deep v left mid (One b))       (Deep (⊕ v va) left mid (Two b a))]
    [(Deep v left mid (Two c b))     (Deep (⊕ v va) left mid (Three c b a))]
    [(Deep v left mid (Three d c b)) (Deep (⊕ v va) left mid (Four d c b a))]
    [(Deep v left mid (Four e d c b))
     (Deep (⊕ v va) 
           left 
           (ft-consR (Node (⊕ (sz e) (sz d) (sz c)) e d c) mid)
           (Two b a))]))

;; hd+tlL ----------------------------------------
(define (digit-hdL d)
  (match d
    [(One a) a]
    [(Two a _) a]
    [(Three a _ _) a]
    [(Four a _ _ _) a]))
(define (digit-tlL d)
  (match d
    [(One _) empty-digit]
    [(Two _ a) (One a)]
    [(Three _ a b) (Two a b)]
    [(Four _ a b c) (Three a b c)]))
(define (digit-hd+tlL d)
  (match d
    [(One a) (values a empty-digit)]
    [(Two a b) (values a (One b))]
    [(Three a b c) (values a (Two b c))]
    [(Four a b c d) (values a (Three b c d))]))
(define (digit-consL a digit) ;; digit can't be Four
  (match digit
    [(One b) (Two a b)]
    [(Two b c) (Three a b c)]
    [(Three b c d) (Four a b c d)]))

;; converts digit to ftree
(define (digit->ftree dig sz)
  (if (digit-empty? dig) empty-ft
      (match dig
        [(One a)        (Single a)]
        [(Two a b)      (Deep (sz dig) (One a) empty-ft (One b))]
        [(Three a b c)  (Deep (sz dig) (Two a b) empty-ft (One c))]
        [(Four a b c d) (Deep (sz dig) (Two a b) empty-ft (Two c d))])))
  
(define (node->digit n) (match n [(Node _ x y z) (Three x y z)]))

;; constructs the appropriate Deep ftree, 
;; according to the first arg, which can be a Digit or null
(define (mk-deepL sz ⊕ maybe-empty-digit mid right)
  (if (digit-empty? maybe-empty-digit)
      (if (ft-empty? mid)
          (digit->ftree right)
          (let-values ([(hd tl) (hd+tlL mid sz ⊕)])
            (Deep (⊕ (sz hd) (sz tl) (sz right)) (node->digit hd) tl right)))
      (Deep maybe-empty-digit mid right)))

;; hd+tlL : [FTREEof X] -> (values X [FTREEof X])
(define (hd+tlL FT sz ⊕) ; FT is not empty
  (match FT
    [(Empty) (error 'hd+tlL "empty tree")]
    [(Single x) (values x empty-ft)]
    [(Deep _ left mid right) 
     (define-values (hd tl) (digit-hd+tlL left))
     (values hd (mk-deepL sz ⊕ tl mid right))]))
;    [(Deep _ (One a) mid right)        
;     (define newFT
;       (if (ft-empty? mid)
;           (digit->ftree right sz)
;           (let-values ([(hd tl) (hd+tlL mid sz ⊕)])
;             (Deep (⊕ (sz hd) (sz tl) (sz right)) (node->digit hd) tl right))))
;     (values a newFT)]
;    [(Deep _ (Two a b) mid right)      
;     (values a (Deep (⊕ (sz b) (sz mid) (sz right)) (One b) mid right))]
;    [(Deep _ (Three a b c) mid right)  
;     (values a (Deep (⊕ (sz b) (sz c) (sz mid) (sz right))
;                     (Two b c) mid right))]
;    [(Deep _ (Four a b c d) mid right) 
;     (values a (Deep (⊕ (sz b) (sz c) (sz d) (sz mid) (sz right))
;                     (Three b c d)
;                     mid right))]))

  ;; returns left element plus rest of ftree
;; ft-hd+tlL : [FTreeof X] -> (values x FTree)
(define (ft-hd+tlL ft)
  (match-define (ftree ∅ sz ⊕ FT) ft)
  (if (ft-empty? FT) 
      empty-ft
      (let-values ([(hd tl) (hd+tlL FT sz ⊕)])
        (values hd (ftree ∅ sz ⊕ tl)))))


(define (ft-hdL ft)
  (when (ft-empty? ft) (error 'ft-hdL "empty tree"))
  (let-values ([(x xs) (ft-hd+tlL ft)]) x))

(define (ft-tlL ft) 
  (when (ft-empty? ft) (error 'ft-tlL "empty tree"))
  (let-values ([(x xs) (ft-hd+tlL ft)]) xs))


;; hd+tlR ----------------------------------------

(define (digit-hdR d)
  (match d
    [(One a) a]
    [(Two _ a) a]
    [(Three _ _ a) a]
    [(Four _ _ _ a) a]))
(define (digit-tlR d)
  (match d
    [(One _) empty-digit]
    [(Two a _) (One a)]
    [(Three a b _) (Two a b)]
    [(Four a b c _) (Three a b c)]))
(define (digit-hd+tlR d)
  (match d
    [(One a) (values a empty-digit)]
    [(Two b a) (values a (One b))]
    [(Three c b a) (values a (Two c b))]
    [(Four d c b a) (values a (Three d c b))]))
(define (digit-consR a digit) ;; digit can't be Four
  (match digit
    [(One b) (Two b a)]
    [(Two c b) (Three c b a)]
    [(Three d c b) (Four d c b a)]))

;; constructs the appropriate Deep ftree, 
;; according to the first arg, which can be a Digit or null
(define (mk-deepR sz ⊕ left mid maybe-empty-digit)
  (if (digit-empty? maybe-empty-digit)
      (if (ft-empty? mid)
          (digit->ftree left)
          (let-values ([(hd tl) (hd+tlR mid sz ⊕)])
            (Deep (⊕ (sz left) (sz tl) (sz hd)) left tl (node->digit hd))))
      (Deep left mid maybe-empty-digit)))

;; hd+tlR : [FTREEof X] -> (values X [FTREEof X])
(define (hd+tlR FT sz ⊕) ; FT is not empty
  (match FT
    [(Empty) (error 'hd+tlR "empty tree")]
    [(Single x) (values x empty-ft)]
    [(Deep _ left mid right)
     (define-values (hd tl) (digit-hd+tlR right))
     (values hd (mk-deepR sz ⊕ left mid tl))]))
;    [(Deep _ left mid (One a))        
;     (define newFT
;       (if (ft-empty? mid)
;           (digit->ftree left sz)
;           (let-values ([(hd tl) (hd+tlR mid)])
;             (Deep (⊕ (sz left) (sz tl) (sz hd)) left tl (node->digit hd)))))
;     (values a newFT)]
;    [(Deep _ left mid (Two b a))      
;     (values a (Deep (⊕ (sz left) (sz mid) (sz b)) left mid (One b)))]
;    [(Deep _ left mid (Three c b a))
;     (values a (Deep (⊕ (sz left) (sz mid) (sz c) (sz b))
;                     left mid (Two c b)))]
;    [(Deep _ left mid (Four d c b a))
;     (values a (Deep (⊕ (sz left) (sz mid) (sz d) (sz c) (sz b))
;                     left mid (Three d c b)))]))

;; returns right element plus rest of ftree
;; ft-hd+tlR : [FTreeof X] -> (values x FTree)
(define (ft-hd+tlR ft)
  (match-define (ftree ∅ sz ⊕ FT) ft)
  (if (ft-empty? FT) 
      empty-ft
      (let-values ([(hd tl) (hd+tlR FT)])
        (values hd (ftree ∅ sz ⊕ tl)))))

(define (ft-hdR ft)
  (when (ft-empty? ft) (error 'ft-hdR "empty tree"))
  (let-values ([(x xs) (ft-hd+tlR ft)]) x))

(define (ft-tlR ft) 
  (when (ft-empty? ft) (error 'ft-tlR "empty tree"))
  (let-values ([(x xs) (ft-hd+tlR ft)]) xs))



;; concatenation ----------------------------------------

(define (ft-append ft1 ft2)
  (cond [(ft-empty? ft1) ft2]
        [(ft-empty? ft2) ft1]
        [else 
;         (match-define (ftree ∅1 sz1 ⊕1 FT1) ft1)
;         (match-define (ftree ∅2 sz2 ⊕2 FT2) ft2)
         (match* (ft1 ft2)
           [((ftree _ _ _ (Single a)) _) (ft-consL a ft2)]
           [(_ (ftree _ _ _ (Single a))) (ft-consR ft1 a)]
           [((ftree _ _ ⊕ (Deep v1 l1 m1 r1))
             (ftree _ _ _ (Deep v2 l2 m2 r2)))
            (Deep (⊕ v1 v2) l1 (ft-append (ft-consR m1 r1) (ft-consL l2 m2)) r2)])]))
  

;; splitting ----------------------------------------

;; [FTreeof X] -> (values [FTreeof X] [FTreeof X])
(define (ft-split p? ft)
  (if (ft-empty? ft) 
      (values ft ft)
      (match-let ([(ftree ∅ sz ⊕ FT) ft])
        (if (p? (sz FT))
            (let-values ([(l x r) (ft-split-tree p? ∅ sz ⊕ FT)])
              (values l (ft-consL x r)))
            (values ft (ftree ∅ sz ⊕ empty-ft))))))
  
;; splits non-empty FTREE
(define (ft-split-tree p? ∅ sz ⊕ FT)
  (match FT
    [(Single x) (values empty-ft x empty-ft)]
    [(Deep _ left mid right)
     (define vl (⊕ ∅ (sz left)))
     (define vlm (⊕ vl (sz mid)))
     (cond 
       [(p? vl) ;; split is somewhere in left
        (define-values (l x r) (split-digit p? ∅ sz ⊕ left))
        (values (digit->ftree l) x (mk-deepL sz ⊕ r mid right))]
       [(p? vlm) ;; split is somewhere in mid
        (define-values (ml mxs mr) (ft-split-tree p? vl sz ⊕ mid))
        (define-values (l x r) 
          (split-digit p? (⊕ vl (sz ml)) sz ⊕ (node->digit mxs)))
        (values (mk-deepR sz ⊕ left ml l) x (mk-deepL sz ⊕ r mr right))]
       [else ;; split is somewhere in right
        (define-values (l x r) (split-digit p? vlm sz ⊕ right))
        (values (mk-deepR left mid l) x (digit->ftree r))])]))

(define (split-digit p? ∅ sz ⊕ digit)
  (match digit
    [(One a) (values empty-digit a empty-digit)]
    [_
     (define-values (x xs) (digit-hd+tlL digit))
     (define vx (⊕ ∅ (sz x)))
     (if (p? vx)
         (values empty-digit x xs)
         (let-values ([(l y r) (split-digit p? vx sz ⊕ xs)])
           (values (digit-consL x l) y r)))]))