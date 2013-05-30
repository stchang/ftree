#lang racket
(require "../ftree/ftree.rkt")

;; Interval Trees
;; - represents sets of intervals, where an interval is a pair of real numbers
;;   lo and hi where lo <= hi

(provide mk-itree itree? it-empty? 
         it-insert it-match it-search
         interval-low interval-high interval
         interval-intersect?)

(struct interval (low high))

(define (interval-intersect? i j)
  (and (<= (interval-low i) (interval-high j))
       (<= (interval-low j) (interval-high i))))

(struct it∅struct ())
(define it∅ (it∅struct))

(struct ftree:itree ftree ()
  #:methods gen:ft
  [(define (gen:mk it ∅ sz ⊕ FT) (ftree:itree ∅ sz ⊕ FT))])

(define (key⊕ x y) (if (eq? y it∅) x y))
(define (prio⊕ x y)
  (cond [(eq? x it∅) y]
        [(eq? y it∅) x]
        [else (max x y)]))
(define (mk-itree)
  (match-define (ftree ∅ sz ⊕ FT) 
    (mk-ftree 
     (interval it∅ it∅)
     (λ (i) i)
     (match-lambda** 
      [((interval lo1 hi1) (interval lo2 hi2))
       (interval (key⊕ lo1 lo2) (prio⊕ hi1 hi2))])))
  (ftree:itree ∅ sz ⊕ FT))

(define empty-it (mk-itree))

(define itree? ftree:itree?)

(define (it-empty? it) (and (itree? it) (ft-empty? it)))

(define (atleast k i) (match i [(interval _ hi) (<= k hi)]))
(define (greater k i) (match i [(interval lo _) (> lo k)]))

(define (it-insert lo hi it)
  (if (< hi lo) (error 'it-insert "interval high end can't be less than low end")
      (let-values 
        ([(l r) (ft-split (match-lambda 
                            [(interval xlo xhi) (>= xlo lo)]) 
                          it)])
        (ft-append l (ft-consL (interval lo hi) r)))))

(define (it-search it lo hi)
  (match-define (ftree ∅ sz ⊕ FT) it)
  (define-values (l x r) (FT-split-tree (curry atleast lo) ∅ sz ⊕ FT))
  (if (and (atleast lo (sz FT))
           (<= (interval-low x) hi))
      x
      #f))

(define (it-match it lo hi)
  (define (matches ft)
    (define ft-atleast-lo (ft-drop-until (curry atleast lo) ft))
    (if (ft-empty? ft-atleast-lo)
        null
        (let-values ([(hd tl) (ft-hd+tlL ft-atleast-lo)])
          (cons hd (matches tl)))))
  (define it-greater-hi (ft-take-until (curry greater hi) it))
  (matches it-greater-hi))