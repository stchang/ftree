#lang racket
(require "../ftree/ftree.rkt")

;; from Hinze and Paterson:
;; "ordered sequences [can be seen as an optimization] of, and subsume, priority
;;  queues, as we have immediate access to the smallest and the greatest element,
;;  and search trees, as we can partition ordered sequences in logarithmic time."

;; another difference from pqueues:
;; - oseqs store elements in sorted order, so hd and tl can grab both min and max
;; elements in constant time, but has to use a special os-insert function
;; - pqueues store elements in arbitrary order, using the "measure", ie sz,
;; to determine the order, so cons can be used to insert

;; INVARIANT: provided cmp function must not include equality, so < is ok but not <=
;; - this implementation assumes equality elements are at the beginning of the
;;   right part after a partition

;; TODO:
;; [o] 2013-05-29: remove striction on cmp so <= is ok
;; [o] 2013-05-29: implement eq? and eqv? variants

(provide mk-oseq oseq? os-empty? 
         os-partition os-insert os-delete-all 
         os-top os-remove-top os-bot os-remove-bot
         os-merge)


(struct os∅struct ())
(define os∅ (os∅struct))
;; cmp must not include equality, so < is ok but not <=
;; because partitioning always assumes the equality elements are on the right
(struct ftree:oseq ftree (cmp)
  #:methods gen:ft
  [(define (gen:mk os ∅ sz ⊕ FT) (ftree:oseq os∅ sz ⊕ FT (ftree:oseq-cmp os)))])

;; unlike pqueue, each node only stores the rightmost element, 
;; so ordering must be maintained internally
(define (mk-oseq cmp)
  (match-define (ftree _ sz ⊕ FT) 
    (mk-ftree 
     os∅ 
     (λ (x) x) 
     (λ (x y) (if (eq? y os∅) x y))))
  (ftree:oseq os∅ sz ⊕ FT cmp))

;(define empty-pq (mk-pqueue))

(define oseq? ftree:oseq?)

(define (os-empty? os) (and (oseq? os) (ft-empty? os)))

;; partition such that for all y in left partition (cmp y x) holds,
;; and for all z in right partition (cmp z x) does not hold
(define (os-partition x os) 
  (define cmp (ftree:oseq-cmp os))
  (ft-split (λ (y) (not (cmp y x))) os))

(define (os-insert x os)
  (define-values (os<x os>=x) (os-partition x os))
  (ft-append os<x (ft-consL x os>=x)))

(define (os-delete-all x os)
  (define-values (os<x os>=x) (os-partition x os))
  (define-values (os=x os>x) (ft-split (λ (y) (not (equal? y x))) os>=x))
;  (if (os-empty? os=x)
;      (let-values ([(os≠x os=x) (ft-split (λ (y) (not (equal? y x))) os<x)])
;        (ft-append os≠x os>x))
      (ft-append os<x os>x))
 
(define os-top ft-hdL)
(define os-remove-top ft-tlL)
(define os-bot ft-hdR)
(define os-remove-bot ft-tlR)

(define (os-merge os1 os2)
  (define (merge os1 os2)
    (cond [(ft-empty? os2) os1]
          [(ft-empty? os1) os2]
          [else
           (let*-values 
               ([(x xs) (ft-hd+tlL os2)]
                [(cmp) (ftree:oseq-cmp os2)]
                [(os1<=x os1>x) 
                 (ft-split (λ (y) (not (or (cmp y x) (equal? y x)))) os1)])
             (ft-append os1<=x (ft-consL x (merge xs os1>x))))]))
  (merge os1 os2))
;
;(define (pq-top+rest pq)
;  (match-define (ftree:pqueue _ sz ⊕ FT) pq)
;  (match/values (FT-split-tree (λ (x) (equal? (sz FT) x)) pq∅ sz ⊕ FT)
;    [(l x r) (values x (ftree:pqueue pq∅ sz ⊕ (FT-append sz ⊕ l r)))]))
;
;(define (pq-top pq)
;  (match-define (ftree:pqueue _ sz _ FT) pq)
;  (sz FT))
;
;(define (pq-rest pq)
;  (define-values (x rst) (pq-top+rest pq))
;  rst)