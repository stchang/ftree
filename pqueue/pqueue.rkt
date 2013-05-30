#lang racket
(require "../ftree/ftree.rkt")

(provide mk-pqueue pqueue? pq-empty? 
         pq-top+rest pq-top pq-rest)

;; pq∅ is the "measure" of an empty pqueue
(struct pq∅struct ())
(define pq∅ (pq∅struct))

(struct ftree:pqueue ftree ()
  #:methods gen:ft
  [(define (gen:mk pq ∅ sz ⊕ FT) (ftree:pqueue pq∅ sz ⊕ FT))])

(define (mk-pqueue cmp)
  (match-define (ftree _ sz ⊕ FT) 
    (mk-ftree 
     pq∅ 
     (λ (x) x) 
     (λ (x y) 
       (cond [(eq? x pq∅) y]
             [(eq? y pq∅) x]
             [(cmp x y) x]
             [else y]))))
  (ftree:pqueue pq∅ sz ⊕ FT))
  
(define pqueue? ftree:pqueue?)

(define (pq-empty? pq) (and (pqueue? pq) (ft-empty? pq)))

(define (pq-top+rest pq)
  (match-define (ftree:pqueue _ sz ⊕ FT) pq)
  (match/values (FT-split-tree (λ (x) (equal? (sz FT) x)) pq∅ sz ⊕ FT)
    [(l x r) (values x (ftree:pqueue pq∅ sz ⊕ (FT-append sz ⊕ l r)))]))

(define (pq-top pq)
  (match-define (ftree:pqueue _ sz _ FT) pq)
  (sz FT))

(define (pq-rest pq)
  (define-values (x rst) (pq-top+rest pq))
  rst)