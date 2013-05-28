#lang racket
(require "../ftree/ftree.rkt")

(provide mk-pqueue pqueue? pq-empty? 
;         pq-consL pq-consR pq-hd+tlL pq-hdL pq-tlL pq-hd+tlR pq-hdR pq-tlR
;        pq-append
         pq-top+rest)

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

;(define empty-pq (mk-pqueue))
  
(define pqueue? ftree:pqueue?)

(define (pq-empty? pq) (and (pqueue? pq) (ft-empty? pq)))

;(define (pq-consL a pq)
;  (match-define (ftree:pqueue ∅ sz ⊕ FT cmp) pq)
;  (ftree:pqueue ∅ sz ⊕ (consL sz ⊕ a FT) cmp))
;
;(define (pq-consR a pq)
;  (match-define (ftree:pqueue ∅ sz ⊕ FT cmp) pq)
;  (ftree:pqueue ∅ sz ⊕ (consR sz ⊕ a FT) cmp))
;
;(define (pq-hd+tlL pq)
;  (match-define (ftree:pqueue ∅ sz ⊕ FT cmp) pq)
;  (if (FT-empty? FT)
;      (error 'pq-hd+tlL "empty queue")
;      (let-values ([(hd tl) (hd+tlL sz ⊕ FT)])
;        (values hd (ftree:pqueue ∅ sz ⊕ tl cmp)))))
;
;(define (pq-hdL pq)
;  (when (pq-empty? pq) (error 'pq-hdL "empty queue"))
;  (let-values ([(x xs) (pq-hd+tlL pq)]) x))
;
;(define (pq-tlL pq) 
;  (when (pq-empty? pq) (error 'pq-tlL "empty queue"))
;  (let-values ([(x xs) (pq-hd+tlL pq)]) xs))
;
;(define (pq-hd+tlR pq)
;  (match-define (ftree:pqueue ∅ sz ⊕ FT cmp) pq)
;  (if (FT-empty? FT) 
;      (error 'pq-hd+tlR "empty sequence")
;      (let-values ([(hd tl) (hd+tlR sz ⊕ FT)])
;        (values hd (ftree:pqueue ∅ sz ⊕ tl cmp)))))
;
;(define (pq-hdR pq)
;  (when (pq-empty? pq) (error 'pq-hdR "empty queue"))
;  (let-values ([(x xs) (pq-hd+tlR pq)]) x))
;
;(define (pq-tlR pq) 
;  (when (pq-empty? pq) (error 'pq-tlR "empty queue"))
;  (let-values ([(x xs) (pq-hd+tlR pq)]) xs))
;
;(define (pq-append pq1 pq2)
;  (cond [(pq-empty? pq1) pq2]
;        [(pq-empty? pq2) pq1]
;        [else 
;         (match* (pq1 pq2)
;           [((ftree:pqueue ∅ sz ⊕ FT1 cmp1) (ftree:pqueue _ _ _ FT2 _))
;            (ftree:pqueue ∅ sz ⊕ (FT-append sz ⊕ FT1 FT2) cmp1)])]))

(define (pq-top+rest pq)
  (match-define (ftree:pqueue _ sz ⊕ FT) pq)
  (match/values (FT-split-tree (λ (x) (equal? (sz FT) x)) pq∅ sz ⊕ FT)
    [(l x r) (values x (ftree:pqueue pq∅ sz ⊕ (FT-append sz ⊕ l r)))]))