#lang racket
(require "../ftree/ftree.rkt")

(provide mk-raseq raseq? ra-empty? empty-ras
         ra-splitat ra-ref)

(struct ftree:raseq ftree ()
  #:methods gen:ft
  [(define (gen:mk ras ∅ sz ⊕ FT) (ftree:raseq ∅ sz ⊕ FT))])

(define (mk-raseq)
  (match-define (ftree ∅ sz ⊕ FT) (mk-ftree 0 (λ _ 1) +))
  (ftree:raseq ∅ sz ⊕ FT))

(define empty-ras (mk-raseq))
  
(define raseq? ftree:raseq?)

(define (ra-empty? ras) (and (raseq? ras) (ft-empty? ras)))

(define (ra-splitat i ras)
  (match/values (ft-split (λ (x) (< i x)) ras)
    [((ftree ∅l szl ⊕l FTl) (ftree ∅r szr ⊕r FTr))
     (values (ftree:raseq ∅l szl ⊕l FTl) (ftree:raseq ∅r szr ⊕r FTr))]))

(define (ra-ref ras i)
  (match-define (ftree:raseq ∅ sz ⊕ FT) ras)
  (match/values (FT-split-tree (λ (x) (< i x)) ∅ sz ⊕ FT)
    [(_ x _) x]))