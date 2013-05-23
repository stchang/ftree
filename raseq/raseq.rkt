#lang racket
(require "../ftree/ftree.rkt")

(provide mk-raseq raseq? ra-empty? empty-ras
         ra-consL ra-consR ra-hd+tlL ra-hdL ra-tlL ra-hd+tlR ra-hdR ra-tlR
         ra-append
         ra-splitat ra-ref)
(struct ftree:raseq ftree ())

(define (mk-raseq)
  (match-define (ftree ∅ sz ⊕ FT) (mk-ftree 0 (λ _ 1) +))
  (ftree:raseq ∅ sz ⊕ FT))

(define empty-ras (mk-raseq))
  
(define raseq? ftree:raseq?)

(define (ra-empty? ras) (and (raseq? ras) (ft-empty? ras)))

(define (ra-consL a ras)
  (match-define (ftree:raseq ∅ sz ⊕ FT) ras)
  (ftree:raseq ∅ sz ⊕ (consL sz ⊕ a FT)))

(define (ra-consR a ras)
  (match-define (ftree:raseq ∅ sz ⊕ FT) ras)
  (ftree:raseq ∅ sz ⊕ (consR sz ⊕ a FT)))

(define (ra-hd+tlL ras)
  (match-define (ftree:raseq ∅ sz ⊕ FT) ras)
  (if (FT-empty? FT)
      (error 'ra-hd+tlL "empty sequence")
      (let-values ([(hd tl) (hd+tlL sz ⊕ FT)])
        (values hd (ftree:raseq ∅ sz ⊕ tl)))))

(define (ra-hdL ras)
  (when (ra-empty? ras) (error 'ra-hdL "empty sequence"))
  (let-values ([(x xs) (ra-hd+tlL ras)]) x))

(define (ra-tlL ras) 
  (when (ra-empty? ras) (error 'ra-tlL "empty sequence"))
  (let-values ([(x xs) (ra-hd+tlL ras)]) xs))

(define (ra-hd+tlR ras)
  (match-define (ftree:raseq ∅ sz ⊕ FT) ras)
  (if (FT-empty? FT) 
      (error 'ra-hd+tlR "empty sequence")
      (let-values ([(hd tl) (hd+tlR sz ⊕ FT)])
        (values hd (ftree:raseq ∅ sz ⊕ tl)))))

(define (ra-hdR ras)
  (when (ra-empty? ras) (error 'ra-hdR "empty sequence"))
  (let-values ([(x xs) (ra-hd+tlR ras)]) x))

(define (ra-tlR ras) 
  (when (ra-empty? ras) (error 'ra-tlR "empty sequence"))
  (let-values ([(x xs) (ra-hd+tlR ras)]) xs))

(define (ra-append ras1 ras2)
  (cond [(ra-empty? ras1) ras2]
        [(ra-empty? ras2) ras1]
        [else 
         (match* (ras1 ras2)
           [((ftree:raseq ∅ sz ⊕ FT1) (ftree:raseq _ _ _ FT2))
            (ftree:raseq ∅ sz ⊕ (FT-append sz ⊕ FT1 FT2))])]))

(define (ra-splitat i ras)
  (match/values (ft-split (λ (x) (< i x)) ras)
    [((ftree ∅l szl ⊕l FTl) (ftree ∅r szr ⊕r FTr))
     (values (ftree:raseq ∅l szl ⊕l FTl) (ftree:raseq ∅r szr ⊕r FTr))]))

(define (ra-ref ras i)
  (match-define (ftree:raseq ∅ sz ⊕ FT) ras)
  (match/values (FT-split-tree (λ (x) (< i x)) ∅ sz ⊕ FT)
    [(_ x _) x]))