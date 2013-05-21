#lang racket
(require rackunit)
(require "ftree-with-measure.rkt")

;; testing cons/hd/tl (ie using ftree as deque) -------------------------------

(define (build-dequeL size f)
  (let loop ([n size])
    (if (zero? n)
        empty-ft
        (let ([nsub1 (sub1 n)])
          (ft-consL (f nsub1) (loop nsub1))))))
(define (build-dequeR size f)
  (let loop ([n size])
    (if (zero? n)
        empty-ft
        (let ([nsub1 (sub1 n)])
          (ft-consR (f nsub1) (loop nsub1))))))


;; sums the m front, ie left, deque elements
(define (sumL m dq)
  (let loop ([n 0] [dq dq])
    (if (= n m) 0
        (+ (ft-hdL dq) (loop (add1 n) (ft-tlL dq))))))
;; sums the m rear, ie right, deque elements
(define (sumR m dq)
  (let loop ([n 0] [dq dq])
    (if (= n m) 0
        (+ (ft-hdR dq) (loop (add1 n) (ft-tlR dq))))))

(define size 1030)
(define dL (build-dequeL size (λ (x) x)))
(define dR (build-dequeR size (λ (x) x)))
(check-true
 (for/and ([thresh size])
   (= (sumR thresh dL)
      (for/sum ([n thresh]) n))))
(check-true
 (for/and ([thresh size])
   (= (sumL thresh dR)
      (for/sum ([n thresh]) n))))

;; alternative testing for cons/hd/tl -----------------------------------------
;; - use mk-ftree instead of empty-ft
;; - use hd+tl instead of separate hd and tl

(define (build-dequeL2 size f)
  (let loop ([n size])
    (if (zero? n)
        (mk-ftree 0 (λ _ 1) +)
        (let ([nsub1 (sub1 n)])
          (ft-consL (f nsub1) (loop nsub1))))))
(define (build-dequeR2 size f)
  (let loop ([n size])
    (if (zero? n)
        (mk-ftree 0 (λ _ 1) +)
        (let ([nsub1 (sub1 n)])
          (ft-consR (f nsub1) (loop nsub1))))))


;; sums the m front, ie left, deque elements
(define (sumL2 m dq)
  (let loop ([n 0] [dq dq])
    (if (= n m) 0
        (let-values ([(hd tl) (ft-hd+tlL dq)])
          (+ hd (loop (add1 n) tl))))))
;; sums the m rear, ie right, deque elements
(define (sumR2 m dq)
  (let loop ([n 0] [dq dq])
    (if (= n m) 0
        (let-values ([(hd tl) (ft-hd+tlR dq)])
          (+ hd (loop (add1 n) tl))))))

(define dL2 (build-dequeL2 size (λ (x) x)))
(define dR2 (build-dequeR2 size (λ (x) x)))
(check-true
 (for/and ([thresh size])
   (= (sumR2 thresh dL2)
      (for/sum ([n thresh]) n))))
(check-true
 (for/and ([thresh size])
   (= (sumL2 thresh dR2)
      (for/sum ([n thresh]) n))))

;; testing append ----------------------------------------

;(define size/2 size)
;(define d1 (build-deque size/2 (λ (x) x)))
;(define d2 (build-deque size/2 (λ (x) (+ x size/2))))
;(define dapp (append d1 d2))
;(define d1front (build-deque-front size/2 (λ (x) x)))
;(define d2front (build-deque-front size/2 (λ (x) (+ x size/2))))
;(define dappfront (append d2front d1front))
;(check-true
; (for/and ([thresh size])
;   (= (sum-front-x thresh dapp)
;      (sum-rear-x thresh dappfront)
;      (for/sum ([n thresh]) n))))
;
;
;(define (build-deque-with-append n f) ; n = size
;  (if (<= n 2)
;      (enqueue (f (sub1 n)) (enqueue (f (- n 2)) empty))
;      (let ([n/2 (/ n 2)])
;        (append (build-deque-with-append n/2 (λ (x) (f x)))
;                (build-deque-with-append n/2 (λ (x) (+ n/2 (f x))))))))
;(define size2 256)
;(define d-with-append1 (build-deque-with-append size2 (λ (x) x)))
;(define d-with-append2 (build-deque-with-append size2 (λ (x) (+ x size2))))
;(define d-with-append (append d-with-append1 d-with-append2))
;#;(let loop ([d d-with-append])
;  (unless (empty? d)
;    (displayln (head d))
;    (loop (tail d))))
;(check-true
; (for/and ([thresh (* 2 size2)])
;   (= (sum-front-x thresh d-with-append)
;      (for/sum ([n thresh]) n))))
;
;(define (log2 x) (/ (log x) (log 2)))
;;; -------- depth analysis --------
;;;- only way to get Deep structures is to append two Shallows of size >= 2
;;;- worst case nesting occurs when leaves are Shallows of size 2
;;;  (binary tree essentially)
;;;- thus, worst case DEEP depth = floor(log(size)/2), 
;;;        ie depth(n) = floor(n/2), where size = 2^n
;;;- appending two deques of size 2^n requires depth(n)+1 appends
;;;- in the lazy version of this degenerate binary deque,
;;;  since every 4 queue elements requires 1 thunk, 
;;;  a deque of size n requires n/4 thunks.
;
;
;;; -------- bottom line --------
;;; - In implicit or catenable or other similar deques, the "worst case"
;;;   operation is O(log n). Thus, when performing an expected constant-time 
;;;   operation in a non-lazy implementation, the worst case is O(log n).
;;;   Compare to banker's deque, where the worst case is O(n).
;;;   Thus, the question is if the overhead of laziness is > O(log n).
;
;;; testing persistence
;;(define n 10)
;;(define dq-persist1 (build-deque-with-append (expt 2 n) (λ (x) x)))
;;(define dq-persist2 (build-deque-with-append (expt 2 n) (λ (x) x)))
;;(time (append (build-deque-with-append (expt 2 n) (λ (x) x)) (build-deque-with-append (expt 2 n) (λ (x) x))))
;;(time (append dq-persist1 dq-persist2))
;;(time (append dq-persist1 dq-persist2))
;;(time (append dq-persist1 dq-persist2))
;
;;; n = 22
;;(time (build-deque-with-append (expt 2 22) values))
;;(time (build-deque-with-append (expt 2 22) values))
;;; cpu time: 1924 real time: 1929 gc time: 976
;;; cpu time: 2040 real time: 2047 gc time: 1104
;(define dq1 (build-deque-with-append (expt 2 9) values))
;(define dq2 (build-deque-with-append (expt 2 9) values))
;(time (for ([x (expt 2 10)]) (append dq1 dq2)))
;
;;; append counting; see also notes in deque-catenable-lazy-tests.rkt
;;; (define dq (build-deque-with-append (expt 2 10) values))
;;; 847 appends
;;; (define dq1 (build-deque-with-append (expt 2 9) values))
;;; (define dq2 (build-deque-with-append (expt 2 9) values))
;;; 412 appends
;;; (append dq1 dq2)
;;; 5 appends
;
;;; **********
;;; * Bottom line is, this is not a great data structure for testing persistence
;;; because the expensive operation only happens when appending deeply nested
;;; DEEP deques, but then most of the work is done during the create.
;;; I can't set up a nice situation like the banker's queue where an 
;;; *already created* queue has a pending expensive operation.
;
;(define args (current-command-line-arguments))
;(define limit-arg
;  (if (zero? (vector-length args)) 1
;      (string->number (vector-ref args 0))))
;
;;; persistence testing lazy vs strict with same code
;(time
; (let* ([n 19]
;        [dq1 (build-deque-with-append (expt 2 n) add1)] ; 421 appends
;        [dq2 (build-deque-with-append (expt 2 n) add1)] ; 421 appends
;;        [dq3 (append dq1 dq2)]                          ; 5 appends
;        [limit limit-arg])
;   (let loop ([count 0])
;     (unless (= count limit) ;; number of times to call tail
;       (tail (append dq1 dq2))    ;; 5 appends each time
;       (loop (add1 count))))))
;;; 842 + 5 on first run + 5*num repeats (n=9)
;;; so strict should catch up to lazy in (847-766)/(5-2) = 27 runs
;;; **reality**: strict catches up after repeating ~1 million times 
;;; (specifically between 2^18 and 2^19)