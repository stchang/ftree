#lang racket
(require rackunit)
(require "../ftree/ftree.rkt")
(require "../pqueue/pqueue.rkt")

;; test generics
(check-true (pqueue? (ft-consL 1 (mk-pqueue <=))))

;; testing cons/hd/tl (ie using ftree as deque) -------------------------------

;; build-dequeL size: result is deque with elems size-1 ... 1
(define (build-pqueueL size f)
  (let loop ([n size])
    (if (zero? n)
        (mk-pqueue <=)
        (let ([nsub1 (sub1 n)])
          (ft-consL (f nsub1) (loop nsub1))))))
;; build-dequeR size: result is deque with elems 1 ... size-1
(define (build-pqueueR size f)
  (let loop ([n size])
    (if (zero? n)
        (mk-pqueue <=)
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
(define dqL (build-pqueueL size (λ (x) x)))
(define dqR (build-pqueueR size (λ (x) x)))
(check-true
 (for/and ([thresh size])
   (= (sumR thresh dqL)
      (for/sum ([n thresh]) n))))
(check-true
 (for/and ([thresh size])
   (= (sumL thresh dqR)
      (for/sum ([n thresh]) n))))

;; alternative testing for cons/hd/tl -----------------------------------------
;; - use mk-ftree instead of empty-ft
;; - use hd+tl instead of separate hd and tl

(define (build-pqueueL2 size f)
  (let loop ([n size])
    (if (zero? n)
        (mk-pqueue >=)
        (let ([nsub1 (sub1 n)])
          (ft-consL (f nsub1) (loop nsub1))))))
(define (build-pqueueR2 size f)
  (let loop ([n size])
    (if (zero? n)
        (mk-pqueue >=)
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

(define dqL2 (build-pqueueL2 size (λ (x) x)))
(define dqR2 (build-pqueueR2 size (λ (x) x)))
(check-true
 (for/and ([thresh size])
   (= (sumR2 thresh dqL2)
      (for/sum ([n thresh]) n))))
(check-true
 (for/and ([thresh size])
   (= (sumL2 thresh dqR2)
      (for/sum ([n thresh]) n))))

;; testing append ----------------------------------------

;; single append --------------------

(define size/2 (/ size 2))
(define dqL/2a (build-pqueueL size/2 (λ (x) x)))
(define dqL/2b (build-pqueueL size/2 (λ (x) (+ x size/2))))
(check-true
 (for/and ([thresh size/2])
   (= (sumR2 thresh dqL/2a)
      (for/sum ([n thresh]) n))))
(check-true
 (for/and ([thresh size/2])
   (= (sumR2 thresh dqL/2b)
      (for/sum ([n thresh]) (+ n size/2)))))
(define dqLapp (ft-append dqL/2b dqL/2a))
(check-true
 (for/and ([thresh size])
   (= (sumR thresh dqLapp)
      (for/sum ([n thresh]) n))))

(define dqR/2a (build-pqueueR size/2 (λ (x) x)))
(define dqR/2b (build-pqueueR size/2 (λ (x) (+ x size/2))))
(check-true
 (for/and ([thresh size/2])
   (= (sumL2 thresh dqR/2a)
      (for/sum ([n thresh]) n))))
(check-true
 (for/and ([thresh size/2])
   (= (sumL2 thresh dqR/2b)
      (for/sum ([n thresh]) (+ n size/2)))))
(define dqRapp (ft-append dqR/2a dqR/2b))
(check-true
 (for/and ([thresh size])
   (= (sumL thresh dqRapp)
      (for/sum ([n thresh]) n))))

;; multi-append --------------------
(define (build-pqueue/appendL n f) ; n = size, must be power of 2
  (if (<= n 2)
      (ft-consL (f (sub1 n)) (ft-consL (f (- n 2)) (mk-pqueue <=)))
      (let ([n/2 (/ n 2)])
        (ft-append 
         (build-pqueue/appendL n/2 (λ (x) (+ n/2 (f x))))
         (build-pqueue/appendL n/2 (λ (x) (f x)))))))
(define sizepow2 512)
(define dqL/append1 (build-pqueue/appendL sizepow2 (λ (x) x)))
(define dqL/append2 (build-pqueue/appendL sizepow2 (λ (x) (+ x sizepow2))))
(define dqL/append (ft-append dqL/append2 dqL/append1))
(check-true
 (for/and ([thresh (* 2 sizepow2)])
   (= (sumR thresh dqL/append)
      (for/sum ([n thresh]) n))))

(define (build-pqueue/appendR n f) ; n = size, must be power of 2
  (if (<= n 2)
      (ft-consR (f (sub1 n)) (ft-consR (f (- n 2)) (mk-pqueue <=)))
      (let ([n/2 (/ n 2)])
        (ft-append 
         (build-pqueue/appendR n/2 (λ (x) (f x)))
         (build-pqueue/appendR n/2 (λ (x) (+ n/2 (f x))))))))
(define dqR/append1 (build-pqueue/appendR sizepow2 (λ (x) x)))
(define dqR/append2 (build-pqueue/appendR sizepow2 (λ (x) (+ x sizepow2))))
(define dqR/append (ft-append dqR/append1 dqR/append2))
(check-true
 (for/and ([thresh (* 2 sizepow2)])
   (= (sumL thresh dqR/append)
      (for/sum ([n thresh]) n))))

;; testing split ----------------------------------------
;; i is index position
(let loop ([i 0] [pq dqL2] [pq2 dqL2]) ; pq uses top+rest, while pq2 uses them separately
  (let-values ([(x pqrst) (pq-top+rest pq)])
;     (printf "i = ~a, hd = ~a (- size i 1) = ~a\n" i (ft-hdL ft2) (- size i 1))))
    (unless (= i (sub1 size))
      (check-true (= x (pq-top pq) (pq-top pq2)))
      (check-true (= x (- size i 1)))
      (loop (add1 i) pqrst (pq-rest pq2)))))

(let loop ([i 0] [pq dqR2] [pq2 dqR2])
  (let-values ([(x pqrst) (pq-top+rest pq)])
;     (printf "i = ~a, hd = ~a (- size i 1) = ~a\n" i (ft-hdL ft2) (- size i 1))))
    (unless (= i (sub1 size))
      (check-true (= x (pq-top pq) (pq-top pq2)))
      (check-true (= x (- size i 1)))
      (loop (add1 i) pqrst (pq-rest pq2)))))

(let loop ([i 0] [pq dqL]  [pq2 dqL])
  (let-values ([(x pqrst) (pq-top+rest pq)])
;     (printf "i = ~a, hd = ~a (- size i 1) = ~a\n" i (ft-hdL ft2) (- size i 1))))
    (unless (= i (sub1 size))
      (check-true (= x (pq-top pq) (pq-top pq2)))
      (check-true (= x i))
      (loop (add1 i) pqrst (pq-rest pq2)))))

(let loop ([i 0] [pq dqR]  [pq2 dqR])
  (let-values ([(x pqrst) (pq-top+rest pq)])
;     (printf "i = ~a, hd = ~a (- size i 1) = ~a\n" i (ft-hdL ft2) (- size i 1))))
    (unless (= i (sub1 size))
      (check-true (= x (pq-top pq) (pq-top pq2)))
      (check-true (= x i))
      (loop (add1 i) pqrst (pq-rest pq2)))))

;(check-true
; (for/and ([i size])
;   (let-values ([(ft1 ft2) (ra-splitat i dqR2)])
;;     (printf "i = ~a, hd = ~a (- size i 1) = ~a\n" i (ft-hdL ft2) (- size i 1))))
;     (= (ra-hdL ft2) i))))
;
;(check-true
; (for/and ([i size])
;;     (printf "i = ~a, hd = ~a (- size i 1) = ~a\n" i (ft-hdL ft2) (- size i 1))))
;   (= (ra-ref dqL2 i) (- size i 1))))
;
;(check-true
; (for/and ([i size])
;;     (printf "i = ~a, hd = ~a (- size i 1) = ~a\n" i (ft-hdL ft2) (- size i 1))))
;   (= (ra-ref dqR2 i) i)))

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