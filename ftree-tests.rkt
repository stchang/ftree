#lang racket
(require rackunit)
(require "ftree.rkt")
;(require "../../NEU_Research/lazyprofile/test-utils.rkt")

;; Similar to build-list function
#;(define (build-queue size f)
  (let loop ([n size])
    (if (zero? n)
        empty-ft
        (let ([nsub1 (sub1 n)])
          (ft-cons (f nsub1) (loop nsub1))))))

;; add first thresh elements of queue, with thresh increasing
#;(let ([base 20] ; queue size will be 2^base - 1
      [num-runs 10] ; test measures time that is avg of num-runs runs
      [start-ratio .01] ; start thresh at queue size * start-ratio
      [samples 100]) ; grow thresh at this rate on each iteration
  (let* ([qsize (expt 2 base)] ; 2^20 - 1 (ie, rear queue size = 0)
         [step (inexact->exact (truncate (/ qsize samples)))])
    (let outer-LOOP ([thresh (inexact->exact (truncate (* start-ratio qsize)))])
      ;      (define q (build-queue qsize add1))
      (define-values (res avg-time)
        (time/avg
         (let loop ([q (build-queue qsize add1)] [n 0])
           (if (>= n thresh)
               0 
               (match-let ([(ViewL x ft) (viewL q)])
                 (+ x (loop ft (add1 n))))))
         num-runs
         #t))
      (printf "(~a ~a)\n" thresh avg-time)
      (unless (> (+ thresh step) qsize)
        (outer-LOOP (+ thresh step))))))



;; test finger tree add/remove/append 
;; by making sure elemnts are in the right order

;; sums the x front, ie left, deque elements
(define (sum-front-x x q)
  (let loop ([n 0] [d q])
    (if (= n x) 0
        (+ (head d) (loop (add1 n) (tail d))))))
;; sums the x rear, ie right, deque elements
(define (sum-rear-x x q)
  (let loop ([n 0] [d q])
    (if (= n x) 0
        (+ (last d) (loop (add1 n) (init d))))))

(define size 1030)
(define d (build-deque size (λ (x) x)))
(define dfront (build-deque-front size (λ (x) x)))
(check-true
 (for/and ([thresh size])
   (= (sum-front-x thresh d)
      (sum-rear-x thresh dfront)
      (for/sum ([n thresh]) n))))

(define size/2 size)
(define d1 (build-deque size/2 (λ (x) x)))
(define d2 (build-deque size/2 (λ (x) (+ x size/2))))
(define dapp (append d1 d2))
(define d1front (build-deque-front size/2 (λ (x) x)))
(define d2front (build-deque-front size/2 (λ (x) (+ x size/2))))
(define dappfront (append d2front d1front))
(check-true
 (for/and ([thresh size])
   (= (sum-front-x thresh dapp)
      (sum-rear-x thresh dappfront)
      (for/sum ([n thresh]) n))))


(define (build-deque-with-append n f) ; n = size
  (if (<= n 2)
      (enqueue (f (sub1 n)) (enqueue (f (- n 2)) empty))
      (let ([n/2 (/ n 2)])
        (append (build-deque-with-append n/2 (λ (x) (f x)))
                (build-deque-with-append n/2 (λ (x) (+ n/2 (f x))))))))
(define size2 256)
(define d-with-append1 (build-deque-with-append size2 (λ (x) x)))
(define d-with-append2 (build-deque-with-append size2 (λ (x) (+ x size2))))
(define d-with-append (append d-with-append1 d-with-append2))
#;(let loop ([d d-with-append])
  (unless (empty? d)
    (displayln (head d))
    (loop (tail d))))
(check-true
 (for/and ([thresh (* 2 size2)])
   (= (sum-front-x thresh d-with-append)
      (for/sum ([n thresh]) n))))

(define (log2 x) (/ (log x) (log 2)))
;; -------- depth analysis --------
;;- only way to get Deep structures is to append two Shallows of size >= 2
;;- worst case nesting occurs when leaves are Shallows of size 2
;;  (binary tree essentially)
;;- thus, worst case DEEP depth = floor(log(size)/2), 
;;        ie depth(n) = floor(n/2), where size = 2^n
;;- appending two deques of size 2^n requires depth(n)+1 appends
;;- in the lazy version of this degenerate binary deque,
;;  since every 4 queue elements requires 1 thunk, 
;;  a deque of size n requires n/4 thunks.


;; -------- bottom line --------
;; - In implicit or catenable or other similar deques, the "worst case"
;;   operation is O(log n). Thus, when performing an expected constant-time 
;;   operation in a non-lazy implementation, the worst case is O(log n).
;;   Compare to banker's deque, where the worst case is O(n).
;;   Thus, the question is if the overhead of laziness is > O(log n).

;; testing persistence
;(define n 10)
;(define dq-persist1 (build-deque-with-append (expt 2 n) (λ (x) x)))
;(define dq-persist2 (build-deque-with-append (expt 2 n) (λ (x) x)))
;(time (append (build-deque-with-append (expt 2 n) (λ (x) x)) (build-deque-with-append (expt 2 n) (λ (x) x))))
;(time (append dq-persist1 dq-persist2))
;(time (append dq-persist1 dq-persist2))
;(time (append dq-persist1 dq-persist2))

;; n = 22
;(time (build-deque-with-append (expt 2 22) values))
;(time (build-deque-with-append (expt 2 22) values))
;; cpu time: 1924 real time: 1929 gc time: 976
;; cpu time: 2040 real time: 2047 gc time: 1104
(define dq1 (build-deque-with-append (expt 2 9) values))
(define dq2 (build-deque-with-append (expt 2 9) values))
(time (for ([x (expt 2 10)]) (append dq1 dq2)))

;; append counting; see also notes in deque-catenable-lazy-tests.rkt
;; (define dq (build-deque-with-append (expt 2 10) values))
;; 847 appends
;; (define dq1 (build-deque-with-append (expt 2 9) values))
;; (define dq2 (build-deque-with-append (expt 2 9) values))
;; 412 appends
;; (append dq1 dq2)
;; 5 appends

;; **********
;; * Bottom line is, this is not a great data structure for testing persistence
;; because the expensive operation only happens when appending deeply nested
;; DEEP deques, but then most of the work is done during the create.
;; I can't set up a nice situation like the banker's queue where an 
;; *already created* queue has a pending expensive operation.

(define args (current-command-line-arguments))
(define limit-arg
  (if (zero? (vector-length args)) 1
      (string->number (vector-ref args 0))))

;; persistence testing lazy vs strict with same code
(time
 (let* ([n 19]
        [dq1 (build-deque-with-append (expt 2 n) add1)] ; 421 appends
        [dq2 (build-deque-with-append (expt 2 n) add1)] ; 421 appends
;        [dq3 (append dq1 dq2)]                          ; 5 appends
        [limit limit-arg])
   (let loop ([count 0])
     (unless (= count limit) ;; number of times to call tail
       (tail (append dq1 dq2))    ;; 5 appends each time
       (loop (add1 count))))))
;; 842 + 5 on first run + 5*num repeats (n=9)
;; so strict should catch up to lazy in (847-766)/(5-2) = 27 runs
;; **reality**: strict catches up after repeating ~1 million times 
;; (specifically between 2^18 and 2^19)