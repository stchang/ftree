#lang racket
(require "ftree.rkt")
(require "../../NEU_Research/lazyprofile/test-utils.rkt")

;; Similar to build-list function
(define (build-queue size f)
  (let loop ([n size])
    (if (zero? n)
        empty-ft
        (let ([nsub1 (sub1 n)])
          (ft-cons (f nsub1) (loop nsub1))))))

;; add first thresh elements of queue, with thresh increasing
(let ([base 20] ; queue size will be 2^base - 1
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