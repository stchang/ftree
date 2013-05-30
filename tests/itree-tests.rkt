#lang racket
(require rackunit)
(require "../ftree/ftree.rkt")
(require "../intervaltree/intervaltree.rkt")

(check-exn exn:fail? (thunk (it-insert 20 2 (mk-itree))))

(define it (it-insert 3 30 (it-insert 4 40 (it-insert 2 20 (it-insert 1 10 (mk-itree))))))


(define (mk-random-interval [lo 0] [hi 100])
  (define random-hi (random (- hi lo)))
  (define random-lo (random (add1 random-hi)))
  (values (+ lo random-lo) (+ lo random-hi)))

(define (mk-random-itree size [lo 0] [hi 100])
  (if (zero? size)
      (mk-itree)
      (let-values ([(random-lo random-hi) (mk-random-interval lo hi)])
        (it-insert random-lo random-hi (mk-random-itree (sub1 size) lo hi)))))

(define (check-ordered? it)
  ; stop if less than 2 elements
  (or (ft-empty? it)
      (ft-empty? (ft-tlL it))
      (let*-values ([(hd tl) (ft-hd+tlL it)]
                    [(hd2 tl2) (ft-hd+tlL tl)])
;        (printf "hd low = ~a, hd2 low = ~a\n" (interval-low hd) (interval-low hd2))
        (and (<= (interval-low hd) (interval-low hd2))
             (check-ordered? tl)))))

(check-true (check-ordered? it))
(check-true (check-ordered? (mk-random-itree 1000 100 10000)))

(define (check-random-interval-searches num-times [lo 0] [hi 100])
  (define it (mk-random-itree 1000 lo hi))
  (let loop ([n num-times])
    (unless (zero? n)
      (let-values ([(random-lo random-hi) (mk-random-interval lo hi)])
        (define res (it-search it random-lo random-hi))
        (define ress (it-match it random-lo random-hi))
;        (printf "random int = ~a ~a\n" random-lo random-hi)
;        (printf "res = ~a ~a\n" (interval-low res) (interval-high res))
;        (map (λ (x) (printf "ress = ~a ~a\n" (interval-low x) (interval-high x))) ress)
        (when res
          (check-true (interval-intersect? res (interval random-lo random-hi))))
        (map (λ (res) (check-true (interval-intersect? res (interval random-lo random-hi)))) ress)
        (loop (sub1 n))))))

(check-random-interval-searches 1000 100 10000)

