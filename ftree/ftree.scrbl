#lang scribble/manual
@(require scribble/eval
          (for-label "ftree.rkt")
          racket/base)

@title{Finger Trees}

@(define the-eval (make-base-eval))
@(the-eval '(require "ftree.rkt"))

@defmodule[ftree]

@author[@author+email["Stephen Chang" "stchang@racket-lang.org"]]

Finger trees are a general purpose data structure that serve as a variety of data structures including a variety of sequences, deques, and queues. Finger trees support amortized constant time access to both ends as well as logarithmic time concatenation and splitting.

Finger trees achieve the log append and split bounds by caching a "measurement" in each node. the actual measurement computed is determined by the specific data structure being represented. For example, caching the size of the subtree at each node implements a random-access sequence.

Based on Hinze and Paterson's JFP 2006 @cite["HP06"] paper.


@section{Finger Trees}

@defproc[(mk-ftree [∅ any/c][elem-sz (-> any/c any/c)][⊕ (-> any/c any/c any/c)]) ftree?]{
  Creates an empty @deftech{finger tree}. @racket[∅] is the "measurement" of the empty tree. @racket[elem-sz] takes an element in the tree and returns the measurement for that element. Finally, @racket[⊕] must be an associative binary operation that combines measurements.
}

@defproc[(ftree? [ft any/c]) boolean?]{ 
  Returns @racket[#t] if @racket[ft] is an @tech{finger tree} and @racket[#f] otherwise.
}

@section{Random-Access Sequences}
@section{Max-Priority Queues}
@section{Ordered Sequences}
@section{Interval Trees}

@(bibliography
  
  (bib-entry #:key "HP06"
             #:author "Ralf Hinze and Ross Paterson"
             #:title "Finger trees: a simple general-purpose data structure"
             #:location "Journal of Functional Programming"
             #:date "2006"))

