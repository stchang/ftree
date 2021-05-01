#lang scribble/manual
@(require scribble/eval
          (for-label ftree
                     raseq
                     pqueue
                     orderedseq
                     intervaltree)
          racket/base)

@title{Finger Trees}

@(define the-eval (make-base-eval))
@(the-eval '(require ftree
                     raseq
                     pqueue/pqueue
                     orderedseq/orderedseq
                     intervaltree/intervaltree))


@author[@author+email["Stephen Chang" "stchang@racket-lang.org"]]

Finger trees are a general purpose data structure that can be used to implement many other data structures including multiple variations of sequences, deques, and queues. Finger trees support amortized constant time access to both ends as well as logarithmic time concatenation and splitting.

Finger trees achieve the log append and split bounds by caching a "measurement" in each node. the actual measurement computed is determined by the specific data structure being represented. For example, caching the size of the subtree at each node implements a random-access sequence.

Based on Hinze and Paterson's JFP 2006 @cite["HP06"] paper.

This implementation currently does not utilize laziness, because preliminary benchmarking indicates that it is not beneficial to do so. In other words, the benefits of laziness do not outweigh the practical overhead that laziness incurs. Thus, the theoretical bounds from the Hinze and Paterson's paper may not hold when the data structures are used persistently. However, the data structures should still perform well in practice.

@section{Finger Trees}

@defmodule[ftree #:use-sources (ftree)]

@defproc[(mk-ftree [∅ any/c][elem-sz (-> any/c any/c)][⊕ (-> any/c any/c any/c)]) ftree?]{
  Creates an empty @deftech{finger tree}. @racket[∅] is the "measurement" of the empty tree. @racket[elem-sz] takes an element in the tree and returns the measurement for that element. Finally, @racket[⊕] must be an associative binary operation that combines measurements.}


@defproc[(ftree? [x any/c]) boolean?]{ 
  Returns @racket[#t] if @racket[x] is an @tech{finger tree} and @racket[#f] otherwise.
}

@defproc[(ft-empty? [ft ftree?]) boolean?]{Indicates whether the given finger tree is empty.}

@defthing[empty-ft ftree?]{The empty finger tree.}

@defproc[(ft-consL [x any/c] [ft ftree?]) ftree?]{
  Inserts the given value in the finger tree on the left.}
@defproc[(ft-hdL [ft ftree?]) any/c]{
  Returns the leftmost element from the finger tree. Errors on an empty tree.}
@defproc[(ft-tlL [ft ftree?]) ftree?]{
  Returns the given finger tree without its leftmost element. Errors on an empty tree.}
@defproc[(ft-hd+tlL [ft ftree?]) (values any/c ftree?)]{
  Combination of @racket[ft-hdL] and @racket[ft-tlL], for efficiency.}

@defproc[(ft-consR [x any/c] [ft ftree?]) ftree?]{
  Inserts the given value in the finger tree on the right.}
@defproc[(ft-hdR [ft ftree?]) any/c]{
  Returns the rightmost element from the finger tree. Errors on an empty tree.}
@defproc[(ft-tlR [ft ftree?]) ftree?]{
  Returns the given finger tree without its rightmost element. Errors on an empty tree.}
@defproc[(ft-hd+tlR [ft ftree?]) (values any/c ftree?)]{
  Combination of @racket[ft-hdR] and @racket[ft-tlR], for efficiency.}

@defproc[(ft-append [ft1 ftree?] [ft2 ftree?]) ftree?]{Appends the given finger trees.}

@defproc[(ft-split [p? (-> any/c boolean?)] [ft ftree?]) (values ftree? ftree?)]{
  Splits the given finger tree into two parts such that the given predicate is false for all elements in the first part and true for all elements in the second part.}

@defthing[gen:ft any/c]{
  A @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{generic interface} (see
  @secref[#:doc '(lib "scribblings/reference/reference.scrbl") "struct-generics"]) for finger tree-based data structures. Any data structure that implements this interface may use the operations above.
@itemize[
  @item{@racket[gen:mk : (-> any/c any/c (-> any/c any/c) any/c any/c any/c)] : Builds a new finger tree based on an existing finger tree. Takes five arguments: @racket[existing-ftree ∅ sz-fn ⊕ internal-ftree].}]}
          






@section{Random-Access Sequences}

@defmodule[raseq #:use-sources (raseq)]

@defproc[(raseq? [x any/c]) boolean?]{Identifies random-access sequences.}
@defproc[(mk-raseq) raseq?]{Makes an empty random access sequence.}
@defthing[empty-ras raseq?]{An empty random access sequence.}
@defproc[(ra-empty? [ras raseq?]) boolean?]{Indicates whether the given random-access sequence is empty.}
@defproc[(ra-splitat [i exact-nonnegative-integer?] [ras raseq?]) (values raseq? raseq?)]{
  Splits the given random-access sequence at the given index.}
@defproc[(ra-ref [ras raseq?] [i exact-nonnegative-integer?]) any/c]{
  Returns the @racket[i]th element of the given random-access sequence.}





@section{Priority Queues}

@defmodule[pqueue #:use-sources (pqueue)]

@defproc[(pqueue? [x any/c]) boolean?]{Identifies priority queues.}
@defproc[(mk-pqueue [<= (-> any/c any/c boolean?)]) pqueue?]{
  Makes an empty priority queue using the given comparison function.}
@defproc[(pq-empty? [pq pqueue?]) boolean?]{Indicates whether the given priority queue is empty.}
@defproc[(pq-top [pq pqueue?]) any/c]{Returns the top element in the priority queue.}
@defproc[(pq-rest [pq pqueue?]) pqueue?]{
  Returns a new priority queue that is the given queue but with the topmost element removed.}
@defproc[(pq-top+rest [pq pqueue?]) (values any/c pqueue?)]{
  Combination of @racket[pq-top] and @racket[pq-rest], for efficiency.}



@section{Ordered Sequences}

@defmodule[orderedseq #:use-sources (orderedseq)]

From Hinze and Paterson's paper:
 "ordered sequences [can be seen as an optimization] of, and subsume, priority
  queues, as we have immediate access to the smallest and the greatest element,
  and search trees, as we can partition ordered sequences in logarithmic time."

Differences with pqueues:
@itemize[
 @item{Ordered sequences store elements in sorted order, so the @racket[ft-hd] and @racket[ft-tl] functions can grab both min and max elements in constant time, but inserting elements requires a special @racket[os-insert] function.}
 @item{Priority queues store elements in arbitrary order, using the "measure" function, ie @racket[sz],
 to determine the order, so @racket[ft-cons] can be used to insert.}]

@defproc[(oseq? [x any/c]) boolean?]{Identifies an ordered sequence.}
@defproc[(mk-oseq [< (-> any/c any/c boolean?)]) oseq?]{
  Makes an empty ordered sequence using the given comparison function. NOTE: The given comparison function must not include equality because when partitioning, the implementation assumes that the equality elements are in the right partition. So @racket[<] is ok but @racket[<=] will not work. The @racket[equal?] function is used for equality.}
@defproc[(os-empty? [os oseq?]) boolean?]{Indicates whether the given ordered sequence is empty.}

@defproc[(os-partition [x any/c] [os oseq?]) (values oseq? oseq?)]{
  Partitions the given ordered sequence into two parts such that all elements in the first part are "less than" the given @racket[x] with respect to the previously specified comparison, and all elements in the second part are "greater than or equal to" @racket[x].}

@defproc[(os-insert [x any/c] [os oseq?]) oseq?]{
  Inserts @racket[x] into @racket[os], maintaining proper order.}
@defproc[(os-delete-all [x any/c] [os oseq?]) oseq?]{
  Returns a new ordered sequence with all @racket[x]'s deleted.}

@defproc[(os-top [os oseq?]) any/c]{Returns the top element of the ordered sequence.}
@defproc[(os-remove-top [os oseq?]) oseq?]{Returns the given oredered sequence without the top element.}
@defproc[(os-bot [os oseq?]) any/c]{Returns the bottom element of the ordered sequence.}
@defproc[(os-remove-bot [os oseq?]) oseq?]{Returns the given oredered sequence without the bottom element.}

@defproc[(os-merge [os1 oseq?] [os2 oseq?]) oseq?]{
  Merges the given ordered sequences, maintaining proper order.}


@section{Interval Trees}

@defmodule[intervaltree #:use-sources (intervaltree)]

Represents sets of intervals where an interval is a pair of real numbers, lo and hi, with lo <= hi. Intervals are sorted in ascending order of the lo's, when added via @racket[it-insert] (but not with any of the @racket[ft-cons] functions). The cached measure is an interval where the lo is the lo of the rightmost element and the hi is the maximum of the element hi's.

@defproc[(itree? [x any/c]) boolean?]{Identifies an interval tree.}
@defproc[(mk-itree) itree?]{Makes an empty interval tree.}
@defproc[(it-empty? [it itree?]) boolean?]{Identifies an empty interval tree.}

@defproc[(it-insert [lo number?] [hi number?] [it itree?]) itree?]{
  Inserts the specified interval in the given interval tree, maintaining ordering.}

@defproc[(it-search [it itree?] [lo number?] [hi number?]) (or/c interval? #f)]{
  Of the intervals in the given interval tree that intersect the given interval, returns the one with the lowest lo.}

@defproc[(it-match [it itree?] [lo number?] [hi number?]) (listof interval?)]{
  Returns all intervals in the given interval tree that intersect the given interval.}

@defproc[(interval? [x any/c]) boolean?]{Identifies an interval struct.}
@defproc[(interval [lo number?] [hi number?]) interval?]{Constructs an interval struct.}
@defproc[(interval-low [i interval?]) number?]{Returns the interval lo.}
@defproc[(interval-high [i interval?]) number?]{Returns the interval hi.}
@defproc[(interval-intersect? [i interval?] [j interval?]) boolean?]{
  Indicates if interval @racket[i] and @racket[j] intersect, where intersection means 
                        @racket[(<= (interval-low i) (interval-high j))] and @racket[(<= (interval-low j) (interval-high i))]
                        is true.}


@(bibliography
  
  (bib-entry #:key "HP06"
             #:author "Ralf Hinze and Ross Paterson"
             #:title "Finger trees: a simple general-purpose data structure"
             #:location "Journal of Functional Programming"
             #:date "2006"))

