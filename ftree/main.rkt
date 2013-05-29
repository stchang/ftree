#lang racket
(require "ftree.rkt")
(provide mk-ftree empty-ft ft-empty? ftree?
         ft-consL ft-consR
         ft-hd+tlL ft-hd+tlR ft-hdL ft-tlL ft-hdR ft-tlR
         ft-append ft-split
         gen:ft)