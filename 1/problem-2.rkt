#!/usr/bin/racket
#lang racket

(define input-file (vector-ref (current-command-line-arguments) 0))
(define input-stream (open-input-file input-file))

(define (increases l)
  (cadr
   (let ([listsize (- (length l) 2)])
    (foldl 
     (lambda (a b c acc) 
       (match acc ((list lastsize result)
                   (if (> (+ a b c) lastsize)
                       (list (+ a b c) (+ result 1))
                       (list (+ a b c) result)))))
     (list 10000 0)
     (take l listsize)
     (take (drop l 1) listsize)
     (drop l 2)))))

(define depths (for/list ([line (in-lines input-stream)])
                 (string->number line)))

(increases depths)
