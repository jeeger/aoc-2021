#!/usr/bin/racket
#lang racket

(define input-file (vector-ref (current-command-line-arguments) 0))
(define input-stream (open-input-file input-file))
(format "~a" (for/fold ([increases 0] [last-element 500] #:result increases) ([line (in-lines input-stream)])
               (let ([depth (string->number line)])
                 (values (+ increases (if (> depth last-element) 1 0)) depth))))
