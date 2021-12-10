#!/usr/bin/racket
#lang racket
(require megaparsack megaparsack/text)
(require (rename-in data/monad [do monad-do]))
(require data/applicative)
(require data/either)

(define (closing char)
  (hash-ref pairs char))

(define (is-opening? char)
  (hash-has-key? pairs char))

(define pairs
  #hash((#\( . #\))
        (#\[ . #\])
        (#\{ . #\})
        (#\< . #\>)))

(define (line-score calculate-score line)
  (define (parse-line-helper pairs line)
    (cond
      ((empty? line)
       (calculate-score pairs line))
      ((is-opening? (car line))
       (parse-line-helper (cons (car line) pairs) (cdr line)))
      ((and (not (empty? pairs))
            (eq? (car line) (closing (car pairs))))
       (parse-line-helper (cdr pairs) (cdr line)))
      (#t (calculate-score pairs line))))
  (parse-line-helper '() (string->list line)))

(define (solution1-score pairs line)
  (let ((solution1-points #hash((#\) . 3)
                                (#\] . 57)
                                (#\} . 1197)
                                (#\> . 25137))))
    (if (empty? line)
        0
        (hash-ref solution1-points (car line)))))

(define (solution2-score pairs line)
  (let ((solution2-points (for/hash ([char (in-string "([{<")]
                                     [score (in-inclusive-range 1 4)])
                            (values char score))))
    (if (not (empty? line))
        0
        (foldr (lambda (pair score) (+ (* score 5) (hash-ref solution2-points pair)))
               0 (reverse pairs)))))

(define (solution2-aggregate scores)
  (let ([filtered (filter (curry < 0) scores)])
    (list-ref (sort filtered <) (quotient (length filtered) 2))))

(define (solution aggregate-score calculate-score fname)
  (aggregate-score (for/list ([line (string-split (port->string (open-input-file fname)))])
                     (line-score calculate-score line))))
       

(define solution1 (curry solution (lambda (l) (apply + l)) solution1-score))
(define solution2 ((curry solution solution2-aggregate solution2-score)))


