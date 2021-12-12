#!/usr/bin/racket
#lang racket

(require "../util.rkt")
(require megaparsack megaparsack/text)
(require (rename-in data/monad [do monad-do]))
(require data/applicative)

(define cave/p (many/p letter/p))
(define connection/p
  (monad-do
   [cave1 <- cave/p]
   (char/p #\-)
   [cave2 <- cave/p]
   (pure (cons (list->string cave1) (list->string cave2)))))

(define (parse-input fname)
  (let ([raw-input (parse-file (many/p connection/p #:sep (char/p #\newline)) fname)]
        [add-edge (lambda (p result) (hash-update
                                      (hash-update result (car p) (λ (s) (set-add s (cdr p))) (set))
                                      (cdr p) (λ (s) (set-add s (car p))) (set)))])
    (foldr add-edge (hash) raw-input)))



(define (small? cave)
  (for/and ([letter (in-string cave)])
    (char-lower-case? letter)))

(define big? (compose not small?))

(define (connected input cave)
  (hash-ref input cave (set)))

(define (paths input initial-visited visit can-visit? start end)
  (define (path-helper path visited)
    (if (equal? (car path) end)
        (list (reverse path))
        (let* ([current (car path)]
               [next (connected input current)])
          (append* (for/list ([next-cave (in-set next)] #:when (can-visit? visited next-cave))
                     (path-helper (cons next-cave path) (visit visited next-cave)))))))
  (path-helper (list start) (visit initial-visited "start")))

(define *test-input* (parse-input "input-test"))

(define (solution1 fname)
  (let ([visit-fn (λ (s c) (if (small? c) (set-add s c) s))]
        [visited?-fn (λ (s c) (not (set-member? s c)))])
    (length (paths (parse-input fname) (set) visit-fn visited?-fn "start" "end"))))

(define (visit-twice data cave)
  (cond
    ((big? cave) data)
    ((and
      (set-member? (cdr data) cave)
      (not (car data)))
     (cons cave (cdr data)))
    ((not (set-member? (cdr data) cave))
     (cons (car data) (set-add (cdr data) cave)))
    (#t (error (~a "Unknown visitation: " data ", " cave)))))

(define (visit-twice-mult data . caves)
  (foldl (λ (c d) (visit-twice d c)) data caves))

(define (debug-can-visit-twice? data cave)
  (let ([result (can-visit-twice? data cave)])
    (when (and (equal? cave "b") result)
      (display (~a "Can visit " cave " with data " data "? " result "\n")))
    result))

(define (can-visit-twice? data cave)
  (cond
    ((equal? "start" cave) #f)
    ((big? cave) #t)
    ((equal? (car data) cave) #f)
    ((not (car data)) #t)
    (#t (not (set-member? (cdr data) cave)))))

(define initial-twice-visited (cons #f (set)))

(define (solution2 fname)
  (length (paths (parse-input fname) initial-twice-visited visit-twice can-visit-twice? "start" "end")))
