#!/usr/bin/racket
#lang racket

(require "../util.rkt")
(require (only-in megaparsack/text letter/p string/p))
(require (only-in megaparsack many/p repeat/p))
(require (rename-in data/monad [do monad-do]))
(require (only-in data/applicative pure))
(require (only-in racket/hash hash-union))

(define template/p
  (monad-do
   [template <- (many/p letter/p)]
   (pure template)))

(define rule/p
  (monad-do
   [letter1 <- letter/p]
   [letter2 <- letter/p]
   (string/p " -> ")
   [replacement <- letter/p]
   (pure (cons (cons letter1 letter2) replacement))))

(define input/p
  (monad-do
   [template <- template/p]
   (repeat/p 2 newline/p)
   [rules <- (many/p rule/p #:sep newline/p)]
   (pure (cons template (make-immutable-hash rules)))))

(define *test-input* (parse-file input/p "input-test"))

(define (extend input l)
  (define (extend-helper result l)
    (if (= (length l) 1)
        (reverse (cons (list-ref l 0) result))
        (let ([replacement (hash-ref input (cons (car l) (cadr l)) (cadr l))])
          (extend-helper (cons replacement (cons (car l) result)) (cdr l)))))
  (extend-helper '() l))

(define (extend-n input n l)
  (if (equal? n 0)
      l
      (extend-n input (sub1 n) (extend input l))))

(define (count-elems l)
  (foldl (λ (elem count) (hash-update count elem add1 0)) (hash) l))

(define (compute n fname)
  (let* ([raw-input (parse-file input/p fname)]
         [extended (extend-n (cdr raw-input) n (car raw-input))]
         [count (hash-values (count-elems extended))])
    (- (apply max count) (apply min count))))

(define solution1 (curry compute 10))

(when (not (equal? (solution1 "input-test") 1588))
  (println "Test solution 1 not correct"))

(define (combine-frequencies p1 p2 common)
  (hash-update (hash-add p1 p2)
               common sub1))

(define (hash-add h . hs)
  (foldl (λ (h1 h2) (hash-union h1 h2 #:combine +)) h hs))

(define (pair->hash p1 p2)
  (hash-update (hash p1 1) p2 add1 0))

(define (make-extender rules)
  (let ([cache (make-hash)])
    (define (update-cache! n p1 p2 v)
      (hash-set! cache (cons n (cons p1 p2)) v)
      v)
    (letrec
        ([pair-helper
          (lambda (n p1 p2)
            (if (hash-has-key? cache (cons n (cons p1 p2)))
                (hash-ref cache (cons n (cons p1 p2)))
                (if (= n 0)
                    (update-cache! n p1 p2 (pair->hash p1 p2))
                    (let* ([replacement (hash-ref rules (cons p1 p2))]
                           [p1-result (pair-helper (sub1 n) p1 replacement)]
                           [p2-result (pair-helper (sub1 n) replacement p2)])
                      (update-cache! n p1 p2 (combine-frequencies p1-result p2-result replacement))))))])
      pair-helper)))


(define (list-pairs l)
  (for/list ([p1 (take l (sub1 (length l)))]
             [p2 (drop l 1)])
    (cons p1 p2)))

(define (extend-dynamic n rules l)
  (define extender (make-extender rules))
  (define (extend-dynamic-helper pairs)
    (let ([p1 (caar pairs)]
          [p2 (cdar pairs)])
      (if (= (length pairs) 1)
          (extender n p1 p2)
          (combine-frequencies (extend-dynamic-helper (cdr pairs))
                               (extender n p1 p2) p2))))
  (extend-dynamic-helper (list-pairs l)))

(define (compute-dynamic n fname)
  (let* ([input (parse-file input/p fname)]
         [frequencies (hash-values (extend-dynamic n (cdr input) (car input)))])
    (- (apply max frequencies) (apply min frequencies))))
      
(define solution1-dynamic (curry compute-dynamic 10))

(when (not (= (solution1-dynamic "input-test") 1588))
  (printf "Test solution 1 dynamic is not correct."))

(define solution2 (curry compute-dynamic 40))
