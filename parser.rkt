#lang racket/base
(require "bitvector.rkt")
(require data/functor)
(require data/applicative)
(require data/monad)
(require (only-in racket/stream stream-first))
(require racket/contract)
(require racket/match)

(define (number/p bits)
  (parser (lambda (bitvector)
            (let-values ([(number rest)
                          (bitvector-read-number bitvector bits)])
              (cons number rest)))))

(define (bits/p bits)
  (parser (lambda (bitvector)
            (let-values ([(bits rest)
                          (bitvector-read-bits bitvector bits)])
              (cons bits rest)))))

(define (bit/p)
  (parser (lambda (bitvector)
            (let-values
                ([(bits rest)
                  (bitvector-read-bits bitvector 1)])
              (cons (vector-ref bits 0) rest)))))

(define (stream-size/p)
  (parser (lambda (bitvector)
            (cons (bitvector-length bitvector) bitvector))))

(struct parser [parse]
  #:methods gen:functor
  [(define (map f x)
     (parser (λ (bv)
               (let ([result ((parser-parse x) bv)])
                 (cons (f (car result)) (cdr result))))))]
  #:methods gen:applicative
  [(define (pure _ x)
     (parser (λ (bv)
               (cons x bv))))
   (define (apply f xs)
     (car ((parser-parse f) (car xs))))]
  #:methods gen:monad
  [(define (chain f x)
     (parser (λ (bv)
               (match-let ([(list-rest result rest)
                            ((parser-parse x) bv)])
                 ((parser-parse (f result)) rest)))))])

(define (parserfn resulttype)
  (-> bitvector? (cons/c resulttype bitvector?)))

(provide (contract-out
          [number/p (-> number? (parserfn number?))]
          [bits/p (-> number? (parserfn (vectorof boolean?)))]
          [stream-size/p (-> (parserfn number?))]
          [bit/p (-> (parserfn boolean?))]))
