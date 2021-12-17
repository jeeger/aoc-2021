#lang racket

(require "../util.rkt")
(require "../parser.rkt")
(require "../bitvector.rkt")

(require (rename-in data/monad [do monad-do]))
(require (only-in data/applicative pure))
(require data/functor)
(require data/applicative)
(require racket/vector)
(require (rename-in racket/base [map basemap]))

(define (input-read fname)
  (port->string (open-input-file fname)))

(define *test-input-1* (input-read "input-test1"))
(define *test-input-2* (input-read "input-test2"))
(define *test-input-3* (input-read "input-test3"))
(define *test-input-4* (input-read "input-test4"))

(struct packet [version] #:transparent)
(struct literal-packet packet [value] #:transparent)
(struct operator-packet packet [typeid subpackets] #:transparent)

(define version/p (number/p 3))
(define typeid/p (number/p 3))

(define packet/p
  (monad-do
   [version <- version/p]
   [typeid <- typeid/p]
   (if (eq? typeid 4)
       (literal-packet/p version)
       (operator-packet/p version typeid))))


(define varint/p
  (monad-do
   [continue <- (bit/p)]
   [bits <- (bits/p 4)]
   [restbits <- (if continue
                    varint/p
                    (pure #()))]
   (pure (vector-append bits restbits))))

(define (literal-packet/p version)
  (monad-do
   [varint <- varint/p]
   (pure (literal-packet version (bits->number varint)))))

(define (n-packets/p howmany)
  (if (eq? howmany 0)
      (pure '())
      (monad-do
       [p <- packet/p]
       [restpackets <- (n-packets/p (sub1 howmany))]
       (pure (cons p restpackets)))))

(define (bits-packets/p bits)
  (if (eq? bits 0)
      (pure '())
      (monad-do
       [current-length <- (stream-size/p)]
       [p <- packet/p]
       [new-length <- (stream-size/p)]
       [restpackets <- (bits-packets/p (- bits (- current-length new-length)))]
       (pure (cons p restpackets)))))

(define (operator-packet/p version typeid)
  (monad-do
   [length-type <- (bit/p)]
   [packetlength <- (if length-type
                        (number/p 11)
                        (number/p 15))]
   [subpackets <- (if length-type
                   (n-packets/p packetlength)
                   (bits-packets/p packetlength))]
   (pure (operator-packet version typeid subpackets))))

(define (show-packet packet [indent 0])
  (if (literal-packet? packet)
      (~a (make-string indent #\ ) "Literal packet. Version: " (packet-version packet) ", value: " (literal-packet-value packet) "\n")
      (~a (make-string indent #\ ) "Operator packet. Version: " (packet-version packet) ", type ID: "
          (operator-packet-typeid packet)
          ", subpackages:\n"
          (basemap (λ (p) (show-packet p (+ 2 indent))) (operator-packet-subpackets packet)) "\n")))

(define (display-packet p)
  (display (show-packet p)))

(define/match (version-sum p)
  [((literal-packet version value)) version]
  [((operator-packet version typeid subpackets))
   (apply + version (basemap version-sum subpackets))])

(define/match (packet-eval p)
  [((literal-packet version value)) value]
  [((operator-packet _ 0 subpackets)) (apply + (basemap packet-eval subpackets))]
  [((operator-packet _ 1 subpackets)) (apply * (basemap packet-eval subpackets))]
  [((operator-packet _ 2 subpackets)) (apply min (basemap packet-eval subpackets))]
  [((operator-packet _ 3 subpackets)) (apply max (basemap packet-eval subpackets))]
  [((operator-packet _ 5 (list l r))) (if (> (packet-eval l) (packet-eval r)) 1 0)]
  [((operator-packet _ 6 (list l r))) (if (< (packet-eval l) (packet-eval r)) 1 0)]
  [((operator-packet _ 7 (list l r))) (if (eq? (packet-eval l) (packet-eval r)) 1 0)])

(define (file->packet fname)
  (packet/p (make-bitvector-from-hex-string (input-read fname))))

(define (solution1 fname)
  (version-sum (file->packet fname)))

(provide (all-defined-out))
