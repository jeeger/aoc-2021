#lang racket/base
(require (prefix-in base: racket/base))
(require megaparsack megaparsack/text)
(require (rename-in data/monad [do monad-do]))
(require (only-in data/applicative pure))
(require (only-in racket/port port->string))

(define char->number (compose string->number string))

(define (parse-file parser name)
  (parse-result! (parse-string parser (port->string (open-input-file name)))))

(define (test-parse-string parser string)
  (parse-result! (parse-string parser string)))

(define newline/p (char/p #\newline))

(define number/p
  (monad-do
   [minus <- (many/p (char/p #\-) #:min 0 #:max 1)]
   [number <- integer/p]
   (pure (if (not (null? minus))
             (- number)
             number))))

(provide parse-file test-parse-string newline/p number/p)
(provide char->number)
