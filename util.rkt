#lang racket
(require megaparsack)
(require megaparsack/text)
(require racket/struct)
(require (only-in data/monad [do monad-do]))
(require (only-in data/applicative pure))

(define char->number (compose string->number string))

(define (parse-file parser name)
  (parse-result! (parse-string parser (port->string (open-input-file name)))))

(define (test-parse-string parser string)
  (parse-result! (parse-string parser string)))

(define newline/p (char/p #\newline))

(struct point [x y]
  #:transparent
  #:methods gen:custom-write
  [(define write-proc (make-constructor-style-printer
                       (lambda (obj) 'point)
                       (lambda (obj) (list (point-x obj) (point-y obj)))))])
(define (point->pair p)
  (cons (point-x p) (point-y p)))

(define (pair->point c) (point (car c) (cdr c)))

(define (adjacent-diagonal input p)
  (for/list ([offset
              '((-1 . -1) (0 . -1) (1 . -1) (1 . 0) (1 . 1)
                          (0 . 1) (-1 . 1) (-1 . 0))]
             #:when (hash-has-key? input (vec+ p (pair->point offset))))
    (vec+ p (pair->point offset))))

(define (adjacent input p)
  (for/list ([offset
              '((0 . -1) (1 . 0) (0 . 1) (-1 . 0))]
             #:when (hash-has-key? input (vec+ p (pair->point offset))))
    (vec+ p (pair->point offset))))

(define cross-product
  (match-lambda* ((list (point x1 y1) (point x2 y2))
                  (- (* x1 y2)
                     (* y1 x2)))))

(define dot-product
  (match-lambda* ((list (point x1 y1) (point x2 y2))
                  (+ (* x1 x2)
                     (* y1 y2)))))

(define vec-
  (match-lambda* ((list (point x1 y1) (point x2 y2))
                  (point (- x1 x2)
                         (- y1 y2)))))

(define vec+
  (match-lambda* ((list (point x1 y1) (point x2 y2))
                  (point (+ x1 x2)
                         (+ y1 y2)))))

(define vec*
  (match-lambda* ((list (point x y) s)
                  (cons (* x s)
                        (* y s)))))

(define (bounds points)
  (define (bounds-helper agg pfun)
    (apply agg (for/list ([p points]) (pfun p))))
  (values (bounds-helper min point-x) (bounds-helper min point-y)
          (bounds-helper max point-x) (bounds-helper max point-y)))

(provide parse-file test-parse-string newline/p)
(provide char->number)
(provide pair->point point->pair)
(provide adjacent adjacent-diagonal)
(provide bounds)
(provide cross-product dot-product vec- vec+ vec*)
(provide (struct-out point))
