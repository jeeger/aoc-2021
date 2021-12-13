#!/usr/bin/racket
#lang racket
(require "../util.rkt")
(require megaparsack megaparsack/text)
(require (rename-in data/monad [do monad-do]))
(require data/applicative)
(require racket/struct)

(define (chars->number l)
  (string->number (apply string l)))

(define point/p
  (monad-do
   [x <- (many+/p digit/p)]
   (char/p #\,)
   [y <- (many+/p digit/p)]
   newline/p
   (pure (point (chars->number x) (chars->number y)))))

(define fold/p
  (monad-do
   (string/p "fold along ")
   [axis <- (or/p (char/p #\x) (char/p #\y))]
   (char/p #\=)
   [point <- (many/p digit/p)]
   (pure (fold (string->symbol (string axis)) (chars->number point)))))

(struct input [points folds]
  #:transparent
  #:methods gen:custom-write
  [(define write-proc (make-constructor-style-printer
                       (lambda (obj) 'input)
                       (lambda (obj) (cons (input-points obj) (input-folds obj)))))])

(struct fold [axis foldpoint]
  #:transparent
  #:methods gen:custom-write
  [(define write-proc (make-constructor-style-printer
                       (lambda (obj) 'fold)
                       (lambda (obj) (list (fold-axis obj) (fold-foldpoint obj)))))])

(define show-fold
  (match-lambda ((fold axis foldpoint)
                 (display (~a "Fold at " (symbol->string axis) "=" foldpoint "\n")))))
(define show-input
  (match-lambda ((input points folds)
                 (for ([p points]) (display (~a (point-x p) "," (point-y p) " ")))
                 (display "\n")
                 (for-each show-fold folds))))

(define input/p
  (monad-do
   [points <- (many+/p point/p)]
   newline/p
   [folds <- (many+/p fold/p #:sep newline/p)]
   (pure (input (apply set points) folds))))

(define apply-fold-point
  (match-lambda*
    ((list (point x y) (fold 'x p))
     (if (< x p) (point x y)
         (point (- (* 2 p) x) y)))
    ((list (point x y) (fold 'y p))
     (if (< y p) (point x y)
         (point x (- (* 2 p) y))))))

(define *test-input* (parse-file input/p "input-test"))

(define (apply-fold input fold)
  (for/set ([point (in-set input)])
    (apply-fold-point point fold)))

(define (solution1 fname)
  (let ([input-raw (parse-file input/p fname)])
    (set-count (apply-fold (input-points input-raw) (car (input-folds input-raw))))))

(define (bounds points)
  (define (bounds-helper agg pfun)
    (apply agg (for/list ([p points]) (pfun p))))
  (values (bounds-helper min point-x) (bounds-helper min point-y)
          (bounds-helper max point-x) (bounds-helper max point-y)))

(define (show-points points)
  (let-values ([(xmin ymin xmax ymax) (bounds points)])
    (for* ([y (in-inclusive-range ymin ymax)]
           [x (in-inclusive-range xmin xmax)])
      (if (set-member? points (point x y))
          (display "x")
          (display "."))
      (when (equal? x xmax)
        (display "\n")))))

(define (solution2 fname)
  (let ([input-raw (parse-file input/p fname)])
    (show-points (foldl (λ (f i) (apply-fold i f)) (input-points input-raw) (input-folds input-raw)))))

(when (not (equal? (solution1 "input-test") 17))
  (error "Solution 1 test incorrect."))
