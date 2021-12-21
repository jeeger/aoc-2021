#lang racket/base

(require racket/struct)
(require racket/match)

(struct point [x y]
  #:transparent
  #:methods gen:custom-write
  [(define write-proc (make-constructor-style-printer
                       (lambda (obj) 'point)
                       (lambda (obj) (list (point-x obj) (point-y obj)))))])
(define (point->pair p)
  (cons (point-x p) (point-y p)))

(define (pair->point c) (point (car c) (cdr c)))

(define/match (point-map-coord p which fn)
  [((point x y) 'x fn) (point (fn x) y)]
  [((point x y) 'y fn) (point x (fn y))])

(define/match (point-map p xfn yfn)
  [((point x y) xfn yfn) (point (xfn x) (yfn y))])
  
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

(define/match (vec> p1 p2)
  [((point x1 y1) (point x2 y2))
   (and (> x1 x2)
        (> y1 y2))])

(define/match (vec< p1 p2)
  [((point x1 y1) (point x2 y2))
   (and (< x1 x2)
        (< y1 y2))])

(define/match (in-point-range range p)
  [((point-range (point minx miny) (point maxx maxy))
    (point x y))
   (and (<= minx x maxx)
        (<= miny y maxy))])

(define/match (point-dist p1 p2)
  [((point x1 y1) (point x2 y2))
   (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))])

(define (bounds points)
  (define (bounds-helper agg pfun)
    (apply agg (for/list ([p points]) (pfun p))))
  (values (bounds-helper min point-x) (bounds-helper min point-y)
          (bounds-helper max point-x) (bounds-helper max point-y)))

(struct point-range [min max] #:transparent #:constructor-name -point-range)

(define (make-point-range p1 p2)
  (let-values ([(minx miny maxx maxy) (bounds (list p1 p2))])
    (-point-range (point minx miny) (point maxx maxy))))


(provide pair->point point->pair point-map-coord point-map)
(provide adjacent adjacent-diagonal)
(provide bounds)
(provide cross-product dot-product vec- vec+ vec* vec> vec<)
(provide (struct-out point))
(provide point-range point-range-min point-range-max make-point-range in-point-range point-dist)
