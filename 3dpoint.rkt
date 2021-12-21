#lang racket/base

(require racket/struct)
(require racket/match)
(require racket/list)
(require math/matrix)
(require racket/set)

(struct point [x y z] #:transparent)

(define (point->list p)
  (list (point-x p) (point-y p) (point-z p)))

(define (point-extract p)
  (values (point-x p) (point-y p) (point-z p)))

(define (list->point l) (point (car l) (cadr l) (caddr l)))

(define/match (point-map-coord p which fn)
  [((point x y z) 'x fn) (point (fn x) y z)]
  [((point x y z) 'y fn) (point x (fn y) z)]
  [((point x y z) 'z fn) (point x y (fn z))])

(define/match (point-map p xfn yfn zfn)
  [((point x y z) xfn yfn zfn) (point (xfn x) (yfn y) (zfn z))])

(define vec-
  (match-lambda* ((list (point x1 y1 z1) (point x2 y2 z2))
                  (point (- x1 x2)
                         (- y1 y2)
                         (- z1 z2)))))

(define vec+
  (match-lambda* ((list (point x1 y1 z1) (point x2 y2 z2))
                  (point (+ x1 x2)
                         (+ y1 y2)
                         (+ z1 z2)))))

(define vec*
  (match-lambda* ((list (point x y z) s)
                  (point (* x s)
                         (* y s)
                         (* z s)))))

(define/match (vec> p1 p2)
  [((point x1 y1 z1) (point x2 y2 z2))
   (and (> x1 x2)
        (> y1 y2)
        (> z1 z2))])

(define/match (vec< p1 p2)
  [((point x1 y1 z1) (point x2 y2 z2))
   (and (< x1 x2)
        (< y1 y2)
        (< z1 z2))])



(define/match (in-point-range range p)
  [((point-range (point minx miny minz) (point maxx maxy maxz))
    (point x y z))
   (and (<= minx x maxx)
        (<= miny y maxy)
        (<= minz z maxz))])

(define/match (point-dist p1 p2)
  [((point x1 y1 z1) (point x2 y2 z2))
   (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2) (expt (- z2 z1) 2)))])

(define/match (point-manhattan-dist p1 pt)
  [((point x1 y1 z1) (point x2 y2 z2))
   (+ (abs (- x2 x1))
      (abs (- y2 y1))
      (abs (- z2 z1)))])

(define (bounds points)
  (define (bounds-helper agg pfun)
    (apply agg (for/list ([p points]) (pfun p))))
  (values (bounds-helper min point-x) (bounds-helper min point-y)
          (bounds-helper max point-x) (bounds-helper max point-y)))

(struct point-range [min max] #:transparent #:constructor-name -point-range)

(define (make-point-range p1 p2)
  (let-values ([(minx miny maxx maxy) (bounds (list p1 p2))])
    (-point-range (point minx miny) (point maxx maxy))))

(define rotation-x (matrix [[1 0 0] [0 0 -1] [0 1 0]]))
(define rotation-y (matrix [[0 0 1] [0 1 0] [-1 0 0]]))
(define rotation-z (matrix [[0 -1 0] [1 0 0] [0 0 1]]))

(define all-rotation-matrices
  (set->list (for*/set ([rotate-x '(0 1 2 3)]
                        [rotate-y '(0 1 2 3)]
                        [rotate-z '(0 1 2 3)])
               (matrix* (matrix-expt rotation-z rotate-z)
                        (matrix-expt rotation-y rotate-y)
                        (matrix-expt rotation-x rotate-x)))))

(define (matrix->point m)
  (point (matrix-ref m 0 0) (matrix-ref m 1 0) (matrix-ref m 2 0)))

(define (rotate-point rotation p)
  (matrix->point (matrix* rotation (col-matrix [(point-x p) (point-y p) (point-z p)]))))

(define (point-rotations p)
  (for/list ([rotation (in-list all-rotation-matrices)])
    (let ([rotated (matrix* rotation (col-matrix [(point-x p) (point-y p) (point-z p)]))])
      (point (matrix-ref rotated 0 0)
             (matrix-ref rotated 1 0)
             (matrix-ref rotated 2 0)))))

(define (point-normalize p)
  (vec* p (/ 1 (point-dist (point 0 0 0) p))))

(define (centroid points)
  (vec* (foldr vec+ (point 0 0 0) points) (/ 1 (length points))))

(define (to-centroid points)
  (let ([centroid (centroid points)])
    (map (λ (p) (vec- p centroid)) points)))

(provide list->point point->list point-map-coord point-map)
(provide bounds)
(provide vec- vec+ vec* vec> vec<)
(provide (struct-out point))
(provide point-range point-range-min point-range-max make-point-range in-point-range point-dist point-rotations all-rotation-matrices rotate-point all-rotation-matrices point-normalize centroid to-centroid point-manhattan-dist)
