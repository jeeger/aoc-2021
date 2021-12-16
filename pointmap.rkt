#lang racket

(require "./util.rkt")
(require racket/contract)

(struct point-map [sizex sizey data] #:constructor-name -point-map)

(define (make-pointmap sizex sizey)
  (-point-map sizex sizey (make-vector (* sizex sizey))))

(define point->index
  (match-lambda* [(list (point-map sx _ _) (point x y))
                  (+ (* sx y) x)]))

(define (pointmap-set! map p value)
  (vector-set! (point-map-data map)
               (point->index map p)
               value)
  map)

(define (pointmap-bounds map)
  (values (point 0 0) (point (point-map-sizex map) (point-map-sizey map))))

(define (pointmap-ref map p)
  (vector-ref (point-map-data map) (point->index map p)))

(define (pointmap-sizex map)
  (point-map-sizex map))
(define (pointmap-sizey map)
  (point-map-sizey map))

(define pointmap-contains
  (match-lambda* [(list (point-map sx sy _)
                        (point x y))
                  (and (< -1 x sx)
                       (< -1 y sy))]))


(define (adjacent-diagonal input p)
  (for/list ([offset
              (list (point -1 -1) (point 0 -1) (point 1 -1) (point 1 0)
                    (point 1 1) (point 0 1) (point -1 1) (point -1 0))]
             #:when (pointmap-contains input (vec+ p offset)))
    (vec+ p offset)))

(define (adjacent input p)
  (for/list ([offset
              (list (point 0 -1) (point 1 0) (point 0 1) (point -1 0))]
             #:when (pointmap-contains input (vec+ p offset)))
    (vec+ p offset)))

(define (pointmap-show m)
  (for* ([y (in-range (pointmap-sizey m))]
         [x (in-range (pointmap-sizex m))])
    (display (~a #:min-width 3 (pointmap-ref m (point x y))))
    (when (eq? x (sub1 (pointmap-sizex m)))
      (display "\n"))))

(define (pointmap-map m fn)
  (-point-map (point-map-sizex m)
              (point-map-sizey m)
              (vector-map fn (point-map-data m))))

(define (pointmap-splice! dest src orig)
  (for* ([y (in-range (pointmap-sizey src))]
         [x (in-range (pointmap-sizex src))])
    (pointmap-set! dest (vec+ orig (point x y))
                   (pointmap-ref src (point x y))))
  dest)


(provide (contract-out
          [make-pointmap (-> number? number? point-map?)]
          [pointmap-set! (-> point-map? point? any/c point-map?)]
          [pointmap-bounds (-> point-map? (values point? point?))]
          [pointmap-map (-> point-map? (-> any/c any/c) point-map?)]
          [pointmap-splice! (-> point-map? point-map? point? point-map?)]
          [pointmap-sizex (-> point-map? number?)]
          [pointmap-sizey (-> point-map? number?)]
          [pointmap-ref (-> point-map? point? any/c)]
          [pointmap-show (-> point-map? void?)]
          [adjacent-diagonal (-> point-map? point? (listof point?))]
          [adjacent (-> point-map? point? (listof point?))]))



