#lang racket
(require (prefix-in base: racket/base))
(require "../util.rkt")
(require (only-in "../2dpoint.rkt" vec+ point bounds))
(require "../pointmap.rkt")
(require (only-in "../bitvector.rkt" bits->number))

(require megaparsack megaparsack/text)
(require data/applicative data/monad)


(define pixel/p
  (do
      [pixel <- (one-of/p (list #\. #\#))]
      (pure (if (eq? pixel #\.)
                #f
                #t))))

(define algorithm/p
  (many/p pixel/p))

(struct sparsemap [default image] #:transparent)
(define/match (sparsemap-set! map point value)
 [((sparsemap default image) point value) (when (not (eq? value default))
                                             (set-add! image point))
                                           map])
(define (empty-sparsemap default)
  (sparsemap default (mutable-set)))
(define/match (sparsemap-bounds map)
  [((sparsemap _default image)) (bounds (set->list image))])
(define/match (sparsemap-ref map point)
  [((sparsemap default image) point) (xor (set-member? image point) default)])
(define/match (sparsemap-count-set map)
  [((sparsemap default image)) (if default
                                   (error "Infinitely many pixels.")
                                   (set-count image))])

(define (list->sparsemap l)
  (let ([result (empty-sparsemap #f)])
    (for ([y (in-list l)]
          [yindex (in-range (length l))])
      (for ([x (in-list y)]
            [xindex (in-range (length y))]
            #:when x)
        (sparsemap-set! result (point xindex yindex) #t)))
    result))

(define input/p
  (do
      [algorithm <- algorithm/p]
      newline/p
    newline/p
    [image <- (many/p algorithm/p #:sep newline/p)]
    (pure (cons (list->vector algorithm) (list->sparsemap image)))))

(define *test-input* (parse-file input/p "input-test"))

(define (pixelmask p)
  (for/list ([offset
              (list (point -1 -1) (point 0 -1) (point 1 -1)
                    (point -1 0) (point 0 0) (point 1 0)
                    (point -1 1) (point 0 1) (point 1 1))])
    (vec+ p offset)))

(define (index image pixel)
  (let ([adjacent-points (pixelmask pixel)])
    (bits->number (list->vector (map (λ (point) (sparsemap-ref image point)) adjacent-points)))))

(define (show-sparsemap image)
  (display (~a "Default value: " (sparsemap-default image) "\n"))
  (let-values ([(minx miny maxx maxy) (sparsemap-bounds image)])
    (for* ([y (in-inclusive-range miny maxy)]
           [x (in-inclusive-range minx maxx)])
      (if (sparsemap-ref image (point x y))
          (display "#")
          (display "."))
      (when (eq? x maxx)
        (display "\n")))))

(define (next-default algorithm sparsemap)
  (vector-ref algorithm (bits->number (make-vector 9 (sparsemap-default sparsemap)))))

(define (enhance algorithm image)
  (let-values ([(minx miny maxx maxy) (sparsemap-bounds image)])
    (let ([result (empty-sparsemap (next-default algorithm image))])
      (for* ([y (in-inclusive-range (- miny 3) (+ maxy 3))]
             [x (in-inclusive-range (- minx 3) (+ maxx 3))])
        (sparsemap-set! result (point x y) (vector-ref algorithm (index image (point x y)))))
      result)))

(define (enhance-n algorithm image count)
  (let enhance-helper ([image image] [count count])
    (if (= count 0)
        image
        (enhance-helper (enhance algorithm image) (sub1 count)))))

(define (show-enhance fname count)
  (match-let ([(list-rest algorithm image) (parse-file input/p fname)])
    (show-sparsemap (enhance-n algorithm image count))))

(define (solution1 fname)
  (match-let ([(list-rest algorithm image) (parse-file input/p fname)])
    (sparsemap-count-set (enhance-n algorithm image 2))))
             
(define (solution2 fname)
  (match-let ([(list-rest algorithm image) (parse-file input/p fname)])
    (sparsemap-count-set (enhance-n algorithm image 50))))
