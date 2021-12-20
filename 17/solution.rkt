#lang racket/base
(require "../util.rkt")
(require megaparsack megaparsack/text)
(require (rename-in data/monad [do monad-do]))
(require (only-in data/applicative pure))
(require racket/match)
(require racket/stream)


(define number/p
  (monad-do
   [minus <- (many/p (char/p #\-)#:min 0 #:max 1)]
   [number <- integer/p]
   (pure (if (not (null? minus))
             (- number)
             number))))

(define coord-range/p
  (monad-do
   [number1 <- number/p]
   (string/p "..")
   [number2 <- number/p]
   (pure (cons number1 number2))))

(define input/p
  (monad-do
   (string/p "target area: x=")
   [xrange <- coord-range/p]
   (string/p ", y=")
   [yrange <- coord-range/p]
   (pure (make-point-range (point (car xrange) (car yrange))
                           (point (cdr xrange) (cdr yrange))))))

(define *test-input* (parse-file input/p "input-test"))

(define (xchange x)
  (cond
    [(x . < . 0) (add1 x)]
    [(x . > . 0) (sub1 x)]
    [#t x]))

(define (positions initialvector)
  (let positions-helper ([pos (point 0 0)] [vec initialvector])
    (stream-cons
     (cons pos vec)
     (positions-helper (vec+ pos vec)
                       (point-map vec xchange sub1)))))

(define/match (gone-past? target pos vec)
  [((point-range rmin rmax)
    (point posx posy)
    (point vecx vecy))
   (or (and (< posx (point-x rmin))
            (< vecx 0))
       (and (< posy (point-y rmin))
            (< vecy 0)))])

(define (testfire target initial maxsteps)
  (let ([peaky 0])
    (for/or ([state (positions initial)]
             [step (in-range maxsteps)]
             #:break (gone-past? target (car state) (cdr state)))
      (set! peaky (max (point-y (car state)) peaky))
      (if (in-point-range target (car state))
          peaky
          #f))))

(define (searchpoints distance)
  (for/list ([diff (in-inclusive-range 0 distance)])
    (point diff (- distance diff))))

(define (searchpoints-second distance)
  (let searchpoints-helper ([result '()] [distance distance])
    (if (= distance 0)
        result
        (searchpoints-helper
         (append
          (for/list ([diff (in-inclusive-range 0 distance)])
            (point diff (- distance diff)))
          (for/list ([diff (in-range 0 distance)])
            (point diff (- (- distance diff))))
          result)
         (sub1 distance)))))
             
(define (search target point-range)
  (apply max
   (map (λ (y)
          (or y 0))
        (for*/list
            ([range (in-range point-range)]
             [vector (in-list (searchpoints range))])
          (testfire target vector 250)))))

(define (solution1 fname)
  (search (parse-file input/p fname) 150))

(when (not (eq? (solution1 "input-test") 45))
  (printf ("Solution 1 test solution is incorrect.")))

(define (vector-reaches target initial maxsteps)
  (let vector-reaches-helper ([pos (point 0 0)] [vector initial] [maxsteps maxsteps])
    (cond
      ((= 0 maxsteps)
       (in-point-range target pos))
      ((in-point-range target pos)
       #t)
      (#t (vector-reaches-helper (vec+ pos vector)
                                 (point-map vector xchange sub1)
                                 (sub1 maxsteps))))))

(define (search-count target point-range)
  (for*/list ([searchvector (in-list (searchpoints-second point-range))]
              #:when (vector-reaches target searchvector 500))
    searchvector))

(define (solution2 fname)
  (length (search-count (parse-file input/p fname) 500)))
