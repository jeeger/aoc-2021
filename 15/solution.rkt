#lang racket/base

(require (only-in "../util.rkt"
                  point point-x point-y parse-file char->number newline/p))
(require "../minheap.rkt")
(require "../pointmap.rkt")
(require (rename-in data/monad [do monad-do]))
(require (only-in data/applicative pure))
(require megaparsack)
(require (only-in megaparsack/text digit/p))
(require racket/set)
(require racket/hash)
(require racket/format)
(require racket/match)
(require charterm)

(define line/p
  (monad-do
   [line <- (many/p digit/p)]
   (pure (map char->number line))))

(define input/p (many/p line/p #:sep newline/p))

(define (make-hash input)
  (let* ([xsize (length (car input))]
         [ysize (length input)]
         [result (make-pointmap xsize ysize)])
    (for ([yl (in-list input)]
          [y (in-range ysize)])
      (for ([v (in-list yl)]
            [x (in-range xsize)])
        (pointmap-set! result (point x y) v)))
    result))

(define *test-input* (make-hash (parse-file input/p "input-test")))

(define (extend-results resultmap lastpoint tentative-distances)
  (define (extend-result-helper distancepair resultmap)
    (match-let*
        ([(list-rest nextpoint new-distance) distancepair]
         [(list-rest oldpoint old-distance) (hash-ref resultmap nextpoint (cons #f (expt 2 30)))])
      (if (< new-distance old-distance)
          (hash-set resultmap nextpoint (cons lastpoint new-distance))
          resultmap)))
  (foldr extend-result-helper resultmap tentative-distances))


(define (dijkstra input from to)
  (define (dijkstra-helper unvisited visited resultmap)
    (cond
      ((minheap-empty? unvisited)
       (error (~a "Unreachable point " to)))
      ((equal? (minheap-min unvisited) to)
       resultmap)
      (#t
       (let* ([current-node (minheap-min unvisited)]
              [current-distance (minheap-min-val unvisited)]
              [unvisited-neighbors
               (filter (λ (p) (not (set-member? visited p)))
                       (adjacent input current-node))]
              [tentative-distances (for/list ([p (in-list unvisited-neighbors)]) (cons p (+ current-distance (pointmap-ref input p))))])
         (dijkstra-helper
          (apply minheap-modify (minheap-remove-min unvisited)
                 #:merge (λ (p1 p2)
                           (cons (car p1) (min (cdr p1) (cdr p2))))
                 (map (λ (p) (cons p (+ current-distance (pointmap-ref input p)))) unvisited-neighbors))
          (set-add visited current-node)
          (extend-results resultmap current-node tentative-distances))))))
  (dijkstra-helper
   (make-minheap (vector (cons from 0)) cdr car)
   (set) (hash from (cons 0 #t))))

(define (pathmap->path pathmap from to)
  (define (pathmap-helper to result)
    (if (equal? from to)
        (cons from result)
        (pathmap-helper (car (hash-ref pathmap to)) (cons to result))))
  (pathmap-helper to '()))

(define (show-path input p)
  (with-charterm
    (charterm-clear-screen)
    (let ([maxx (pointmap-sizex input)]
          [maxy (pointmap-sizey input)]
          [pset (list->set p)])
      (for* ([y (in-inclusive-range 0 maxy)]
             [x (in-inclusive-range 0 maxx)])
        (charterm-cursor (add1 x) (add1 y))
        (when (set-member? pset (point x y))
          (charterm-inverse))
        (charterm-display (hash-ref input (point x y)))
        (charterm-normal)))))

(define (path input from to)
  (pathmap->path (dijkstra input from to) from to))

(define (show-solution fname from to)
  (let* ([raw-input (parse-file input/p fname)]
         [input (make-hash raw-input)]
         [path (path input from to)])
    (show-path input path)))

(define (file->path fname from to)
  (let* ([raw-input (parse-file input/p fname)]
         [input (make-hash raw-input)])
    (path input from to)))

(define (solution1 fname)
  (let* ([raw-input (parse-file input/p fname)]
         [input (make-hash raw-input)]
         [maxpoint (point (sub1 (length (car raw-input))) (sub1 (length raw-input)))]
         [path (path input (point 0 0) maxpoint)])
    (for/sum ([p (in-list (cdr path))])
      (pointmap-ref input p))))

;;(show-solution "input-test2" (point 1 0) (point 2 2))
(when (not (eq? (solution1 "input") 583))
  (printf "Solution 1 no longer correct."))

(define (tile-input pointmap tilex tiley)
  (let ([result (make-pointmap (* tilex (pointmap-sizex pointmap))
                               (* tiley (pointmap-sizey pointmap)))])
    (for* ([xtile (in-range tilex)]
           [ytile (in-range tiley)])
      (pointmap-splice!
       result
       (pointmap-map pointmap (λ (v) (add1 (modulo (+ xtile ytile v -1) 9))))
       (point (* xtile (pointmap-sizex pointmap))
              (* ytile (pointmap-sizey pointmap)))))
    result))

(define (solution2 fname)
  (let* ([raw-input (parse-file input/p fname)]
         [input
          (tile-input (make-hash raw-input) 5 5)]
         [maxpoint (point (sub1 (pointmap-sizex input)) (sub1 (pointmap-sizey input)))]
         [path (path input (point 0 0) maxpoint)])
    (for/sum ([p (in-list (cdr path))])
      (pointmap-ref input p))))
         

(when (not (eq? (solution2 "input-test") 315))
  (printf "Solution 2 not correct."))
