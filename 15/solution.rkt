#lang racket/base

(require "../util.rkt")
(require "../minheap.rkt")
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
  (for*/hash ([y (in-range (length input))]
              [x (in-range (length (car input)))])
    (values (point x y) (list-ref (list-ref input y) x))))

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
              [tentative-distances (for/list ([p (in-list unvisited-neighbors)]) (cons p (+ current-distance (hash-ref input p))))])
         (dijkstra-helper
          (apply minheap-modify (minheap-remove-min unvisited)
                 #:merge (λ (p1 p2)
                           (cons (car p1) (min (cdr p1) (cdr p2))))
                 (map (λ (p) (cons p (+ current-distance (hash-ref input p)))) unvisited-neighbors))
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
    (let-values ([(minx miny maxx maxy) (bounds (hash-keys input))]
                 [(pset) (list->set p)])
      (for* ([y (in-inclusive-range miny maxy)]
             [x (in-inclusive-range minx maxx)])
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
      (hash-ref input p))))

;;(show-solution "input-test2" (point 1 0) (point 2 2))
(solution1 "input")
