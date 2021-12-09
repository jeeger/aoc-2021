#!/usr/bin/racket
#lang racket

(require megaparsack megaparsack/text)
(require (rename-in data/monad (do monad-do)))

(define (test-parser parser string)
  (parse-result! (parse-string parser string)))

(define line/p (many/p digit/p))
(define input/p (many/p line/p #:sep (char/p #\newline)))
(define (parse-input input)
  (parse-result! (parse-string input/p (port->string (open-input-file input)))))

(define char->number (compose string->number string))

(define (tomap input)
  (for*/hash ([y (in-range (length input))]
              [x (in-range (length (car input)))])
    (values (cons x y) (char->number (list-ref (list-ref input y) x)))))

(define (height input p)
  (hash-ref input p 10))

(define *test-input*
  (tomap (parse-input "input-test")))

(define (vec+ p1 p2)
  (cons (+ (car p1) (car p2))
        (+ (cdr p1) (cdr p2))))

(define (lowest? input p)
  (> (apply min (for/list ([n (adjacent input p)])
                  (height input n)))
     (height input p)))

(define (solution1 fname)
  (let* ([raw-input (parse-input fname)]
         [map-input (tomap raw-input)])
    (for*/sum ([y (length raw-input)]
               [x (length (car raw-input))]
               #:when (lowest? map-input (cons x y)))
      (add1 (height map-input (cons x y))))))

(define (adjacent input p)
  (for/list ([offset '((-1 . 0) (0 . -1) (1 . 0) (0 . 1))]
             #:when (hash-has-key? input (vec+ p offset)))
    (vec+ p offset)))

(define (points input)
  (hash-keys input))

(define (find-basin input start)
  ;; Returns a basin
  (define (is-candidate visited p)
    (and (< (hash-ref input p) 9)
         (not (set-member? visited p))))
  (define (find-basin-helper candidates basin)
    (if (empty? candidates)
        basin
        (let* ([p (car candidates)]
               [next-points (filter ((curry is-candidate) basin) (adjacent input p))])
          (find-basin-helper (append next-points (cdr candidates))
                             (set-add basin p)))))
  (find-basin-helper (list start) (set)))


(define (basin-size basin)
  (set-count basin)) 

(define (basins input)
  (define (find-basins possible basins)
    (if (empty? possible)
        basins
        (let ((basin (find-basin input (car possible))))
          (find-basins (filter-not (λ (p) (set-member? basin p)) possible)
                       (cons basin basins)))))
  (find-basins (filter (λ (p) (< (height input p) 9)) (points input)) '()))

(define (solution2 fname)
  (let* ([input (tomap (parse-input fname))]
         [found-basins (basins input)])
    (apply * (take (sort (map basin-size found-basins) >) 3))))
         
