#!/usr/bin/racket
#lang racket

(require megaparsack megaparsack/text)

(define input/p (many/p (repeat/p 10 digit/p) #:min 10 #:max 10 #:sep (char/p #\newline)))
(define (parse-input fname)
  (parse-result! (parse-string input/p (port->string (open-input-file fname)))))

(define char->number (compose string->number string))

(define (read-input input)
  (let ([parsed-input (parse-input input)])
    (for*/hash ([y (in-range (length parsed-input))]
                [x (in-range (length (list-ref parsed-input y)))])
      (values (cons x y) (char->number (list-ref (list-ref parsed-input y) x))))))

(define *test-input* (read-input "input-test"))
(define *small-test-input* #hash(((0 . 0) . 0)
                                 ((1 . 0) . 0)
                                 ((2 . 0) . 0)
                                 ((0 . 1) . 0)
                                 ((1 . 1) . 10)
                                 ((2 . 1) . 9)
                                 ((0 . 2) . 0)
                                 ((1 . 2) . 0)
                                 ((2 . 2) . 0)))
(define (vec+ p1 p2)
  (cons (+ (car p1) (car p2))
        (+ (cdr p1) (cdr p2))))

(define (adjacent input p)
  (for/list ([offset
              '((-1 . -1) (0 . -1) (1 . -1) (1 . 0) (1 . 1)
                          (0 . 1) (-1 . 1) (-1 . 0))]
             #:when (hash-has-key? input (vec+ p offset)))
    (vec+ p offset)))

(define (gain-energy-all input)
  (for/hash ([pair (in-hash-pairs input)])
    (values (car pair) (add1 (cdr pair)))))

(define (find-flashable-points input)
  (for/list ([pair (in-hash-pairs input)]
             #:when (> (cdr pair) 9))
    (car pair)))

(define (charge input p)
  (hash-ref input p))
(define (flashable? input p)
  (> (charge input p) 9))

(define (flash input)
  (define (flash-helper input flashable flashed)
    (if (set-empty? flashable)
        (values input flashed)
        (let* ([toflash (set-first flashable)]
               [adjacent (adjacent input toflash)]
               [next-input (foldr (lambda (p input) (hash-update input p add1)) input adjacent)]
               [newly-flashable (list->set (filter (lambda (p) (and (flashable? next-input p) (not (set-member? flashed p)))) adjacent))])
          (flash-helper next-input
                        (set-union newly-flashable (set-rest flashable))
                        (set-add flashed toflash)))))
  (flash-helper input (list->set (find-flashable-points input)) (set)))

(define (step input flashed-fn)
  (let-values ([(next-input flashed) (flash (gain-energy-all input))])
    (values (foldr (lambda (p input) (hash-set input p 0)) next-input (set->list flashed)) (flashed-fn flashed))))

(define (flashes-after input n)
  (define (flashes-after-helper input n flashcount)
    (if (eq? n 0)
        flashcount
        (let-values (((next-input flashed) (step input set-count)))
          (flashes-after-helper next-input (sub1 n) (+ flashed flashcount)))))
  (flashes-after-helper input n 0))

(define (solution1 fname)
  (flashes-after (read-input fname) 100))

(define (show-octopi input)
  (let ([max-x (apply max (for/list ([p (in-hash-pairs input)])
                            (caar p)))]
        [max-y (apply max (for/list ([p (in-hash-pairs input)])
                 (cdar p)))])
    (for* ([y (in-inclusive-range 0 max-y)]
           [x (in-inclusive-range 0 max-x)])
      (display (~a " " (hash-ref input (cons x y)) " " #:min-width 5 #:align 'center))
      (when (eq? x max-x)
        (display "\n")))))

(define (synchronized input)
  (define (synchronized-helper input n)
    (let-values (((next-input synchronized) (step input (lambda (flashed) (eq? (set-count flashed) (length (hash-values input)))))))
      (if synchronized
          n
          (synchronized-helper next-input (add1 n)))))
  (synchronized-helper input 1))

(define (solution2 fname)
  (synchronized (read-input fname)))

