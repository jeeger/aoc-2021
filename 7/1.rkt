#lang racket

(define (read-input fname)
  (sort (map string->number (string-split (port->string (open-input-file fname)) ",")) <))

(define (delta x y)
  (abs (- x y)))

(define (fuel-cost positions target)
  (for/sum ([position positions]) (delta position target)))

(define (increased-fuel-cost positions target)
  (for/sum ([position positions])
    (let ((diff (delta position target)))
      (/ (+ (* diff diff) diff) 2))))

(define (median l)
  (let ((sorted (sort l <)))
    (if (= (modulo (length l) 2) 0)
        (/ (+ (list-ref sorted (quotient (length l) 2))
              (list-ref sorted (sub1 (quotient (length l) 2))))
           2)
      (list-ref sorted (quotient (length l) 2)))))

(define (fuel-costs positions targets)
  (map ((curry fuel-cost) positions) targets))

(define (increased-fuel-costs positions targets)
  (map ((curry increased-fuel-cost) positions) targets))

(define (solution1 fname)
  (let ([input (read-input fname)])
    (fuel-cost input (median input))))


(define (position-bounds input)
  (values (apply min input)
          (apply max input)))

(define (solution2 fname)
  (let*-values ([(input) (read-input fname)]
               [(l r) (position-bounds input)])
    (apply min (increased-fuel-costs input (sequence->list (in-inclusive-range l r))))))
