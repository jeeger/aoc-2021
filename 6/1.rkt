#lang racket

(define (read-input fname)
  (let ((result (make-hash)))
  (for ([fish (map string->number (string-split (port->string (open-input-file fname)) ","))])
    (hash-update! result fish add1 0))
  result))

(define (fish-count fishes)
  (apply + (hash-values fishes)))

(define (show-fishes fishes)
  (for ([fish (in-hash-pairs fishes)])
    (display (~a (cdr fish) " fishes of age " (car fish) "\n"))))


(define (age-fishes fishes)
  (for/hash ([fish (in-hash-pairs fishes)])
    (values (sub1 (car fish)) (cdr fish))))

(define (add-fishes fishes . toadd)
  (foldr (λ (toadd fishes) (hash-update fishes (car toadd) (λ (count) (+ count (cdr toadd))) 0)) fishes toadd))

(define (spawn-fishes fishes)
  (let ((spawning-fishes (hash-ref fishes -1 0)))
    (if (> spawning-fishes 0)
        (hash-remove (add-fishes fishes (cons 6 spawning-fishes) (cons 8 spawning-fishes)) -1)
        fishes)))

(define (tick-repeatedly fishes days)
  (foldr (lambda (arg fishes) (tick fishes)) fishes (range days)))

(define (tick-debug fishes days)
  (show-fishes (tick-repeatedly fishes days)))

(define tick (compose spawn-fishes age-fishes))
(define solution1 (fish-count (tick-repeatedly (read-input "input") 80)))
(define solution2 (fish-count (tick-repeatedly (read-input "input") 256)))
