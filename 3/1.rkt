#lang racket
(require data/bit-vector)

(define (valid-line line)
  (and (non-empty-string? line)
       (not (string-prefix? line "#"))))

(define (read-problem port)
  (for/list ([line (in-lines port)] #:when (valid-line line))
    (apply bit-vector (map (lambda (x) (equal? x #\1)) (string->list line)))))

(define (count-bits bv count)
  (for/list [(bit bv)
             (index (in-range (bit-vector-length bv)))]
    (+ (list-ref count index) (if bit 1 0))))

(define (most-common l size)
  (map (λ (x) (> x (/ size 2))) l))

(define (least-common l size)
  (map (λ (x) (<= x (/ size 2))) l))

(define (tonum l)
  (for/sum [(elem (in-list (reverse l)))
            (pos (in-range (length l)))
            #:when elem]
    (expt 2 pos)))

(define (solution file)
  (let* [(input (read-problem (open-input-file file)))
         (word-width (bit-vector-length (car input)))
         (input-size (length input))
         (counted (foldr count-bits (for/list [(in-range word-width)] 0) input))]
    (printf "~a" (* (tonum (most-common counted input-size))
                    (tonum (least-common counted input-size))))))

(solution "1")
