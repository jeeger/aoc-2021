#lang racket
(require data/bit-vector)

(define (valid-line line)
  (and (non-empty-string? line)
       (not (string-prefix? line "#"))))

(define (read-problem port)
  (for/list ([line (in-lines port)] #:when (valid-line line))
    (apply bit-vector (map (lambda (x) (equal? x #\1)) (string->list line)))))

(define (count-set-bits l)
  (let ((count-set-bits-helper
         (lambda (bv count)
           (for/list [(bit bv)
                      (index (in-range (bit-vector-length bv)))]
             (+ (list-ref count index) (if bit 1 0))))))
    (foldr count-set-bits-helper (for/list [(in-range (bit-vector-length (car l)))] 0) l)))

(define (bit-criterion comp)
  (lambda (input)
    (let [(set-bits (count-set-bits input))
          (size (length input))]
      (for/bit-vector [(elem (in-list set-bits))] (comp elem (/ size 2))))))

(define (tonum bv)
  (for/sum [(elem (in-bit-vector bv))
            (pos (in-range (- (bit-vector-length bv) 1) -1 -1))
            #:when elem]
    (expt 2 pos)))


(define (filter-by-criterion-bit index criterion l)
  (filter (lambda (elem) (eq? (bit-vector-ref criterion index)
                              (bit-vector-ref elem index))) l))

(define (filter-by-criterion criterion input)
  (letrec ((criterion-helper (lambda (index input)
                               (if (eq? (length input) 1)
                                   (car input)
                                   (let ((criterion-values (criterion input)))
                                     (criterion-helper (+ 1 index) (filter-by-criterion-bit index criterion-values input)))))))
    (criterion-helper 0 input)))

(define (solution filename)
  (let [(input (read-problem (open-input-file filename)))]
    (* (tonum (filter-by-criterion (bit-criterion >=) input))
       (tonum (filter-by-criterion (bit-criterion <) input)))))

(define (show-input input)
  (printf "~a~n" (map bit-vector->string input)))


