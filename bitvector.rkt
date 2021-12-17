#lang racket/base
(require racket/vector)
(require racket/sequence)
(require racket/contract)

(struct bv [length data] #:constructor-name -bv)

(define (make-bitvector-from-hex-string string)
  (make-bitvector-from-hex (list->vector (string->list string))))

(define (make-bitvector-from-hex hexdigits)
  (let* ([resultlength (* 4 (vector-length hexdigits))]
         [resultvector (make-vector resultlength #f)]
         [result (-bv resultlength resultvector)])
    (for ([digit (sequence-map (λ (digit) (string->number (string digit) 16)) (in-vector hexdigits))]
          [index (in-range (vector-length hexdigits))])
      (for ([offset (in-range 4)]
            [mask (in-list (list 8 4 2 1))])
        (vector-set! resultvector (+ (* index 4) offset) (< 0 (bitwise-and digit mask)))))
    result))

(define (make-bitvector-from-bits bits)
  (-bv (length bits) (list->vector bits)))

(define (bits->number bits)
  (for/sum ([bit (in-vector bits)]
            [scale (sequence-map (λ (x) (expt 2 x)) (in-inclusive-range (sub1 (vector-length bits)) 0 -1))])
    (if bit
        scale
        0)))

(define (bv-bits bv length)
  (values (vector-take (bv-data bv) length)
          (-bv (- (bv-length bv) length)
               (vector-drop (bv-data bv) length))))

(define (bv-number bv length)
  (let-values ([(bits rest) (bv-bits bv length)])
    (values (bits->number bits) rest)))

(define (bitvector-length bv)
  (bv-length bv))

(define (bitvector-empty? bv)
  (= (bv-length bv 0)))

(provide (contract-out
          [make-bitvector-from-hex (-> (vectorof char?) bv?)]
          [make-bitvector-from-bits (-> (vectorof boolean?) bv?)]
          [make-bitvector-from-hex-string (-> string? bv?)]
          [bits->number (-> (vectorof boolean?) number?)]
          [rename bv-number bitvector-read-number (->* (bv? number?) () (values number? bv?))]
          [rename bv-bits bitvector-read-bits (->* (bv? number?) () (values (vectorof boolean?) bv?))]
          [bitvector-length (-> bv? number?)]
          [bitvector-empty? (-> bv? boolean?)])
         (rename-out [bv? bitvector?]))
         
