#lang racket/base
(require racket/contract)
(require racket/vector)
(require racket/format)
(require racket/match)
(require (only-in racket/function identity const))

(struct minheap [size valuefn elemfn data]
  #:constructor-name -minheap)

(define (vec-safe vec size idx default)
  (if (< idx size)
      (vector-ref vec idx)
      default))

;; Children of 0: 1 2
;; Children of 1: 3 4
;; Children of 2: 5 6
(define (vec-children size vec idx)
  (let ([c1 (sub1 (* (add1 idx) 2))]
        [c2 (* (add1 idx) 2)])
    (list (vec-safe vec size c1 #f)
          (vec-safe vec size c2 #f))))

;; Parent of 0: None
;; Parent of 1: 0
;; Parent of 2: 0
;; Parent of 3: 1
;; Parent of 4: 1
;; Parent of 5: 2
;; Parent of 6: 2
(define (vec-parent vec idx)
  (if (= 0 idx)
      (error "Getting parent of idx 0")
      (vector-ref vec
                  (quotient (sub1 idx) 2))))


(define (parent-idx idx)
  (quotient (sub1 idx) 2))

(define (children-idx idx)
  (list (sub1 (* (add1 idx) 2))
        (* (add1 idx) 2)))


;; Lastlevel 1: 0
;; Lastlevel 2: 1
;; Lastlevel 3: 1
;; Lastlevel 4: 2
;; Lastlevel 5: 2
(define (lastlevel size)
  (if (eq? size 0)
      (error "Last level of empty heap")
      (sub1 (integer-length size))))

;; Level index 0: 0
;; Level index 1: 1
;; Level index 2: 3
;; Level index 3: 7
(define (level-start level)
  (sub1 (arithmetic-shift 1 level)))

(define (level-end level)
  (sub1 (level-start (add1 level))))
      
(define (vector-swap! vec idx1 idx2)
  (let ([tmp (vector-ref vec idx1)])
    (vector-set! vec idx1 (vector-ref vec idx2))
    (vector-set! vec idx2 tmp))
  vec)

(define (swapup minheap idx)
  (if (= idx 0)
      minheap
      (let* ([pidx (parent-idx idx)]
             [extract (minheap-valuefn minheap)]
             [parentval (extract (vector-ref (minheap-data minheap) pidx))]
             [idval (extract (vector-ref (minheap-data minheap) idx))]
             [data (minheap-data minheap)])
        (if (> parentval idval)
            (begin
              (vector-swap! data pidx idx)
              (swapup minheap pidx))
            minheap))))

(define (swapdown minheap idx)
  (match-let ([(list cv1 cv2) (vec-children (minheap-size minheap) (minheap-data minheap) idx)]
              [(list ci1 ci2) (children-idx idx)]
              [parent (vector-ref (minheap-data minheap) idx)]
              [data (minheap-data minheap)]
              [extract (minheap-valuefn minheap)])
    (cond
      ;; Happy case
      ((and cv1 cv2 (<= (extract parent) (extract cv1)) (<= (extract parent) (extract cv2)))
       minheap)
      ;; Swap parent and c2
      ((and cv1 cv2 (<= (extract parent) (extract cv1)) (> (extract parent) (extract cv2)))
       (vector-swap! data idx ci2)
       (swapdown minheap ci2))
      ((and cv1 cv2 (<= (extract parent) (extract cv2)) (> (extract parent) (extract cv1)))
       (vector-swap! data idx ci1)
       (swapdown minheap ci1))
      ((and cv1 cv2
            (> (extract parent) (extract cv1))
            (> (extract parent) (extract cv2))
            (< (extract cv1) (extract cv2)))
       (vector-swap! data idx ci1)
       (swapdown minheap ci1))
      ((and cv1 cv2
            (> (extract parent) (extract cv1))
            (> (extract parent) (extract cv2))
            (< (extract cv2) (extract cv1)))
       (vector-swap! data idx ci2)
       (swapdown minheap ci2))
      ((and cv1 cv2
            (> (extract parent) (extract cv1))
            (> (extract parent) (extract cv2))
            (< (extract cv1) (extract cv2)))
       (vector-swap! data idx ci1)
       (swapdown minheap ci1))
      ((and cv1 (> (extract parent) (extract cv1)))
       (vector-swap! data idx ci1)
       (swapdown minheap ci1))
      ((and cv2 (> (extract parent) (extract cv2)))
       (vector-swap! data idx ci2)
       (swapdown minheap ci2))
      (#t minheap))))
       
(define (make-minheap [data #()] [cmp identity] [elemfn identity])
  (if (vector-empty? data)
      (-minheap 0 cmp elemfn (make-vector 0))
      (let ([result (-minheap (vector-length data) cmp elemfn (vector-copy data))]
            [startindex (level-end (sub1 (lastlevel (vector-length data))))])
        (for ([idx (in-inclusive-range startindex 0 -1)])
          (swapdown result idx))
        result)))

(define (minheap-grow minheap)
  (let ([newdata (make-vector (* 2 (max 1 (vector-length (minheap-data minheap)))))])
    (vector-copy! newdata 0 (minheap-data minheap) 0)
    (-minheap (minheap-size minheap) (minheap-valuefn minheap) (minheap-elemfn minheap) newdata)))

(define (minheap-needs-extending? minheap)
  (= (minheap-size minheap) (vector-length (minheap-data minheap))))

(define (minheap-insert minheap . elems)
  (letrec ([insert-helper
         (lambda (elem minheap)
           (if (minheap-needs-extending? minheap)
               (insert-helper elem (minheap-grow minheap))
               (let ([data (minheap-data minheap)])
                 (vector-set! data (minheap-size minheap) elem)
                 (swapup
                  (-minheap (add1 (minheap-size minheap)) (minheap-valuefn minheap) (minheap-elemfn minheap) data)
                  (minheap-size minheap)))))])
    (foldl insert-helper minheap elems)))

(define (minheap-empty? minheap)
  (eq? (minheap-size minheap) 0))

(define (minheap-remove-min minheap)
  (if (minheap-empty? minheap)
      (error "Removing element from empty heap.")
      (begin
        (vector-set! (minheap-data minheap) 0 (vector-ref (minheap-data minheap) (sub1 (minheap-size minheap))))
        (swapdown (-minheap (sub1 (minheap-size minheap))
                            (minheap-valuefn minheap)
                            (minheap-elemfn minheap)
                            (minheap-data minheap)) 0))))

(define (minheap-min minheap)
  ((minheap-elemfn minheap) (vector-ref (minheap-data minheap) 0)))

(define (minheap-min-val minheap)
  ((minheap-valuefn minheap) (vector-ref (minheap-data minheap) 0)))

(define (take-second _one two) two)
(define (minheap-modify minheap #:merge [mergefn take-second] . tofinds)
  (define (minheap-modify-helper tofind minheap)
    (let ([index
           (for/or ([elem (in-vector (minheap-data minheap))]
                    [index (in-range (minheap-size minheap))])
             (if (equal? ((minheap-elemfn minheap) elem) ((minheap-elemfn minheap) tofind))
                 (cons elem index)
                 #f))])
      (if (not index)
          (minheap-insert minheap tofind)
          (let* ([old-value ((minheap-valuefn minheap) (car index))]
                 [new-data (mergefn (car index) tofind)]
                 [new-value ((minheap-valuefn minheap) new-data)])
            (begin
              (vector-set! (minheap-data minheap) (cdr index) new-data)
              (if (< new-value old-value)
                  (swapup minheap (cdr index))
                  (swapdown minheap (cdr index))))))))
  (foldl minheap-modify-helper minheap tofinds))

(provide (contract-out [make-minheap (->* () (vector? (-> any/c number?) (-> any/c any/c)) minheap?)]
                       [minheap-modify (->* (minheap?) (#:merge (-> any/c any/c any/c)) #:rest (listof any/c) minheap?)]
                       [minheap-empty? (-> minheap? boolean?)]
                       [minheap-min (-> minheap? any/c)]
                       [minheap-min-val (-> minheap? number?)]
                       [minheap-size (-> minheap? number?)]
                       [minheap-remove-min (-> minheap? minheap?)]
                       [minheap-insert (->* (minheap?) () #:rest (listof any/c) minheap?)]))
