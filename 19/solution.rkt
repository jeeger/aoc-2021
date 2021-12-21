#lang racket
(require (prefix-in base: racket/base))
(require megaparsack megaparsack/text)
(require data/monad)
(require data/applicative)
(require math/matrix)
(require racket/hash)
(require "../util.rkt")
(require "../3dpoint.rkt")

(struct scanner [index reports] #:transparent)
(struct transformation [rotation offset] #:transparent)
(define identity-transformation (transformation (identity-matrix 3) (point 0 0 0)))

(define coordinate/p
  (do
      [coordinates <- (many/p number/p #:sep (char/p #\,) #:min 3 #:max 3)]
      newline/p
    (pure (list->point coordinates))))

(define scanner/p
  (do
   (string/p "--- scanner ")
   [index <- number/p]
   (string/p " ---\n")
   [reports <- (many+/p coordinate/p)]
   (pure (scanner index reports))))

(define input/p
  (many/p scanner/p #:sep newline/p))

(define *test-input* (parse-file input/p "input-test"))
(define *simple-test-input* (parse-file input/p "input-test-3"))
(define *overlapping-test-input* (parse-file input/p "input-overlapping"))

(define (rotate-points rotation points)
  (map (curry rotate-point rotation) points))

(define (show-rotation matrix)
  (for* ([y (in-range 0 3)]
         [x (in-range 0 3)])
    (display (~a #:min-width 3 #:align 'right (matrix-ref matrix y x)))
    (when (eq? x 2)
      (display "\n")))
  (display "\n"))

(define (find-rotated-point-overlap points1 points2)
  (let ([normalized-one (to-centroid points1)]
        [normalized-two (to-centroid points2)])
    (let find-rotated-overlap ([rotations all-rotation-matrices])
      (if (null? rotations)
          #f
          (let ([rotated (rotate-points (car rotations) normalized-two)])
            (if (> (length (set-intersect normalized-one rotated)) 0)
                (set-intersect normalized-one rotated)
                (find-rotated-overlap (cdr rotations))))))))


(define (count-overlaps p points)
  (let overlap-helper ([points points] [result (hash)])
    (if (null? points)
        result
        (let ([offset (vec- (car points) p)])
          (overlap-helper (cdr points) (hash-update result offset add1 0))))))

(define (find-matching-offset h)
  (for/or ([(key value) (in-hash h)])
    (if (value . >= . 12)
        key
        #f)))

(define (find-offsets rotation points1 points2)
  (let ([rotated (rotate-points rotation points2)])
    (let offset-helper ([points rotated] [result (hash)])
      (if (null? points)
          #f
          (let* ([next-offsets (hash-union result (count-overlaps (car points) points1) #:combine +)]
                 [matching-offset (find-matching-offset next-offsets)])
            (if matching-offset
                (transformation rotation matching-offset)
                (offset-helper (cdr points) next-offsets)))))))
            
(define (find-offset points1 points2)
  (let find-rotated-overlap ([rotations all-rotation-matrices])
    (if (null? rotations)
        #f
        (or (find-offsets (car rotations) points1 points2)
            (find-rotated-overlap (cdr rotations))))))

(define (normalize-points transformation points)
  (for/list ([point (in-list (rotate-points (transformation-rotation transformation) points))])
    (vec+ (transformation-offset transformation) point)))

(define/match (normalize-scanner _transformation _scanner)
  [(transformation (scanner idx reports))
   (scanner idx (normalize-points transformation reports))])

(define (find-position s1 s2)
  (find-offset (scanner-reports s1) (scanner-reports s2)))

(define (find-new-positions positions left rights)
  (if (not (hash-has-key? positions (scanner-index left)))
      positions
      (let* ([left-id (scanner-index left)]
             [left-position (hash-ref positions left-id)]
             [left-normalized (normalize-scanner left-position left)])
        (foldl (λ (right result)
                 (if (hash-has-key? positions (scanner-index right))
                     result
                     (let ([new-position (find-position left-normalized right)])
                       (if new-position
                           (hash-set result (scanner-index right) new-position)
                           result))))
               positions rights))))

(define (find-all-positions scanners)
  (let position-helper ([lefts scanners]
                        [positions (hash (scanner-index (car scanners)) identity-transformation)])
    (cond
      [(= (hash-count positions) (length scanners))
           positions]
      [(null? lefts)
       (position-helper scanners positions)]
      [#t 
       (position-helper (cdr lefts)
                        (find-new-positions positions (car lefts) scanners))])))

(define (find-beacons scanners)
  (let ([positions (find-all-positions scanners)])
    (for/fold ([result (set)]) ([scanner scanners])
      (set-union result (list->set (scanner-reports (normalize-scanner (hash-ref positions (scanner-index scanner)) scanner)))))))
       
(define (solution1 fname)
  (set-count (find-beacons (parse-file input/p fname))))

(define (solution2 fname)
  (let ([positions (find-all-positions (parse-file input/p fname))])
    (apply max (for*/list ([left (in-hash-values positions)]
                           [right (in-hash-values positions)])
                 (point-manhattan-dist (transformation-offset left)
                                       (transformation-offset right))))))
