#lang racket

(require megaparsack megaparsack/text)
(require (rename-in data/monad (do monad-do)))
(require data/applicative)

(struct vent [x1 y1 x2 y2] #:constructor-name -vent)

(define parse-vent/p
  (monad-do [num1 <- integer/p]
      (char/p #\,)
    [num2 <- integer/p]
    (string/p " -> ")
    [num3 <- integer/p]
    (char/p #\,)
    [num4 <- integer/p]
    (pure (-vent num1 num2 num3 num4))))

(define parse-solution/p
  (many/p parse-vent/p #:sep (char/p #\newline)))

(define (straight? v)
  (xor (= (vent-x1 v) (vent-x2 v))
       (= (vent-y1 v) (vent-y2 v))))

(define (min-x v)
  (min (vent-x1 v) (vent-x2 v)))

(define (min-y v)
  (min (vent-y1 v) (vent-y2 v)))

(define (max-x v)
  (max (vent-x1 v) (vent-x2 v)))

(define (max-y v)
  (max (vent-y1 v) (vent-y2 v)))

(define (vent-p1 v)
  (cons (vent-x1 v)
        (vent-y1 v)))

(define (vent-p2 v)
  (cons (vent-x2 v)
        (vent-y2 v)))

(define (crosses? v x y)
  (and (<= (min-x v) x (max-x v))
       (<= (min-y v) y (max-y v))))

(define (read-vents filename)
  (parse-result! (parse-string parse-solution/p (port->string (open-input-file filename)))))

(define (show-vent v)
  (display (~a (vent-x1 v) "," (vent-y1 v) "->" (vent-x2 v) "," (vent-y2 v) #\newline)))

(define (find-value-x f vs)
  (apply f (map (λ (v) (f (vent-x1 v) (vent-x2 v))) vs)))

(define (find-value-y f vs)
  (apply f (map (λ (v) (f (vent-y1 v) (vent-y2 v))) vs)))

(define (delta l r)
  (abs (- l r)))

(define (plot-line-low p0 p1)
  (let* ((dx (delta (car p1) (car p0)))
         (dy (delta (cdr p1) (cdr p0)))
         (yi (if (< (- (cdr p1) (cdr p0)) 0) -1 1)))
    (do ([D (- (* 2 dy) dx)]
         [result '()]
         [x (car p0)]
         [y (cdr p0)])
        ((> x (car p1)) result)
      (set! result (cons (cons x y) result))
      (set! x (+ x 1))
      (if (> D 0)
          (begin
            (set! y (+ y yi))
            (set! D (+ D (* 2 (- dy dx)))))
          (set! D (+ D (* 2 dy)))))))

(define (plot-line-high p0 p1)
  (let* ((dx (delta (car p1) (car p0)))
         (dy (delta (cdr p1) (cdr p0)))
         (xi (if (< (- (car p1) (car p0)) 0) -1 1)))
    (do ([D (- (* 2 dx) dy)]
         [result '()]
         [x (car p0)]
         [y (cdr p0)])
        ((> y (cdr p1)) result)
      (set! result (cons (cons x y) result))
      (set! y (+ y 1))
      (if (> D 0)
          (begin
            (set! x (+ x xi))
            (set! D (+ D (* 2 (- dx dy)))))
          (set! D (+ D (* 2 dx)))))))

(define (points-between p0 p1)
  (let ((x0 (car p0))
        (x1 (car p1))
        (y0 (cdr p0))
        (y1 (cdr p1)))
  (cond
    ;; Point
    ((and (= x0 x1)
          (= y0 y1))
     (list p0))
    ;; Vertical line
    ((= x0 x1)
     (for/list ([y (in-inclusive-range (min y0 y1)
                                       (max y0 y1))])
       (cons x0 y)))
    ;; Horizontal line
    ((= y0 y1)
     (for/list ([x (in-inclusive-range (min x0 x1)
                                       (max x0 x1))])
       (cons x y0)))
    ((and (< (delta y0 y1) (delta x0 x1))
          (> x0 x1))
     (plot-line-low p1 p0))
    ((and (< (delta y0 y1) (delta x0 x1))
          (<= x0 x1))
     (plot-line-low p0 p1))
    ((> y0 y1)
     (plot-line-high p1 p0))
    (#t
     (plot-line-high p0 p1)))))

(define (combinations vents)
  (if (empty? vents)
      '()
      (append (map (λ (vent) (list (car vents) vent)) (cdr vents))
              (combinations (cdr vents)))))


(define (solution filterfn filename)
  (let [(input (filter filterfn (read-vents filename)))
        (result (make-hash))]
    (for ([vent input])
      (for ([point (points-between (vent-p1 vent) (vent-p2 vent))])
        (hash-update! result point (λ (x) (+ x 1)) 0)))
    (length (filter (λ (x) (> x 1)) (hash-values result)))))

(define solution1 (lambda (fn) (solution straight? fn)))
(define solution2 (lambda (fn) (solution (const #t) fn)))

(define (crossing-count vents)
  (let ((result (make-hash)))
    (for ([vent vents]) (map (λ (p) (hash-update! result p (λ (x) (+ x 1)) 0))
                             (points-between (vent-p1 vent)
                                             (vent-p2 vent))))
    result))

(define (show-vents vs)
  (let ((crossing-count (crossing-count vs))
        (min-x (find-value-x min vs))
        (max-x (find-value-x max vs))
        (min-y (find-value-y min vs))
        (max-y (find-value-y max vs)))
    (for* ([y (in-inclusive-range min-y max-y)]
           [x (in-inclusive-range min-x max-x)])
      (let ((crossed (hash-ref crossing-count (cons x y) 0)))
        (if (= crossed 0)
            (display (~a "."))
            (display (~a crossed)))
        (when (= x max-x)
          (printf "~n"))))))
