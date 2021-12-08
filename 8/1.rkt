#lang racket

(struct note [wiring output])

(define (parse-wiring line)
  (string-split line " "))
(define (parse-output line)
  (string-split line " "))
(define (parse-line line)
  (match-let (((list wiring output) (string-split line " | ")))
    (note (parse-wiring wiring) (parse-output output))))
(define (parse-input input)
  (map parse-line (string-split (port->string (open-input-file input)) "\n")))

(define (unique-digits note)
  (length (filter (λ (o) (member (string-length o) '(2 4 3 7))) (note-output note))))

(define (solution1 input)
  (for/sum ([note (parse-input input)])
    (unique-digits note)))

(define *test-input* (parse-input "input-test"))
(require csp)

(define (combinations l count)
  (cond ((= count 0)
         '())
        ((= count 1)
         (map list l))
        ((= (length l) count)
         (list l))
        (#t
         (append
          (map (λ (r) (cons (car l) r)) (combinations (cdr l) (sub1 count)))
          (combinations (cdr l) count)))))

(define string->set (compose list->set string->list))
(define set->string (compose list->string set->list))

(define digit-segments
  (make-hash (map (λ (c) (cons (car c) (string->set (cdr c))))
              '((zero . "abcefg")
               (one . "cf")
               (two . "acdeg")
               (three . "acdfg")
               (four . "bcdf")
               (five . "abdfg")
               (six . "abdefg")
               (seven . "acf")
               (eight . "abcdefg")
               (nine . "abcdfg")))))

(define (common-subset-of-size? size)
  (lambda (s1 s2)
    (= size (set-count (set-intersect s1 s2)))))

(define (add-digit-constraints problem)
  (for ([comb (combinations (hash-keys digit-segments) 2)])
    (let ((l (car comb))
          (r (cadr comb)))
      (add-constraint! problem
                       (common-subset-of-size?
                        (set-count (set-intersect
                                    (hash-ref digit-segments l)
                                    (hash-ref digit-segments r)))) (list l r)))))

(define (extend-problem problem note digit digit-length)
  (add-var! problem digit
            (map string->set 
                 (filter (λ (output) (= (string-length output) digit-length))
                         (note-wiring note)))))

(define (sym->number sym)
  (match sym
    ('zero 0)
    ('one 1)
    ('two 2)
    ('three 3)
    ('four 4)
    ('five 5)
    ('six 6)
    ('seven 7)
    ('eight 8)
    ('nine 9)))

(define (decode assignment note)
  (let ((decoder (make-hash (map (lambda (c) (cons (set->string (cdr c)) (sym->number (car c)))) assignment))))
    (car (foldr + (λ (digit running)
                    (cons (+ (car running) (* digit (cdr running)))
                          (* 10 (cdr running)))) (0 1)
                (reverse (map (λ (output) (hash-ref decoder output)) (note-output note)))))))

(define (get-decoder note)
  (let ((problem (make-csp)))
    (for ([digit (in-list '(zero one two three four five six seven eight nine))]
          [digit-length (in-list '(6 2 5 5 4 5 6 3 7 6))])
      (extend-problem problem note digit digit-length))
    (add-digit-constraints problem)
    (make-hash (map (lambda (s) (cons (cdr s) (sym->number (car s)))) (solve problem)))))

(define (to-number l)
  (define (to-number-helper l result scale)
    (if (empty? l)
        result
        (to-number-helper (cdr l) (+ result (* scale (car l))) (* scale 10))))
  (to-number-helper (reverse l) 0 1))

(define (digit-value note)
  (let ([decoder (get-decoder note)])
    (to-number (map (λ (out) (hash-ref decoder (string->set out))) (note-output note)))))

(define (solution2 input)
  (for/sum ([note (parse-input input)])
    (digit-value note)))
