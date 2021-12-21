#lang racket

(require "../util.rkt")
(require (prefix-in base: racket/base))
(require megaparsack megaparsack/text)
(require (rename-in data/monad [do monad-do]))
(require data/functor data/applicative)

(struct snailfish [l r] #:transparent)

(struct snailfish-optic [fs] #:transparent
  #:constructor-name -optic
  #:methods gen:functor
  [(define (map f x)
     (if (snailfish-optic? f)
         (-optic (append (snailfish-optic-fs f) (snailfish-optic-fs x)))
         (-optic (cons f (snailfish-optic-fs x)))))]
  #:methods gen:applicative
  [(define (pure _ x)
     (-optic (const x)))
   (define (apply f xs)
     (let apply-helper ([fns (reverse (snailfish-optic-fs f))] [result (car xs)])
       (if (null? fns)
           result
           (if (or (eq? result #f)
                   (number? result))
               #f
               (apply-helper (cdr fns) ((car fns) result))))))])

(struct snailfish-optic-bottom snailfish-optic []
  #:transparent
  #:constructor-name -optic-bottom
  #:methods gen:functor
  [(define (map f x)
     (-optic-bottom (list)))]
  #:methods gen:applicative
  [(define (pure _ x)
     (-optic-bottom (const x)))
   (define (apply f xs)
     #f)])


(define base-optic (-optic '()))
(define optic-empty? (compose empty? snailfish-optic-fs))

(define (make-optic fns)
  (-optic fns))
(define (optic-parent optic)
  (-optic (rest (snailfish-optic-fs optic))))
(define (optic-left optic)
  (-optic (cons snailfish-l (snailfish-optic-fs optic))))
(define (optic-right optic)
  (-optic (cons snailfish-r (snailfish-optic-fs optic))))
(define (optic-first optic)
  (car (snailfish-optic-fs optic)))
(define (optic-const value)
  (-optic (list (const value))))
(define (optic-bottom)
  (-optic-bottom (list)))

(define (snailfish-recursive-find test traverse base)
  (-optic
   (list (lambda (fish)
           (let recursive-find ([input (base fish)] [base base])
             (if (test input)
                 base
                 (if (snailfish? input)
                     (recursive-find (traverse input) (map traverse base))
                     (optic-bottom))))))))

(define (snailfish-first-left-number base) (snailfish-recursive-find number? snailfish-l base))
(define (snailfish-first-right-number base) (snailfish-recursive-find number? snailfish-r base))

(define (snailfish-next-right optic)
  (if (optic-empty? optic)
      (const (optic-bottom))
      (if (equal? (optic-first optic) snailfish-l)
          (snailfish-first-left-number (optic-right (optic-parent optic)))
          (snailfish-next-right (optic-parent optic)))))

(define (snailfish-next-left optic)
  (if (optic-empty? optic)
      (const (optic-bottom))
      (if (equal? (optic-first optic) snailfish-r)
          (snailfish-first-right-number (optic-left (optic-parent optic)))
          (snailfish-next-left (optic-parent optic)))))

(define snailfish/p
  (monad-do
   (char/p #\[)
   [left <- (or/p integer/p snailfish/p)]
   (char/p #\,)
   [right <- (or/p integer/p snailfish/p)]
   (char/p #\])
   (pure (snailfish left right))))

(define input/p
  (many/p snailfish/p #:sep newline/p))

(define (string->snailfish string)
  (parse-result! (parse-string snailfish/p string)))

(define (optic-find test fish)
  (let optic-find-helper ([depth 0] [fish fish] [path base-optic])
    (cond
      ((test depth fish)
       path)
      ((snailfish? fish)
       (or (optic-find-helper (add1 depth)
                              (snailfish-l fish)
                              (optic-left path))
           (optic-find-helper (add1 depth)
                              (snailfish-r fish)
                              (optic-right path))))
      (#t #f))))

(define snailfish-find-first-explodable
  (curry optic-find (λ (depth fish) (and (snailfish? fish) (= 4 depth)))))

(define snailfish-find-first-splittable
  (curry optic-find (λ (depth fish) (and (number? fish) (> fish 9)))))

(define/match (optic-rooted optic)
  [((snailfish-optic-bottom fs)) (-optic-bottom (list))]
  [((snailfish-optic fs)) (-optic (reverse fs))])

(define (optic-rest optic)
  (-optic (cdr (snailfish-optic-fs optic))))

(define (optic-set original optic value)
  (let optic-set-helper ([original original] [optic (optic-rooted optic)])
    (cond
      [(snailfish-optic-bottom? optic) original]
      [(optic-empty? optic) value]
      [(equal? base-optic optic) value]
      [(equal? (optic-first optic) const) value]
      [(equal? (optic-first optic) snailfish-l)
       (struct-copy snailfish original [l (optic-set-helper (snailfish-l original) (optic-rest optic))])]
      [(equal? (optic-first optic) snailfish-r)
       (struct-copy snailfish original [r (optic-set-helper (snailfish-r original) (optic-rest optic))])]
      [#t (error (~a "Unknown optic " (car optic)))])))

(define (optic-set* value . pairs)
  (foldl (λ (pair result)
           (optic-set result (car pair) (cdr pair))) value pairs))

(define *test-input* (string->snailfish "[[[[4,3],4],4],[7,[[8,4],9]]]"))

(define (snailfish-explode where fish)
  (let ([left ((snailfish-next-left where) fish)]
        [right ((snailfish-next-right where) fish)])
    (optic-set* fish
                (cons left (+ (or (left fish) 0) ((optic-left where) fish)))
                (cons right (+ (or (right fish) 0) ((optic-right where) fish)))
                (cons where 0))))

(define (snailfish-split where fish)
  (let ([old-value (where fish)])
    (optic-set fish where (snailfish (floor (/ old-value 2))
                                     (ceiling (/ old-value 2))))))

(define (snailfish-reduce-once fish)
  (let ([explodable (snailfish-find-first-explodable fish)]
        [splittable (snailfish-find-first-splittable fish)])
    (if explodable
        (snailfish-explode explodable fish)
        (if splittable
            (snailfish-split splittable fish)
            fish))))

(define (snailfish-reduce fish)
  (let ([newfish (snailfish-reduce-once fish)])
    (if (not (equal? newfish fish))
        (snailfish-reduce newfish)
        fish)))

(define (snailfish-add l r)
  (snailfish-reduce (snailfish l r)))

(define/match (magnitude fish)
  [((snailfish l r)) (+ (* 3 (magnitude l)) (* 2 (magnitude r)))]
  [(_) fish])

(define (add-fishes fishes)
  (let helper ([fishes (cdr fishes)] [result (car fishes)])
    (if (empty? fishes)
        result
        (helper (cdr fishes) (snailfish-add result (car fishes))))))

(define (solution1 fname)
  (let ([input (parse-file input/p fname)])
    (magnitude (add-fishes input))))

(define (solution2 fname)
  (let ([input (parse-file input/p fname)])
    (apply max (for*/list ([combination (in-combinations input 2)])
                 (max (magnitude (snailfish-add (car combination) (cadr combination)))
                      (magnitude (snailfish-add (cadr combination) (car combination))))))))

(provide (all-defined-out))
