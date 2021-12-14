#lang racket
(require megaparsack)
(require megaparsack/text)
(require racket/struct)

(define (parse-file parser name)
  (parse-result! (parse-string parser (port->string (open-input-file name)))))

(define (test-parse-string parser string)
  (parse-result! (parse-string parser string)))

(define newline/p (char/p #\newline))

(struct point [x y]
  #:transparent
  #:methods gen:custom-write
  [(define write-proc (make-constructor-style-printer
                       (lambda (obj) 'point)
                       (lambda (obj) (list (point-x obj) (point-y obj)))))])

(define cross-product
  (match-lambda* ((list (point x1 y1) (point x2 y2))
                  (- (* x1 y2)
                     (* y1 x2)))))

(define dot-product
  (match-lambda* ((list (point x1 y1) (point x2 y2))
                  (+ (* x1 x2)
                     (* y1 y2)))))

(define vec-
  (match-lambda* ((list (point x1 y1) (point x2 y2))
                  (point (- x1 x2)
                         (- y1 y2)))))

(define vec+
  (match-lambda* ((list (point x1 y1) (point x2 y2))
                  (point (+ x1 x2)
                         (+ y1 y2)))))

(define vec*
  (match-lambda* ((list (point x y) s)
                  (cons (* x s)
                        (* y s)))))

(provide parse-file test-parse-string newline/p)
(provide point point-x point-y cross-product dot-product vec- vec+ vec*)
