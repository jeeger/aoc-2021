#lang racket

(define (valid-line line)
  (and (non-empty-string? line)
       (not (string-prefix? line "#"))))

(define debug? #f)

(define (read-commands port)
  (for/list ([line (in-lines port)] #:when (valid-line line))
    (match-let ([(list command argument) (string-split line)])
      (list (string->symbol command) (string->number argument)))))

(define (read-syntax path port)
  (let [(body (read-commands port))]
    #`(module sub-module "sublang2.rkt"
        (match-let ([(list aim depth horz) (thread-through '(0 0 0) #,@body)])
          (* depth horz)))))

(define-syntax-rule (my-module-begin LIST)
  (#%module-begin
   LIST))

(define-syntax (thread-through-debug ARG)
  (syntax-case ARG ()
    [(_ INITIAL HEAD TAIL ...)
     #'(thread-through-debug (HEAD INITIAL) TAIL ...)]
    [(_ VALUE) #'VALUE]))

(define-syntax (thread-through ARG)
  (syntax-case ARG ()
    [(_ INITIAL HEAD TAIL ...) #'(thread-through (HEAD INITIAL) TAIL ...)]
    [(_ VALUE) #'VALUE]))

(define-syntax-rule (forward arg)
  (match-lambda ((list aim depth horz)
                 (let [(new-depth (+ depth (* aim arg)))
                       (new-horz (+ horz arg))]
                   (when debug?
                     (printf "Going forward. A: ~a, D: ~a->~a, H: ~a->~a~n"
                             aim depth new-depth horz new-horz))
                   (list aim new-depth new-horz)))))

(define-syntax-rule (up arg)
  (match-lambda ((list aim depth horz)
                 (let [(new-aim (- aim arg))]
                   (when debug?
                     (printf "Going up. A: ~a->~a, D: ~a, H: ~a~n"
                             aim new-aim depth horz))
                   (list new-aim depth horz)))))

(define-syntax-rule (down arg)
  (match-lambda ((list aim depth horz)
                 (let [(new-aim (+ arg aim))]
                 (when debug?
                   (printf "Going up. A: ~a->~a, D: ~a, H: ~a~n"
                           aim new-aim depth horz))
                 (list new-aim depth horz)))))

(provide read-syntax)
(provide forward up down thread-through thread-through-debug match-let match debug?)
(provide (rename-out [my-module-begin #%module-begin]))
