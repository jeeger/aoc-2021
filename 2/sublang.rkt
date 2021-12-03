#lang racket

(define (valid-line line)
  (and (non-empty-string? line)
       (not (string-prefix? line "#"))))

(define (read-commands port)
  (for/list ([line (in-lines port)] #:when (valid-line line))
    (match-let ([(list command argument) (string-split line)])
      (list (string->symbol command) (string->number argument)))))

(define (read-syntax path port)
  (let [(body (read-commands port))]
    #`(module sub-module "sublang.rkt"
        (match-let ([(list depth horz) (thread-through '(0 0) #,@body)])
                   (* depth horz)))))

(define-syntax-rule (my-module-begin LIST)
  (#%module-begin
   LIST))

(define-syntax (thread-through ARG)
  (syntax-case ARG ()
    [(_ INITIAL HEAD TAIL ...) #'(thread-through (HEAD INITIAL) TAIL ...)]
    [(_ VALUE) #'VALUE]))
 
(define-syntax-rule (forward arg)
  (lambda (position)
    (list (car position) (+ (cadr position) arg))))

(define-syntax-rule (up arg)
  (lambda (position)
    (list (- (car position) arg) (cadr position))))

(define-syntax-rule (down arg)
  (lambda (position)
    (list (+ (car position) arg) (cadr position))))

(provide read-syntax)
(provide forward up down thread-through match-let)
(provide (rename-out [my-module-begin #%module-begin]))
