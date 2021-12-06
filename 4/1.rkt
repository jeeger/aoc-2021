#lang racket

(require megaparsack megaparsack/text)
(require data/monad data/applicative)

(define (testparse parser string)
  (parse-result! (parse-string parser string)))

(define drawn-numbers/p
  (many/p integer/p #:sep (char/p #\,)))

(define parse-line/p
  (repeat/p 5 (do (many/p (char/p #\ ))
                  integer/p)))

(define parse-board/p
  (repeat/p 5 (do [line <- parse-line/p]
                  (or/p (char/p #\newline)
                        (lookahead/p eof/p))
                (pure line))))

(define parse-bingo/p
  (do [draw <- drawn-numbers/p]
      (repeat/p 2 (char/p #\newline))
    [boards <- (many/p parse-board/p
                       #:sep (char/p #\newline))]
    (pure (cons draw boards))))

(define (make-board l)
  (let ((pos (make-hash)))
    (for [(row-value (in-list l))
          (row (in-range (length l)))]
      (for [(number (in-list row-value))
            (column (in-range (length row-value)))]
        (hash-set! pos number (cons row column))))
    pos))

(define (board-size b)
  (cdr (assoc 'size b)))

(define (board-pos b num)
  (let ((pair (hash-ref (cdr (assoc 'board b)) num)))
    (values (car pair) (cdr pair))))

(define (board-rows b)
  (cdr (assoc 'rows b)))

(define (board-columns b)
  (cdr (assoc 'columns b)))

(define (unmarked-board b)
  (let ((board (make-board b))
         (row-markings
          (for/hash ((x (in-range (length b))))
            (values x '())))
         (column-markings
          (for/hash ((x (in-range (length b))))
            (values x '()))))
     (list (cons 'size (length b))
           (cons 'board board)
           (cons 'columns column-markings)
           (cons 'rows row-markings))))

(define (add-member r v num)
  (let ((old (hash-ref r v #f)))
    (cond
      ((not old) r)
      ((member num old) (error "Duplicate number added"))
      (#t (hash-set r v (cons num old))))))

(define (won? board)
  (or
   (for/or (((r v) (in-hash (board-rows board))))
     (= (length v) (board-size board)))
   (for/or (((c v) (in-hash (board-columns board))))
     (= (length v) (board-size board)))))

(define (markable? b num)
  (hash-ref (cdr (assoc 'board b)) num #f))

(define (mark-board b . nums)
  (define (mark-helper num b)
    (if (markable? b num)
        (let-values ([(row column) (board-pos b num)])
          (list (car b)
                (cadr b)
                (cons 'columns (add-member (board-columns b) column num))
                (cons 'rows (add-member (board-rows b) row num))))
        b))
    (foldr mark-helper b nums))

(define (marked? b num)
  (for/or ((row (in-hash-values (board-columns b)))
           (column (in-hash-values (board-rows b))))
    (or (member num row)
        (member num column))))

(define (unmarked-sum b)
  (for/sum ((num (in-hash-keys (cdr (assoc 'board b))))
             #:unless
             (marked? b num))
    num))

(define (final-score b final-called)
  (* (unmarked-sum b)
     final-called))

(define (play-boards bs num)
  (for/list ((board bs))
    (mark-board board num)))

(define (winning-boards bs)
  (filter won? bs))

(define (play-debug bs nums)
  (match nums
    ((list-rest num nums)
     (let* ((new-boards (play-boards bs num))
            (won-boards (winning-boards new-boards)))
       (if (> (length won-boards) 0)
           (begin
             (printf "Winner:~n")
             (print-board (car won-boards))
             (final-score (car won-boards) num))
           (begin
             (printf "Playing with new boards.~n")
             (map print-board new-boards)
             (play-debug new-boards nums)))))
    ('() "No one has won.")))

(define (play bs nums)
  (match nums
    ((list-rest num nums)
     (let* ((new-boards (play-boards bs num))
            (won-boards (winning-boards new-boards))) 
       (if (> (length won-boards) 0)
           (final-score (car won-boards) num)
           (play new-boards nums))))
    ('() "No one has won.")))

(define (solution playfn filename)
  (let* ((problem (parse-result! (parse-string parse-bingo/p  (port->string (open-input-file filename)))))
         (boards (map unmarked-board (cdr problem)))
         (draws (car problem)))
    (playfn boards draws)))

(define (num-at-pos b row column)
  (let ((board (cdr (assoc 'board b))))
    (for/or (((k v) (in-hash board)))
      (if (and (= (car v) row)
               (= (cdr v) column))
          k
          #f))))

(define (print-board b)
  (for* ((y (in-range (board-size b)))
         (x (in-range (board-size b))))
    (let ((cur-num (num-at-pos b y x)))
      (if (marked? b cur-num)
          (display (~a "|" cur-num "|" #:left-pad-string " " #:min-width 6 #:align 'right))
          (display (~a " " cur-num " " #:min-width 6 #:left-pad-string " " #:align 'right)))
      (when (= x (- (board-size b) 1))
        (printf "~n"))))
  (printf "~n"))
