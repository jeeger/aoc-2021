#lang racket

(require "../util.rkt")
(require megaparsack megaparsack/text)
(require (rename-in data/monad [do monad-do])
         (only-in data/applicative pure))

(define input/p
  (monad-do
   (string/p "Player 1 starting position: ")
   [p1start <- integer/p]
   newline/p
   (string/p "Player 2 starting position: ")
   [p2start <- integer/p]
   (pure (cons p1start p2start))))

(define *test-input* (parse-file input/p "input-test"))

(struct die [rolled result] #:transparent)
(define (die-wrap-result result)
  (let ([mod (modulo result 100)])
    (if (= mod 0)
        100
        mod)))

(define/match (roll _die)
  [((die rolled result))
   (values result (die (add1 rolled)
                       (die-wrap-result (add1 result))))])

(define/match (three-rolls _die)
  [((die rolled result))
   (values (+ (* 3 result) 3) (die (+ 3 rolled) (die-wrap-result (+ result 3))))])

(struct gamestate [p1pos p2pos p1points p2points dice] #:transparent)
(define (game-start p1pos p2pos)
  (gamestate p1pos p2pos 0 0 (die 0 1)))

(define (game-start-score p1pos p2pos p1score p2score)
  (gamestate p1pos p2pos p1score p2score (die 0 1)))

;; game-wrap-position 11: 1
;; game-wrap-position 12: 2
;; game-wrap-position 19: 9
(define (game-wrap-position newpos)
  (let ([mod (modulo newpos 10)])
    (if (= mod 0)
        10
        mod)))

(define/match (won? _gamestate)
  [((gamestate _ _ p1points p2points _))
   (or (>= p1points 1000)
       (>= p2points 1000))])

(define/match (turn _gamestate)
  [((gamestate p1pos p2pos p1points p2points die))
   (let*-values ([(p1-result next-die) (three-rolls die)]
                 [(p1-next-position) (game-wrap-position (+ p1pos p1-result))]
                 [(p1-gamestate) (gamestate p1-next-position p2pos (+ p1points p1-next-position) p2points next-die)]
                 [(p2-result next-die) (three-rolls next-die)]
                 [(p2-next-position) (game-wrap-position (+ p2pos p2-result))]
                 [(p2-gamestate)
                  (gamestate p1-next-position p2-next-position (+ p1points p1-next-position) (+ p2points p2-next-position)
                             next-die)])
     (if (won? p1-gamestate)
         p1-gamestate
         p2-gamestate))])

(define (play-turns p1pos p2pos turns)
  (let play-helper ([gamestate (game-start p1pos p2pos)] [turns turns])
    (if (= 0 turns)
        gamestate
        (let ([next-gamestate (turn gamestate)])
          (if (won? next-gamestate)
              next-gamestate
              (play-helper next-gamestate (sub1 turns)))))))

(define (play-until-won p1pos p2pos)
  (let play-helper ([gamestate (game-start p1pos p2pos)])
    (let ([next-gamestate (turn gamestate)])
      (if (won? next-gamestate)
          next-gamestate
          (play-helper next-gamestate)))))

(define/match (calculate-solution _gamestate)
  [((gamestate _ _ p1score p2score (die rolled _)))
   (* (min p1score p2score) rolled)])

(define (solution1 fname)
  (match-let ([(list-rest p1pos p2pos) (parse-file input/p fname)])
    (calculate-solution (play-until-won p1pos p2pos))))

(define (go-forward pos result)
  (game-wrap-position (+ pos result)))

(define (add-score score pos result)
  (+ score (game-wrap-position (+ pos result))))

(define *winning-score* 3)
(define/match (sum-results toadd sum)
  [((list-rest p1 p2) (list-rest s1 s2)) (cons (+ p1 s1) (+ p2 s2))])

(define/match (next-gamestate _player _gamestate roll)
  [('one (gamestate p1pos p2pos p1score p2score die) roll)
   (gamestate (go-forward p1pos roll) p2pos (add-score p1score p1pos roll) p2score die)]
  [('two (gamestate p1pos p2pos p1score p2score die) roll)
   (gamestate p1pos (go-forward p2pos roll) p1score (add-score p2score p2pos roll) die)])

(define (collect-gamestates p1pos p2pos p1score p2score depth)
  (define/match (won? _gamestate)
    [((gamestate _ _ p1points p2points _))
     (or (>= p1points 21)
         (>= p2points 21))])
  (let gamestate-helper ([gamestate (game-start-score p1pos p2pos p1score p2score)] [depth depth])
    (if (zero? depth)
        (list gamestate)
        (append* (for*/list ([roll1 '(1 2 3)]
                             [roll2 '(1 2 3)])
                   (let* ([p1-gamestate (next-gamestate 'one gamestate roll1)]
                          [p2-gamestate (next-gamestate 'two p1-gamestate roll2)])
                     (if (won? p1-gamestate)
                         (list p1-gamestate)
                         (if (won? p2-gamestate)
                             (list p2-gamestate)
                             (gamestate-helper p2-gamestate (sub1 depth))))))))))

(define (player-wins gamestates)
  (foldl (match-lambda* ((list (gamestate _ _ p1score p2score _) (list-rest p1-wins p2-wins))
                         (cond
                           [(>= p1score 21) (cons (add1 p1-wins) p2-wins)]
                           [(>= p2score 21) (cons p1-wins (add1 p2-wins))])))
         (cons 0 0)
         gamestates))

(define (winning-gamestates p1pos p2pos p1score p2score)
  (define/match (won? _gamestate)
    [((gamestate _ _ p1points p2points _))
     (or (>= p1points 21)
         (>= p2points 21))])
  (let gamestate-helper ([gamestate (game-start-score p1pos p2pos p1score p2score)])
    (append* (for/list ([roll1 '(1 2 3)])
               (let ([p1-gamestate (next-gamestate 'one gamestate roll1)])
                 (if (won? p1-gamestate)
                     (list p1-gamestate)
                     (append*
                      (for/list ([roll2 '(1 2 3)])
                        (let ([p2-gamestate (next-gamestate 'two p1-gamestate roll2)])
                          (if (won? p2-gamestate)
                              (list p2-gamestate)
                              (gamestate-helper p2-gamestate)))))))))))

(define/match (play-turn player p1pos p2pos p1score p2score)
  [('one p1pos p2pos p1score p2score)
   (display (~a #:separator " " "P1: Called with" p1pos p2pos p1score p2score "\n"))
   (foldl sum-results
          (cons 1 1)
          (for/list ([roll (in-list '(1 2 3))])
            (let ([next-pos (go-forward p1pos roll)]
                  [next-score (add-score p1score p1pos roll)])
              (display (~a #:separator " " "P1: Next pos, score:" next-pos next-score "\n"))
              (if (>= next-score *winning-score*)
                  (cons 1 0)
                  (play-turn 'two
                             next-pos
                             p2pos
                             next-score
                             p2score)))))]
  [('two p1pos p2pos p1score p2score)
   (display (~a #:separator " " "P2: Called with" p1pos p2pos p1score p2score "\n"))
   (foldl sum-results
          (cons 1 1)
          (for/list ([roll (in-list '(1 2 3))])
            (let ([next-pos (go-forward p2pos roll)]
                  [next-score (add-score p2score p2pos roll)])
              (display (~a #:separator " " "P2: Next pos, score:" next-pos next-score "\n"))
              (if (>= next-score *winning-score*)
                  (cons 0 1)
                  (play-turn 'one
                             p1pos
                             next-pos
                             p1score
                             next-score)))))])

(define (player-one-wins p1start p2start)
  (define cache (make-hash))
  (define (hash-update p1p p1s p2p p2s value)
    (hash-set! cache (list p1p p1s p2p p2s) value)
    value)
  (define (hash-has p1p p1s p2p p2s)
    (hash-has-key? cache (list p1p p1s p2p p2s)))
  (define (hash-get p1p p1s p2p p2s)
    (hash-ref cache (list p1p p1s p2p p2s)))
  (let play-helper ([p1pos p1start] [p2pos p2start] [p1score 0] [p2score 0])
    (for*/sum ([p1-result (in-inclusive-range 1 3)]
               [p2-result (in-inclusive-range 1 3)])
      (let* ([p1-newpos (game-wrap-position (+ p1pos p1-result))]
             [p1-newscore (+ p1score p1-newpos)]
             [p2-newpos (game-wrap-position (+ p2pos p2-result))]
             [p2-newscore (+ p2score p2-newpos)])
        (if (>= p1-newscore 21)
            (hash-update p1-newpos p1-newscore p2pos p2score 1)
            (if (>= (+ p2score p2-newpos) 21)
                (hash-update p1-newpos p1-newscore p2-newpos p2-newscore 0)
                (if (hash-has p1-newpos p1-newscore p2-newpos p2-newscore)
                    (hash-get p1-newpos p1-newscore p2-newpos p2-newscore)
                    (play-helper p1-newpos p2-newpos p1-newscore p2-newscore))))))))
