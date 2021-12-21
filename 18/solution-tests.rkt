#lang racket

(require "../util.rkt")
(require (rename-in "solution.rkt" [string->snailfish sf]))
(require rackunit)

(define-syntax-rule (snailfish-reduce-check arg expected)
  (check-equal? (snailfish-reduce-once (sf arg))
                (sf expected)))

(check-equal? (snailfish-add (sf "[1,2]") (sf "[[3,4],5]"))
              (sf "[[1,2],[[3,4],5]]"))
(snailfish-reduce-check "[[[[[9,8],1],2],3],4]"
                        "[[[[0,9],2],3],4]")
(snailfish-reduce-check "[7,[6,[5,[4,[3,2]]]]]"
                        "[7,[6,[5,[7,0]]]]")
(snailfish-reduce-check "[[6,[5,[4,[3,2]]]],1]" "[[6,[5,[7,0]]],3]")
(snailfish-reduce-check "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
(snailfish-reduce-check "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")

(check-equal? (add-fishes (parse-file input/p "input-test2"))
              (sf "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"))

