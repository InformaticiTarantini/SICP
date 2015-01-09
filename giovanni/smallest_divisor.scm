#lang planet neil/sicp

(define (smallest-divisor n)
  (define (square x) (* x x))
  (define (tr-divisor n test-divisor)
    (define (div? a b) (= (remainder b a) 0))
    (cond ((> (square test-divisor) n) n)
          ((div? test-divisor n) test-divisor)
          (else (tr-divisor n (+ test-divisor 1)))))
  (tr-divisor n 2))

