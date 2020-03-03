#lang racket

(define (gcd x y)
  (define a (abs x))
  (define b (abs y))
  (cond ((= 0 b) a)
        (else (gcd b (remainder a b)))))

(define (llen a-list)
  (if (empty? a-list)
      0
      (add1 (llen (rest a-list)))))
