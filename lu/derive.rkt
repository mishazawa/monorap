#lang racket
(define (d/dx fn)
  (define d (/ 1 100000))
  (λ (x) (/ (- (fn (+ x d)) (fn (- x d))) 2 d)))