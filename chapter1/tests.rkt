#lang racket

(require "exercises.rkt")
(require "../helpers.rkt")
(require rackunit)

(exercise 1 10)
(check-eq? (g 4) (g-simple 4))
(check-eq? (h 4) (h-simple 4))
(check-eq? (k 4) (k-simple 4))

(exercise 1 11)
(check-eq? (f-recur 4) (f-iter 4))

(exercise 1 12)
(check-eq? (pascal 5 3) 6)

(exercise 1 16)
(check-eq? (fast-expt-recur 2 0) 1)
(check-eq? (fast-expt-recur 2 3) 8)
