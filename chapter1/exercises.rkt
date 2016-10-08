#lang racket

(require "../helpers.rkt")
(require racket/trace)
(require rackunit)

(exercise 1 1)
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

(exercise 1 2)
(display-eval (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
                 (* 3 (- 6 2) (- 2 7))))

(exercise 1 3)
(define (square x) (* x x))
(define (square-sum a b)
  (+ (square a) (square b)))
(define (larger-square a b c)
  (cond ((< a b c) (square-sum b c))
        ((< b a c) (square-sum a c))
        ((< c b a) (square-sum a b))
        ((< c a b) (square-sum a b))
        ((< b c a) (square-sum c a))
        ((< a c b) (square-sum c b))))
(display-eval (larger-square 1 2 3))
(display-eval (larger-square 2 3 1))

(exercise 1 4)
(displayln '(define (a-plus-abs-b a b)
              ((if (> b 0) + -) a b)))
(displayln "The if expression allows us to switch the operator used on a and b")

(exercise 1 5)
(displayln "Applicative order would result in an endless recursion because the two if arguments are
being evaluated regardless of the predicate's result.")

(exercise 1 6)
; First, the original sqrt
(define (sqrt x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (average x y)
    (/ (+ x y) 2))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (sqrt-iter 1.0))
(display-eval (sqrt 9))

; Then, our implementation
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause) (else else-clause)))
(define (new-sqrt x)
  (define (sqrt-iter guess)
    (new-if (good-enough? guess)
            guess
            (sqrt-iter (improve guess))))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (average x y)
    (/ (+ x y) 2))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (sqrt-iter 1.0)
  )
(displayln "All arguments will be evaluated -- since this is not a special form anymore")
(displayln '(new-sqrt 9))

(exercise 1 7)
(define (sqrt-improved x)
  (define (sqrt-iter last-guess guess)
    (if (good-enough? last-guess guess)
        guess
        (sqrt-iter guess (improve guess))))
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? last-guess guess)
    (let ((change (abs (- (square last-guess) (square guess)))))
      (< (/ change guess) 0.001)))
  (sqrt-iter x 1.0))
(display-eval (sqrt-improved 9))

(exercise 1 8)
(define (cube x) (* x x x))
(define (cube-root x)
  (define (cube-root-iter guess)
    (if (close-enough? guess)
        guess
        (cube-root-iter (improve guess))))
  (define (close-enough? guess)
    (< (abs (- (cube guess) x)) 0.01))
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (cube-root-iter 1))
(display-eval (cube-root 8))

(code 1 2 1)
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
(display-eval (factorial 2))
(define (factorial-iter n)
  (define (iter product counter max-count)
    (if (> counter max-count)
        product
        (iter (* counter product)
              (+ counter 1)
              max-count)))
  (iter 1 1 n))
(display-eval (factorial 3))

(exercise 1 9)
(define (new-+-a a b)
  (if (= a 0)
      b
      (inc (new-+-a (dec a) b))))
(trace new-+-a)
(define (new-+-b a b)
  (if (= a 0)
      b
      (new-+-b (dec a) (inc b))))
(trace new-+-b)
(display-eval (new-+-a 4 5))
(display-eval (new-+-b 4 5))
(displayln "The first + describes a recursive process, the second one describes an iterative process
(tail recusion!)")

(exercise 1 10)
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(display-eval (A 1 10))
(display-eval (A 2 4))
(display-eval (A 3 3))

(define (f n)
  (A 0 n))
(define (f-simple n)
  (* n 2))
(display-eval (f 1))
(display-eval (f-simple 1))

(define (g n)
  (A 1 n))
(define (g-simple n)
  (expt 2 n))
(display-eval (g 4))
(display-eval (g-simple 4))

(define (h n)
  (A 2 n))
(define (h-simple n)
  (define (iter n acc)
    (if (= n 0)
        acc
        (iter (- n 1) (expt acc 2))))
  (if (= n 0)
      0
      (iter n 2)))
(display-eval (h 4))
(display-eval (h-simple 4))

; Example from exercise
(define (k n)
  (* 5 n n))
(define (k-simple n)
  (* 5 (square n)))
(display-eval (k 4))
(display-eval (k-simple 4))

(code 1 2 2)
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(define (fib-improved n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

; Counting change
(define (count-change amount)
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0)
               (= kinds-of-coins 0))
           0)
          (else
           (+ (cc amount (- kinds-of-coins 1))
              (cc (- amount (first-denomination kinds-of-coins))
                  kinds-of-coins)))))
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (cc amount 5))
(display-eval (count-change 100))

(exercise 1 11)
(define (f-recur n)
  (cond ((< n 3) n)
        (else (+ (f-recur (- n 1))
                 (* 2 (f-recur (- n 2)))
                 (* 3 (f-recur (- n 3)))))))
(display-eval (f-recur 4))

(define (f-iter n)
  (define (iter a b c count)
    (let ([current-iteration (+ a (* 2 b) (* 3 c))])
      (cond ((> count 0) (iter current-iteration a b (- count 1)))
            (else current-iteration))))
  (match n
    [1 1]
    [2 2]
    [3 4]
    [_ (iter 4 2 1 (- n 4))]))
(display-eval (f-iter 4))
(check-eq? (f-recur 4) (f-iter 4))

(exercise 1 12)
(define (pascal row element)
  (cond
    [(< element 1) 0]
    [(> element row) 0]
    [(= row element 1) 1]
    [else (let ([left (- element 1)]
                [right element])
            (+ (pascal (- row 1) left)
               (pascal (- row 1) right)))]))
(trace pascal)
(display-eval (pascal 1 1))
(display-eval (pascal 2 2))
(display-eval (pascal 3 2))
(check-eq? (pascal 5 3) 6)
