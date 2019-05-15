;;; sicp.el --- SICP exercises implemented in Emacs Lisp.

;;; Commentary:

;; The exercises in the computer science text Structure and
;; Interpretation of Computer Programs, by Abelson, Sussman, and Sussman
;; implemented in Emacs Lisp.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

10

(+ 5 3 4)

(defvar a 3)

(defvar b (+ a 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun two-largest (a b c)
  "Return two largest of A, B and C as list."
  (if (> a b)
      (list a (max b c))
    (list b (max a c))))

(defun sum-squares-of-two-largest (a b c)
  "Sum squares of two largest of A, B and C."
  (seq-reduce '+ (mapcar (lambda (n) (* n n)) (two-largest a b c)) 0))

(sum-squares-of-two-largest 2 1 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The operator is returned from the if-statement; + if b is greater
;; than zero and - otherwise.

;; Work in progress here: Can't figure out how to return the operator.
;; Can we use for example backticks or (intern "+") to accomplish
;; something similar?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun p ()
  "Recurse infinitely."
  (p))

(defun test (x y)
  "Return 0 if X is 0, otherwise return Y."
  (if (= x 0)
      0
    y))

(test 0 (p))

;; The expression above results in endless recursion. This tells us that
;; the interpreter uses applicative-order evaluation, in which the
;; operand (p) is evaluated before the operator test is applied. If the
;; interpreter had used normal-order evaluation, the expression would
;; have returned 0, since evaluation of (p) would have been delayed
;; until needed, and when x equals 0, evaluation of (p) is never needed.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evaluation results in infinite recursion, since the first thing that
;; happens when a function is called is that all its arguments are
;; evaluated. In this case, the result is a call stack where sqrt-iter
;; calls new-if, which calls sqrt-iter, which calls new-if, forever.

(defun new-if (p a b)
  "If P is true A should be evaluated, else B should be evaluated."
  (cond (p a)
        (t b)))

;; Working examples.
(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

;; Failing example. (See the *Messages* buffer.)
(new-if (= 1 1)
        (message "We are both evaluated since")
        (message "we are function arguments!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun good-enough-p (guess x)
  "Return true if difference between GUESS squared and X is small."
  (< (abs (- (* guess guess) x)) 0.001))

(defun improve (guess x)
  "Return new approximation by averaging GUESS with X divided by GUESS."
  (average guess (/ x guess)))

(defun average (x y)
  "Return average of X and Y."
  (/ (+ x y) 2))

(defun sqrt-iter (guess x)
  "Calculate, using GUESS as starting point, square root of X."
  (if (good-enough-p guess x)
      guess
    (sqrt-iter (improve guess x) x)))

(defun sqrt (x)
  "Calculate square root of X."
  (sqrt-iter 1.0 x))

;; For very small numbers, 0.001 is too large:
;;
;; (sqrt 0.0004) => 0.0354008825558513
;; (- 0.0004 (* (sqrt 0.0004) (sqrt 0.0004))) => -0.0008532224857331766
;; sqrt-iter being invoked recursively with arguments guess and x:
;;
;; good-enough-p concludes that 0.035 is a sufficiently good guess,
;; because the difference between 0.035 * 0.035 and 0.0004 is less than
;; 0.001. We have to lower 0.001 to at least 0.0001 to get close to the
;; correct answer 0.02.
;;
;; For very large numbers, 0.001 is too small. (sqrt 9999999999999991)
;; results in infinite recursion because improve eventually returns the
;; same result, 99999999.99999997, over and over. Because of limitation
;; in precision of floats, and the rounding that therefore occurs.

(defun new-good-enough-p (previous-guess guess)
  "Return t if difference between PREVIOUS-GUESS and GUESS is small."
  (< (abs (/ (- guess previous-guess) guess)) 0.00001))

(defun new-sqrt-iter (previous-guess guess x)
  "Calculate the square root of X recursively.
PREVIOUS-GUESS serves as memory and GUESS is the guess."
  (if (new-good-enough-p guess previous-guess)
      guess
    (new-sqrt-iter guess (improve guess x) x)))

(defun new-sqrt (x)
  "Calculate square root of X."
  (new-sqrt-iter 0.0 1.0 x))

;; With the suggested change in good-enough-p, and changing 0.001 to
;; 0.00001, we get better results for both small and large numbers:
;;
;; (- 0.0004 (* (new-sqrt 0.0004) (new-sqrt 0.0004))) => 0.0
;; (- 9999999999999991 (* (new-sqrt 9999999999999991) (new-sqrt 9999999999999991))) => -2.0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cbrt (x)
  "Calculate the cube root of X."

  (defun cbrt-sqrt-iter (guess prev-guess x)
    "Calculate the cube root of X iteratively.
The function halts when the difference between PREVIOUS-GUESS and GUESS
is sufficiently small."
    (if (good-enough-p guess prev-guess)
        guess
      (cbrt-sqrt-iter (improve guess x) guess x)))

  (defun good-enough-p (guess prev-guess)
    "Return t/nil if diff between GUESS and PREV-GUESS is small/large."
    (< (abs (/ (- guess prev-guess) guess)) 0.00001))

  (defun improve (guess x)
    "Improve the approximate cube root GUESS of X."
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

  (cbrt-sqrt-iter 1.0 0.0 x))

;; (* (cbrt 28) (cbrt 28) (cbrt 28)) => 28.00000000000869

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.9
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun inc (n)
  "Increase N by 1."
  (+ n 1))

(defun dec (n)
  "Decrease N by 1."
  (- n 1))

(defun add1 (a b)
  "Add A and B."
  (if (= a 0)
      b
    (inc (add1 (dec a) b))))

(defun add2 (a b)
  "Add A and B."
  (if (= a 0)
      b
    (add2 (dec a) (inc b))))

;; (add1 4 5)
;; (inc (add1 (dec 4) 5))
;; (inc (add1 (- 4 1) 5))
;; (inc (add1 3 5))
;; (inc (inc (add1 (dec 3) 5)))
;; (inc (inc (add1 (- 3 1) 5)))
;; (inc (inc (add1 2 5)))
;; (inc (inc (inc (add1 (dec 2) 5))))
;; (inc (inc (inc (add1 (- 2 1) 5))))
;; (inc (inc (inc (add1 1 5))))
;; (inc (inc (inc (inc (add1 (dec 1) 5)))))
;; (inc (inc (inc (inc (add1 (- 1 1) 5)))))
;; (inc (inc (inc (inc (add1 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc (+ 5 1))))
;; (inc (inc (inc 6)))
;; (inc (inc (+ 6 1)))
;; (inc (inc 7))
;; (inc (+ 7 1))
;; (inc 8)
;; (+ 8 1)
;; 9

;; (add2 4 5)
;; (add2 (dec 4) (inc 5))
;; (add2 (- 4 1) (+ 5 1))
;; (add2 3 6)
;; (add2 (dec 3) (inc 6))
;; (add2 (- 3 1) (+ 6 1))
;; (add2 2 7)
;; (add2 (dec 2) (inc 7))
;; (add2 (- 2 1) (+ 7 1))
;; (add2 1 8)
;; (add2 (dec 1) (inc 8))
;; (add2 (- 1 1) (+ 8 1))
;; (add2 0 9)
;; 9

;; The process generated by add1 is recursive, and the process generated
;; by add2 is iterative. And note that both add1 and add2 are recursive.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun A (x y)
  "The Ackermann function.
Produce large numbers with small nonnegative integers X and Y."
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (t (A (- x 1) (A x (- y 1))))))

;; (A 1 10) => 1024
;; (A 2 4) => 65536
;; (A 3 3) => 65536

(defun f (n)
  "Return 2*N."
  (A 0 n))

;; (f n) example:
;; (f 3)
;; (A 0 3)
;; (* 2 3)
;; 6

;; Finding the mathematical definition (f n):
;; (f n)
;; (A 0 n)
;; (* 2 n)

(defun g (n)
  "Return 0 if N equals 0, 2^N otherwise."
  (A 1 n))

;; (g n) example:
;; (g 3)
;; (A 1 3)
;; (A 0 (A 1 (- 3 1)))
;; (* 2 (A 1 2))
;; (* 2 (A 0 (A 1 (- 2 1))))
;; (* 2 (* 2 (A 1 1)))
;; (* 2 (* 2 2)
;; 8

;; Finding the mathematical definition of (g n):
;; (g n)
;; (A 1 n)
;; (A 0 (A 1 (- n 1)))
;; (* 2 (A 1 (- n 1)))
;; (* 2 (A 0 (A 1 (- (- n 1) 1))))
;; (* 2 (* 2 (A 1 (- n 2))))
;; (* 2 (* 2 (A 0 (A 1 (- (- n 2) 1)))))
;; (* 2 (* 2 (* 2 (A 1 (- n 3)))))
;; ... continues until n is 0.
;; Will return 0 if n is zero since it leads to a base case. Will return
;; 2^n otherwise, since 1 as the first argument results in 2 being
;; multiplied with itself n-1 times, and then being multiplied with a
;; final 2 when the base for when the second argument being equal to 1
;; is reached.

(defun h (n)
  "Return 0 if N equals 0, 2 if N equals 1, and 2^(h(N-1)) otherwise."
  (A 2 n))

;; (h n) example:
;; (h 3)
;; (A 2 3)
;; (A 1 (A 2 2))
;; (A 1 4)
;; (A 0 (A 1 3))
;; (A 0 (A 0 (A 1 2)))
;; (A 0 (A 0 (A 0 (A 1 1))))
;; (* 2 2 2 2)
;; 16

;; Finding the mathematical definition of (h n):
;; (h n)
;; (A 2 n)
;; (A 1 (A 2 (- n 1)))
;; ... and we can stop here and re-use the definitions of h(n) and g(n):
;; (A 1 (h (- n 1))) // replaced (A 2 (- n 1)) with (h (- n 1))
;; (g (h (- n 1))) // as per definition of g(n)
;; ... and knowing that g(n) = 2^n, we get h(n) = 2^(h(n-1)). Except for
;; n = 0 and n = 1, since these corresponds to two of Ackermann's
;; base cases. So (h 3) becomes 2^(h(2)) which becomes 2^(2^(h(1)))
;; which becomes 2^(2^2).

(provide 'sicp)

;;; sicp.el ends here
