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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Figure out the syntax.

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
;; (inc (add1 3 5))
;; (inc (inc (add1 2 5)))
;; (inc (inc (inc (add1 1 5))))
;; (inc (inc (inc (inc (add1 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9

;; (add2 4 5)
;; (add2 3 6)
;; (add2 2 7)
;; (add2 1 8)
;; (add2 0 9)
;; 9

;; The process generated by add1 is recursive, and the process generated
;; by add2 is iterative.

(provide 'sicp)

;;; sicp.el ends here
