;;; eintr.el --- Excercises from eintr.

;;; Commentary:

;; The exercises in the text An Introduction to Programming in Emacs
;; Lisp, Revised Third Edition, by Robert J. Chassell .
;;
;; Evaluate Elisp in Emacs for example by putting cursor after the last
;; close-paren and type C-x C-e.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercises 1.11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generate an error message by evaluating an appropriate symbol that is
;; not within parentheses.

/
foo

;; Generate an error message by evaluating an appropriate symbol that is
;; between parentheses.

(/)
(foo)

;; Create a counter that increments by two rather than one.

(defvar eintr-counter)
(setq eintr-counter 0)
(setq eintr-counter (+ eintr-counter 2))

;; Write an expression that prints a message in the echo area when
;; evaluated.

(message "foo bar")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercises 2.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn ;; Let's use progn so evaluation with C-x C-e can be done once.
  (message "start")
  (message "buffer name before switch: %s" (buffer-name))
  (set-buffer "sicp.el")
  (message "buffer name: %s" (buffer-name))
  (message "file name: %s" (buffer-file-name))
  (message "length of file: %d" (point-max))
  (message "cursor position before move to middle of file: %d" (point))
  (goto-char (/ (point-max) 2))
  (message "cursor position after move to middle of file: %d" (point))
  (message "end"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercises 3.12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun multiply-by-2 (n)
  "Multiply N by 2."
  (* 2 n))

(multiply-by-2 5)

(defun multiply-by-2-interactive (n)
  "Multiply N by 2."
  (interactive "p")
  (* 2 n))

(multiply-by-2-interactive 9)

(defun fill-column> (n)
  "Print YOLO if 'fill-column' is greater than N."
  (if (> fill-column n)
      (message "YOLO")))

(fill-column> 10)
(fill-column> 1000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercises 4.6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simplified-end-of-buffer ()
  "Go to end of buffer."
  (interactive)
  (push-mark)
  (goto-char (point-max)))

(simplified-end-of-buffer)

(defun print-buffer-exists-info (buffer-name)
  "Print message saying if BUFFER-NAME exists."
  (if (get-buffer buffer-name)
      (message "%s exists" buffer-name)
    (message "%s doesn't exist" buffer-name)))

(buffer-exists-p "*scratch*")
(buffer-exists-p "non-existing-buffer")

;;; eintr.el ends here
