(defpackage main
  (:use :common-lisp :dimension :young
        #+cmu :ext
        #+sbcl :sb-ext))

(in-package main)

(defvar *n*)
(defvar *class*)
(defvar *tableaux-counter*)
(defvar *diagrams-counter*)
(defvar *func*)

(defun on-tableau (sequence)
  (declare (ignore sequence))
  ;(print (funcall sequence))
  (incf *tableaux-counter*)
  t)

(defun on-diagram (func)
  (declare (ignore func))
  (incf *diagrams-counter*)
  t)

(defun main ()
  (format t "Input ordering class ([w]eak, [c]onvex, [a]dmissible) : ")
  (force-output)
  (setf *class* (read-line))
  (cond
    ((equalp *class* "w") (setf *func* #'enumerate-weak-orderings))
    ((equalp *class* "c") (setf *func* #'enumerate-convex-orderings))
    ((equalp *class* "a") (setf *func* #'enumerate-admissible-orderings))
    (t (print "'w', 'c', or 'a' is expected")))

  (format t "Input dimension: ")
  (force-output)
  (setf *dimension* (parse-integer (read-line)))

  (format t "Input diagram size: ")
  (force-output)
  (setf *n* (parse-integer (read-line)))

  (setf *tableaux-counter* 0)
  (setf *diagrams-counter* 0)

  (let ((*on-sequence* #'on-tableau)
        (*on-diagram* #'on-diagram))
    (funcall *func* *n*))

  (format t "n=~a: ~a tableaux~%" *n* *tableaux-counter*)
  (format t "n=~a: ~a diagrams~%" *n* *diagrams-counter*)
)
