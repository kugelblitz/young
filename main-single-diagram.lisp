(defpackage main
  (:use :common-lisp :dimension :algebra :calculus :young
        #+cmu :ext
        #+sbcl :sb-ext))

(in-package main)

(defvar *n*)
(defvar *lnf*)
(defvar *class*)
(defvar *func*)

(defun calc-c (n nord)
  (* -2 (/ (- (log nord) (* (log-n-fact n) 0.5)) (sqrt n))))

(defun on-diagram (func)
  (let ((size (funcall func :size)))
    (when (zerop (mod size 100))
      (format t "~a ~a~%"
              size
              (calc-c size
                      (calc-orderings-in-diagram (funcall func :corners)
                                                 (funcall func :dimples)))))))


(defun main ()

  (format t "Input dimension: ")
  (force-output)
  (setf *dimension* (parse-integer (read-line)))

  (format t "Input diagram size: ")
  (force-output)
  (setf *n* (parse-integer (read-line)))

  (format t "Input diagram type ([p]lancherel or [r]ichardson)) : ")
  (setf *class* (read-line))
  (cond
    ((equalp *class* "p") (setf *func* #'random-diagram-plancherel-markov))
    ((equalp *class* "r") (setf *func* #'random-diagram-richardson))
    (t (print "'p' or 'r' expected")))

  (setf *lnf* (log-n-fact *n*))
  (setf *on-diagram* #'on-diagram)
  (funcall *func* *n*)
  nil)

;(main)
;(quit)
