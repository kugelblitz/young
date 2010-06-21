(defpackage main
  (:use :common-lisp :algebra :young :dimension :calculus
        #+cmu :ext
        #+sbcl :sb-ext))

(in-package main)

(defvar *n* 100)
(defvar *n-fact*)
(defvar *n-diagrams* 1000)
(defvar *lnf*)

(defun calc-c (nord)
  (* -2 (/ (- (log nord) (* *lnf* 0.5)) (sqrt *n*))))

(defvar *all-c*)

(defun print-results ()
  (let ((avg-c 0)
        (nd (length *all-c*))
        (dev 0))
    (setf avg-c (/ (apply #'+ *all-c*) nd))
    (loop for c in *all-c*
       do
         (incf dev (sqr (- c avg-c))))
    (setf dev (sqrt (/ dev nd)))
    (format t "nd=~a: average = ~a, disp = ~a~%" nd avg-c dev)))

(defun main ()
  (format t "Input dimension: ")
  (force-output)
  (setf *dimension* (parse-integer (read-line)))

  (format t "Input diagram size: ")
  (force-output)
  (setf *n* (parse-integer (read-line)))

  (format t "Input number of diagrams to generate: ")
  (force-output)
  (setf *n-diagrams* (parse-integer (read-line)))

  (format t "got n=~a~%" *n*)
  
  (setf *lnf* (log-n-fact *n*))
  (setf *n-fact* (factorial *n*))
  (setf *all-c* nil)
  (loop for i from 1 to *n-diagrams* do
       (let* ((diagram (random-diagram-rsk *n*))
              (nord (calc-orderings-in-diagram
                     diagram nil))
              (c (calc-c nord)))
         (push c *all-c*))
       (when (zerop (mod i 100))
         (print-results))))

;(main)
;(quit)
