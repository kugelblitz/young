(defpackage main
  (:use :common-lisp :dimension :calculus :young
        #+cmu :ext
        #+sbcl :sb-ext))

(in-package main)

(defvar *n* 100)
(defvar *n-fact*)
(defvar *lnf*)

(defvar *n-found*)

(defvar *nord-max*)
(defvar *c-max*)

(defvar *diag-max*)

(defun calc-c (nord)
  (* -2 (/ (- (log nord) (* *lnf* 0.5)) (sqrt *n*))))

(defun print-results ()
  (format t "(n=~a) ~a diagrams:~%  max-dim = ~a~%  max-diag = ~a~%  max-c = ~a~%"
          *n* *n-found* *nord-max* *diag-max* *c-max*))

(defun on-diagram (func)
  (let* ((nord
          (calc-orderings-in-diagram
           (funcall func :corners)
           (funcall func :dimples)))
         (c (calc-c nord)))
    (incf *n-found*)
    (when (> nord *nord-max*)
      (setf *nord-max* nord)
      (setf *c-max* c)
      (setf *diag-max* (funcall func :corners))
      ;(print-results)
      )))

(defun main2 ()
  (setf *dimension* 2)

  (loop for i from 1 below 1000 do
   (let ()
     (setf *n* i)
	
     (setf *n-fact* (factorial *n*))
     (setf *lnf* (log-n-fact *n*))
     (setf *n-found* 0)
     (setf *nord-max* 0)
     (setf *c-max* 0)
     
     (setf *on-diagram* #'on-diagram)
     
     (enumerate-symmetric-diagrams-2d *n*)
     (print-results)
     )))

(defun main ()
  (setf *dimension* 2)

  (format t "Input diagram size: ")
  (force-output)
  (setf *n* (parse-integer (read-line)))

  (setf *n-fact* (factorial *n*))
  (setf *lnf* (log-n-fact *n*))
  (setf *n-found* 0)
  (setf *nord-max* 0)
  (setf *c-max* 0)
  
  (setf *on-diagram* #'on-diagram)
  
  (enumerate-symmetric-diagrams-2d *n*)
  (print-results)
  nil)

;(main2)
;(main)
;(quit)
