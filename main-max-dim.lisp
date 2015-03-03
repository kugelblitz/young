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
(defvar *sum*)

(defun calc-c (nord)
  (* -2 (/ (- (log nord) (* *lnf* 0.5)) (sqrt *n*))))


(defun print-results ()
  (format t "(n=~a) ~a diagrams:~%  max-dim = ~a~%  max-diag = ~a~%  max-c = ~a~%  avg-c = ~a~%"
          *n* *n-found* *nord-max* *diag-max* *c-max* *sum*))

(defun on-diagram (func)
  (let* ((nord
          (calc-orderings-in-diagram
           (funcall func :corners)
           (funcall func :dimples)))
         (c (calc-c nord)))
    (when (> nord *nord-max*)
      (setf *nord-max* nord)
      (setf *c-max* c)
      (setf *diag-max* (copy-list (funcall func :corners))))
    
    (incf *n-found*)
    (incf *sum* (* c (/ (sqr nord) *n-fact*)))
    (when nil;(zerop (mod *n-found* 10000))
      (print-results))))

(defun main-sub (dim size)
  (setf *dimension* dim)
  (setf *n* size)
  (setf *n-fact* (factorial *n*))
  (setf *lnf* (log-n-fact *n*))
  (setf *n-found* 0)
  (setf *nord-max* 0)
  (setf *c-max* 0)
  (setf *sum* 0)

  (setf *on-diagram* #'on-diagram)
  (enumerate-diagrams *n*)
  (print-results)
  nil)

(defun main ()
  (let ((dim nil)
	(size nil))
    (format t "Input dimension: ")
    (force-output)
    (setf dim (parse-integer (read-line)))
    (format t "Input diagram size: ")
    (force-output)
    (setf size (parse-integer (read-line)))
    (main-sub dim size)))

(defun main-loop ()
    (loop for i from 3 to 300 do
	 (main-sub 2 i)))


;(main)
;(quit)
