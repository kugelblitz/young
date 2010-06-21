(defpackage queue
  (:use :common-lisp)
  (:export dequeue
           enqueue
           make-queue))

(in-package queue)

(defun make-queue ()
  (cons nil nil))

(defun enqueue (obj queue)
  (if (cdr queue)
      (setf (cdr (cdr queue)) (list obj)
            (cdr queue) (cddr queue))
    (setf (cdr queue) (list obj)
          (car queue) (cdr queue)))
  nil)

(defun dequeue (queue)
  (when (cdr queue)
    (if (eq (cdr queue)
	    (car queue))
	(setf (car queue) nil
	      (cdr queue) nil))
    (setf (car queue) (cdar queue)))
  nil)