(asdf:operate 'asdf:load-op :cl-cairo2 :verbose nil)

(defpackage draw-poly
  (:use :common-lisp :cl-cairo2))

(in-package draw-poly)

(defun start (size filename)
  (defparameter *surface* (create-ps-surface filename size size))
  (setf cl-cairo2::*context* (create-context *surface*))
  (destroy *surface*)
  (set-source-rgb 1 1 1)
  (rectangle 0 0 size size)
  (fill-path)
  (set-source-rgb 0.2 0.2 1)
  (set-line-width 1))

(defun finish ()
  (destroy cl-cairo2::*context*))

(defun draw-polygon (polygon)
  (move-to (first (car polygon)) (second (car polygon)))
  (dolist (vertex (cdr polygon))
    (line-to (first vertex) (second vertex)))
  (close-path)
  (stroke))

(defun draw-lsegs (lsegs)
  (dolist (lseg lsegs)
    (move-to (first (first lseg)) (second (first lseg)))
    (line-to (first (second lseg)) (second (second lseg))))
  (close-path)
  (stroke))
