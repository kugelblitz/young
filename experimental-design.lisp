(defpackage experimental-design
  (:use :common-lisp)
  (:export cube box-behnken box-wilson-ccf box-wilson-ccc))

(in-package experimental-design)

(defun cube (dimension)
  (if (zerop dimension)
      (list nil)
      (let ((result)
            (lower (cube (1- dimension))))
        (dolist (pt lower)
          (push (cons 1 pt) result)
          (push (cons -1 pt) result))
        result)))

(defun box-behnken (dimension)
  (let ((cube (cube (1- dimension)))
        (result (list (make-list dimension :initial-element 0))))
    (dolist (cube-pt cube)
      (loop
         for i from 0 below dimension
           do
           (let ((pt (copy-list cube-pt)))
             (push 0 pt)
             (setf (car pt) (nth i pt))
             (setf (nth i pt) 0)
             (push pt result))))
    result))

(defun box-wilson-ccf (dimension)
  "Wilson Central Composite Face-centered"
  (append (cube dimension) (box-behnken dimension)))

(defun box-wilson-ccc (dimension)
  "Wilson Central Composite Circumscribed"
  (let ((mult (/ (round (* 1000 (/ (sqrt dimension)
                                    (sqrt (1- dimension))))) 1000)))
    (append (cube dimension)
            (mapcar (lambda (pt)
                      (mapcar (lambda (x) (* x mult)) pt))
                    (box-behnken dimension)))))