(defpackage experimental-design
  (:use :common-lisp)
  (:export cube box-behnken box-wilson-ccf box-wilson-ccc box-wilson-ccc-2))

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
        (result))
    (setf result (list (make-list dimension :initial-element 0)))
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

(defun unit-vectors (dimension)
  (let ((result))
    (loop
       for i from 0 below dimension
       do
         (let ((pt (make-list dimension :initial-element 0)))
           (setf (nth i pt) 1)
           (push (copy-list pt) result)
           (setf (nth i pt) -1)
           (push (copy-list pt) result)
           ))
    result))

(defun box-wilson-ccf (dimension)
  "Wilson Central Composite Face-centered"
  (append (cube dimension) (unit-vectors dimension)
          (list (make-list dimension :initial-element 0))))

(defun box-wilson-ccc (dimension)
  "Wilson Central Composite Circumscribed"
  (let ((mult (/ (round (* 1000 (sqrt dimension))) 1000)))
    (append (cube dimension)
            (mapcar (lambda (pt)
                      (mapcar (lambda (x) (* x mult)) pt))
                    (unit-vectors dimension)))))

(defun box-wilson-ccc-2 (dimension)
  "Wilson Central Composite Circumscribed"
  (let ((mult 2))
    (append (cube dimension)
            (mapcar (lambda (pt)
                      (mapcar (lambda (x) (* x mult)) pt))
                    (unit-vectors dimension)))))