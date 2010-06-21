(defpackage geometry
  (:use :common-lisp :dimension :matrix)
  (:export
   zero-vector
   is-zero-vector
   hyperplane
   hyperplane-orient-from-origin
   point-sign
   dot-product
   increment-point
   for-all-increments))

(in-package geometry)

(defun zero-vector ()
  (make-list *dimension* :initial-element 0))

(defun is-zero-vector (point)
  (declare (list point))
  (not (find-if (lambda (x) (not (zerop (the fixnum x))))
                point)))

(defun dot-product (a b)
  (if (null a)
      0
      (the fixnum
        (+ (the fixnum (* (the fixnum (car a)) (the fixnum (car b))))
           (the fixnum (dot-product (cdr a) (cdr b)))))))

(defun hyperplane (points)
  (let* ((first-point (car points))
         (matrix (make-array (list *dimension* *dimension*) :element-type 'fixnum))
         (multiplier 1) (free-term 0)
         (result))
    (declare (fixnum multiplier free-term *dimension*))
    (loop for point in (cdr points)
       for col of-type fixnum from 1 do
         (loop for a of-type fixnum in point
            for b of-type fixnum in first-point
            for row of-type fixnum from 0 do
              (setf (aref matrix row col) (- a b))))
    (loop for row from 0 below *dimension* do
         (push (the fixnum
                 (* multiplier (the fixnum
                                 (determinant
                                  (submatrix2 matrix row))))) result)
         (setq multiplier (- multiplier)))
    (setf result (nreverse result))
    (setf free-term (- (the fixnum (dot-product result first-point))))
    (cons free-term result)))

(defun point-sign (point hyperplane)
  (the fixnum (+ (the fixnum (car hyperplane))
                 (the fixnum (dot-product point (cdr hyperplane))))))

(defun hyperplane-orient-from-origin (hp)
  (if (find-if (lambda (x) (> x 0)) (cdr hp))
      hp
      (mapcar #'- hp)))

(defun increment-point (point i)
  (let ((result (copy-list point)))
    (setf (nth i result) (the fixnum (1+ (the fixnum (nth i point)))))
    result))

(defun for-all-increments (point func)
  (declare (function func))
  (apply func (list (cons (the fixnum (1+ (the fixnum (car point))))
			  (cdr point))))
  (when (cdr point)
    (for-all-increments
     (cdr point)
     (lambda (x) (apply func (list (cons (car point) x))))))
  nil)
