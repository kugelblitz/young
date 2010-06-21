(defpackage matrix
  (:use :common-lisp :dimension)
  (:export
   submatrix
   submatrix2
   determinant
   fill-row))

(in-package matrix)

(defun copy-matrix (matrix)
  (declare (type (simple-array fixnum (* *)) matrix))
  (let* ((result (make-array (array-dimensions matrix)
                             :element-type 'fixnum))
         (height (array-dimension matrix 0))
         (width (array-dimension matrix 1)))
    (loop for i from 0 below height do
         (loop for j from 0 below width do
              (setf (aref result i j)
                    (aref matrix i j))))
    result))

(defun fill-row (matrix result n matrix-row result-row)
  (declare (fixnum n matrix-row result-row)
	   (type (simple-array fixnum (* *)) matrix result))
  (loop for index from 1 below n do
	(setf (aref result result-row (1- index))
	      (aref matrix matrix-row index))))

(declaim (inline fill-row))

(defun submatrix (matrix row-to-delete &optional (n *dimension*))
  (declare (fixnum n row-to-delete))
  (declare (type (simple-array fixnum (* *)) matrix))
  (let* ((result (make-array (list (the fixnum (1- n))
                                   (the fixnum (1- n)))
                             :element-type 'fixnum)))
    (when (not (zerop row-to-delete))
      (loop for row from 1 below row-to-delete do
           (fill-row matrix result n row (1- row)))
      (fill-row matrix result n 0 (1- row-to-delete)))
    (loop for row from (1+ row-to-delete) below n do
         (fill-row matrix result n row (1- row)))
    result))

(defun submatrix2 (matrix row-to-delete &optional (n *dimension*))
  (declare (fixnum n row-to-delete))
  (declare (type (simple-array fixnum (* *)) matrix))
  (let* ((result (make-array (list (the fixnum (1- n))
                                   (the fixnum (1- n)))
                             :element-type 'fixnum)))
    (when (not (zerop row-to-delete))
      (loop for row from 0 below row-to-delete do
           (fill-row matrix result n row row)))
    (loop for row from (1+ row-to-delete) below n do
         (fill-row matrix result n row (1- row)))
    result))


(defun determinant-sub (matrix n)
  (declare (fixnum n)
           (type (simple-array fixnum (* *)) matrix))
  (if (= 1 n)
      (aref matrix 0 0)
      (let ((rows nil) (prod 1) (selected-row nil) (multiplier 0)
            (numerator 1) (denominator 1) (nrows 0))
        (declare (fixnum nrows numerator denominator prod multiplier))
        (loop for index of-type fixnum from 0 below n do
             (when (not (zerop (aref matrix index 0)))
               (push index rows)
               (setq nrows (1+ nrows))
               (setq prod (* prod (aref matrix index 0)))))
        (setf rows (nreverse rows))
        (if (zerop nrows)
            0
            (progn
              (setq selected-row (car rows))
              (setq multiplier (/ prod (aref matrix selected-row 0)))
              (loop for index from 1 below n do
                   (setf
                    (aref matrix selected-row index)
                    (* (aref matrix selected-row index) multiplier)))
              (dolist (row (cdr rows))
                (setq multiplier (/ prod (aref matrix row 0)))
                (loop for index from 1 below n do
                     (setf
                      (aref matrix row index)
                      (- (the fixnum (* (aref matrix row index) multiplier))
                         (aref matrix selected-row index)))))
              (unless (zerop selected-row)
                (setq numerator -1))
              (when (= nrows 1)
                (setq numerator (* numerator (aref matrix (car rows) 0))))
              (when (> nrows 2)
                (setq denominator (expt prod (- (length rows) 2))))
              (/ (the fixnum (* (the fixnum
                                  (determinant-sub
                                   (submatrix matrix selected-row n) (1- n)))
                                numerator))
                 denominator))))))

(defun determinant (matrix)
  (declare (type (simple-array fixnum (* *)) matrix))
  (determinant-sub (copy-matrix matrix) (array-dimension matrix 0)))
