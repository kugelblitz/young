(defpackage find-mlims
  (:use :common-lisp :combinatorics :dimension :algebra :geometry :young :gauss)
  (:export
   find-mlims
   *on-mlims*))

(in-package find-mlims)

(defvar *on-mlims*)

(defvar *basis*)
(defvar *normal-forms*)
(defvar *full-only*)
(defvar *quotient-algebra-dimension*)

(defun get-normal-form (monom)
  (or (gethash monom *normal-forms*)
      (let ((nf (normal-form
                 (list (cons 1 monom)) *basis*)))
        (setf (gethash monom *normal-forms*) nf)
        nf)))

(defun yield-mlims (corners seq)
  (when *full-only*
    (unless (equalp (length seq) *quotient-algebra-dimension*)
      (return-from yield-mlims nil)))
  (when *on-mlims*
    (funcall *on-mlims* (lambda (name)
                          (cond
                            ((equalp name :corners) corners)
                            ((equalp name :size) (length seq)))))))

(defun find-mlims-sub (seq dimples corners)
  (let ((sorted-corners (sort (copy-list corners)
                                  (lambda (x y) (< (car x) (car y))))))
    (unless (equalp (car seq) (car sorted-corners))
;      (format t "non-canonical~%")
      (return-from find-mlims-sub nil)))
  (let ((*boundary-dimples* *boundary-dimples*)
        (remaining-dimples))
    (dolist (dimple dimples)
      (let ((gauss:*matrix* gauss:*matrix*)
            (gauss:*sqmatrix* gauss:*sqmatrix*)
            (gauss:*hmatrix* gauss:*hmatrix*)
            (gauss:*n-polys* gauss:*n-polys*)
            (gauss:*n-monoms* gauss:*n-monoms*)
            (gauss:*hash* gauss:*hash*))
        (if (gauss:add-polynom (get-normal-form dimple))
            (push dimple *boundary-dimples*)
            (push dimple remaining-dimples))))
    (if remaining-dimples
        (for-all-cuts
         remaining-dimples
         (lambda (dimple other-dimples)
;           (format t "HANDLING ~a of ~a~%" dimple remaining-dimples)
           (let ((gauss:*matrix* gauss:*matrix*)
                 (gauss:*sqmatrix* gauss:*sqmatrix*)
                 (gauss:*hmatrix* gauss:*hmatrix*)
                 (gauss:*n-polys* gauss:*n-polys*)
                 (gauss:*n-monoms* gauss:*n-monoms*)
                 (gauss:*hash* gauss:*hash*))
             (when (gauss:add-polynom (get-normal-form dimple))
               (error "internal???"))
             (find-mlims-sub
              (cons dimple seq)
              (update-dimples other-dimples dimple)
              (add-corner corners dimple)))))
      (yield-mlims corners seq))))

(defun find-mlims (basis &key full-only)
  (let ((*quotient-algebra-dimension*
         (n-points-under (mapcar #'cdar basis))))
    (unless *quotient-algebra-dimension*
      (error "ideal is not zero-dimension"))
    (let ((*dimension* (length (cdaar basis)))
          (*normal-forms* (make-hash-table :test #'equalp))
          (*basis* basis)
          (*boundary-dimples*)
          (*full-only* full-only)
          (gauss:*hash* (make-hash-table :test #'equalp))
          (gauss:*matrix* (make-array '(0 0)))
          (gauss:*sqmatrix* (make-array '(0 0)))
          (gauss:*hmatrix* (make-array 0))
          (gauss:*n-polys* 0)
          (gauss:*n-monoms* 0))
      (find-mlims-sub nil (list (zero-vector)) nil)
    )))
