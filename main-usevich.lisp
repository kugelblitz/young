(defpackage main
  (:use :common-lisp :dimension :combinatorics :young :geometry
        :algebra :poly-io :external-helpers :find-mlims :cl-ppcre
        #+cmu :ext
        #+sbcl :sb-ext))

(in-package main)

(defvar *mlims* nil)

(defvar *normal-forms*)
(defvar *basis*)
(defvar *qa-dim*)
(defvar *max-coideal* 100)

(defun get-normal-form (monom)
  (or (gethash monom *normal-forms*)
      (let ((nf (normal-form
                 (list (cons 1 monom)) *basis*)))
        (setf (gethash monom *normal-forms*) nf)
        nf)))

(defun closest-to-origin (corners)
  (let* ((best (car corners))
         (best-sum (apply #'+ best)))
    (dolist (iter (cdr corners))
      (let ((sum (apply #'+ iter)))
        (when (< sum best-sum)
          (setf best-sum sum)
          (setf best iter))))
    best))


(defun test-hypo (coideal)
  (let ((gauss:*matrix* (make-array '(0 0)))
        (gauss:*sqmatrix* (make-array '(0 0)))
        (gauss:*hmatrix* (make-array 0))
        (gauss:*hash* (make-hash-table :test #'equalp))
        (gauss:*n-polys* 0)
        (gauss:*n-monoms* 0)
        (corners)
        (outer (list (zero-vector)))
        (seq)
        (dim 0)
        (cnt 0))
    (when (find-divisor (car outer) coideal)
      (return-from test-hypo))
    (loop
       do
         (when (equalp dim *qa-dim*)
           (format t "FOUND ~a~%" corners)
           (return-from test-hypo corners))
         (incf cnt)
         (when (equalp cnt *max-coideal*)
           (format t "GIVING UP~%")
           (return-from test-hypo nil))
         (let ((new-corner (closest-to-origin outer)))
           (unless new-corner
             (format t "IN A CAVE~%")
             (return-from test-hypo nil))
           (push new-corner seq)
           (unless (gauss:add-polynom (get-normal-form new-corner))
             (incf dim))
           (setf corners (add-corner corners new-corner))
           (setf outer
                 (delete-if
                  (lambda (x)
                    (find-divisor x coideal))
                  (add-dimple outer new-corner)))))))

(defun main ()
  (let ((poly-io:*vars* nil))
    (format t "Input list of variables:")
    (force-output)
    (setf poly-io:*vars* (cl-ppcre:split " " (read-line)))
    (setf *dimension* (length poly-io:*vars*))
    
    (format t "Input generators:")
    (force-output)
    (setf *basis* (get-groebner-base (parse-polys (read-line))))
    (setf *mlims* nil)

    (setf *qa-dim* (n-points-under (mapcar #'cdar *basis*)))
    (unless *qa-dim*
      (error "ideal is not zero-dimension"))
    
    (format t "Got Groebner basis: ")
    (polys-print *basis*)
    (format t "~%")

    (format t "QA-DIM=~a~%" *qa-dim*)
    (format t "MLIMS:~%")
    (let ((find-mlims:*on-mlims*
           (lambda (func) (format t "  ~a~%" (funcall func :corners))
                   (push (funcall func :corners) *mlims*))))
      (find-mlims *basis* :full-only t))
    (for-all-representatives
     *mlims*
     (lambda (rep)
       (let ((*boundary-dimples* rep)
             (*normal-forms* (make-hash-table :test #'equalp)))
         (format t "trying ~a... " rep)
         (test-hypo rep))))
    ))
