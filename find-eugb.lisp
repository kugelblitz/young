(defpackage find-eugb
  (:use :common-lisp :dimension :algebra :geometry :young :gauss)
  (:export find-eugb
           *on-poly*
           *on-give-up*))

(in-package find-eugb)

(defvar *normal-forms*)
(defvar *basis*)
(defvar *on-poly*)
(defvar *on-give-up*)
(defvar *bad-coideals*)
(defvar *max-size* 500)

(defun contains-divisors-of (divisors multiples)
  (find-if (lambda (x)
             (find-divisor x divisors))
           multiples))

(defun remove-multiples-of (multiples monom)
  (remove-if (lambda (x)
               (monom-divides monom x))
             multiples))

(defun for-coideals (diagrams func &optional coideal)
  (if diagrams
      (let ((diagram (car diagrams)))
        (if (contains-divisors-of coideal diagram)
            (for-coideals (cdr diagrams) func coideal)
            (dolist (monom diagram)
              (for-coideals
               (cdr diagrams)
               func
               (cons monom
                     (remove-multiples-of coideal monom))))))
      (funcall func coideal)))

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

(defun find-poly (coideal)
  (let ((gauss:*matrix* (make-array '(0 0)))
        (gauss:*sqmatrix* (make-array '(0 0)))
        (gauss:*hmatrix* (make-array 0))
        (gauss:*hash* (make-hash-table :test #'equalp))
        (gauss:*n-polys* 0)
        (gauss:*n-monoms* 0)
        (inner)
        (outer (list (zero-vector)))
        (seq))
    (loop
       do
         (when (> gauss:*n-polys* *max-size*)
           (push coideal *bad-coideals*)
           (when *on-give-up*
             (funcall *on-give-up* coideal))
           (return-from find-poly nil))
         (unless outer
           (return-from find-poly nil))
         (let ((new-corner (closest-to-origin outer)))
           (push new-corner seq)
           (let ((coef
                  (gauss:add-polynom (get-normal-form new-corner)))
                 (result))
             (when coef
               (setf seq (nreverse seq))
               (loop
                  for c in coef
                  for monom in seq
                  do
                    (unless (zerop c)
                      (push (cons c monom) result)))
               (return-from find-poly result)))
           (setf inner (add-corner inner new-corner))
           (setf outer
                 (delete-if
                  (lambda (x)
                    (find-divisor x coideal))
                  (add-dimple outer new-corner)))))))

(defun coideal-belongs (smaller bigger)
  (not (find-if (lambda (s)
                  (not (find-divisor s smaller)))
                bigger)))

(defun find-eugb (basis)
  (let* ((*dimension* (length (cdaar basis)))
         (*normal-forms* (make-hash-table :test #'equalp))
         (*basis* basis)
         (*bad-coideals*)
         (diagrams))
    (loop
       do
         (block alpha
           (for-coideals
            diagrams
            (lambda (coideal)
              (unless (find-if
                       (lambda (bigger-coideal)
                         (coideal-belongs coideal bigger-coideal))
                       *bad-coideals*)
                (let ((poly (find-poly coideal)))
                  (when (and *on-poly* poly)
                    (funcall *on-poly* poly)
                    (push (mapcar #'cdr poly) diagrams)
                    (return-from alpha))))))
           (return)))))
