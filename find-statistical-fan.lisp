(defpackage find-statistical-fan
  (:use :common-lisp :combinatorics :dimension :algebra :geometry :young :gauss)
  (:export
   find-fan
   *on-sequence*))

(in-package find-statistical-fan)

(defvar *design*)
(defvar *evaluated-monomials*)
(defvar *on-sequence*)
(defvar *boundary-dimples*)

(defun evaluate-monomial-in-point (monom pt)
  (reduce #'* (mapcar #'expt pt monom)))

(defun evaluate-monomial (monom)
  (mapcan (lambda (x)
            (let ((value (evaluate-monomial-in-point monom x)))
              (unless (zerop value)
                (list (cons value (copy-list x))))))  *design*))

(defun evaluate-monomial-use-cache (monom)
  (or (gethash monom *evaluated-monomials*)
      (let ((evaluated (evaluate-monomial monom)))
        (setf (gethash monom *evaluated-monomials*) evaluated)
        evaluated)))

(defun yield-sequence (seq polys)
  (when (equalp (length seq) (length *design*))
    (when *on-sequence*
      (funcall *on-sequence* (reverse seq) polys))))

(defun find-fan-sub (seq dimples corners polys)
  (let ((sorted-corners (sort (copy-list corners)
                              #'lex-more)))
    (unless (equalp (car seq) (car sorted-corners))
      (return-from find-fan-sub nil)))
  (let ((*boundary-dimples* *boundary-dimples*)
        (remaining-dimples)
        (remaining-dimples-sorted))
    (dolist (dimple dimples)
      (let ((gauss:*matrix* gauss:*matrix*)
            (gauss:*sqmatrix* gauss:*sqmatrix*)
            (gauss:*hmatrix* gauss:*hmatrix*)
            (gauss:*n-polys* gauss:*n-polys*)
            (gauss:*n-monoms* gauss:*n-monoms*)
            (gauss:*hash* gauss:*hash*))
        (let ((coef (gauss:add-polynom (evaluate-monomial dimple)))
              )
          (if coef
            (progn
              (let ((seq (copy-list seq))
                    (poly))
                (push dimple seq)
                (setf seq (reverse seq))
                (loop
                   for c in coef
                   for monom in seq
                   do
                     (unless (zerop c)
                       (push (cons c monom) poly)))
                (push poly polys))
              (push dimple *boundary-dimples*)
              )
            (push dimple remaining-dimples)))
        ))
    (setf remaining-dimples-sorted (sort (copy-list remaining-dimples) #'lex-more))
    (if remaining-dimples
        (for-all-cuts
         remaining-dimples-sorted
         (lambda (dimple other-dimples)
           (let ((gauss:*matrix* gauss:*matrix*)
                 (gauss:*sqmatrix* gauss:*sqmatrix*)
                 (gauss:*hmatrix* gauss:*hmatrix*)
                 (gauss:*n-polys* gauss:*n-polys*)
                 (gauss:*n-monoms* gauss:*n-monoms*)
                 (gauss:*hash* gauss:*hash*))
             (when (gauss:add-polynom (evaluate-monomial dimple))
               (error "internal error"))
             (find-fan-sub
              (cons dimple seq)
              (update-dimples other-dimples dimple)
              (add-corner corners dimple)
              polys)))))
      (yield-sequence seq polys)))

(defun find-fan (design)
  (let ((*design* design)
        (*dimension* (length (car design)))
        (*evaluated-monomials* (make-hash-table :test #'equalp))
        (*boundary-dimples*)
        (gauss:*hash* (make-hash-table :test #'equalp))
        (gauss:*matrix* (make-array '(0 0)))
        (gauss:*sqmatrix* (make-array '(0 0)))
        (gauss:*hmatrix* (make-array 0))
        (gauss:*n-polys* 0)
        (gauss:*n-monoms* 0))
    (find-fan-sub nil (list (zero-vector)) nil nil)
    ))
