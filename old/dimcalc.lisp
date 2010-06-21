(defpackage dimcalc
  (:use :common-lisp :algebra :gauss)
  (:export get-intersection-dim
           get-diagram-intersection-polys))

(in-package dimcalc)

(defun make-support-hyperplanes (support)
  (let ((res nil))
    (algebra::for-all-subsets
     support 2
     (lambda (subset)
       (let ((hp (algebra::hyperplane-orient-from-origin
                  (algebra::hyperplane subset))))
         (when (not (algebra::find-point-above support hp))
           (push hp res)))))
    res))

(defun make-initial-inner-corners (support)
  (let ((prev-monom (car support))
        (result nil))
    (dolist (monom (cdr support))
      (push (list (1+ (first prev-monom))
                  (1+ (second monom)))
            result)
      (setq prev-monom monom))
    result))

(defun complement-to-convex (support)
  (let ((inner-corners (make-initial-inner-corners support))
        (hyperplanes (make-support-hyperplanes support))
        (res nil)
        (new-inner-corners nil))
    (loop while inner-corners do
         (setq new-inner-corners nil)
         (dolist (corner inner-corners)
           (when (not (find-if (lambda (hp)
                                 (algebra::point-above corner hp))
                               hyperplanes))
             (when (not (find corner res :test #'equalp))
               (push corner res)
               (algebra::for-all-increments
                corner
                (lambda (new-corner)
                  (push new-corner new-inner-corners))))))
         (setq inner-corners new-inner-corners))
    res))

(defun make-poly-from-monoms (indices seq)
  (let ((result nil))
    (loop for index in indices
       for monom in seq do
         (when (not (zerop index))
           (setq result (cons (cons index monom) result))))
    result))

(defun get-diagram-intersection-polys (diagram basis)
  (let ((normal-forms)
        (monoms)
        (hash (make-hash-table :test 'equalp))
        (hash-count 0)
        (gauss-env)
        (result))
    (algebra:for-young-diagram
     diagram
     (lambda (monom)
       (push monom monoms)
       (push (normal-form (list (cons 1 monom)) basis) normal-forms)))
    (dolist (nf normal-forms)
      (dolist (term nf)
        (let ((monom (cdr term)))
          (unless (gethash monom hash)
            (setf (gethash monom hash) hash-count)
    (incf hash-count)))))
    (setf gauss-env (gauss::new-env hash))
    (dolist (nf normal-forms)
      (let ((indices (gauss::add-polynom-to-matrix-2 nf gauss-env)))
        (when indices
          (push (make-poly-from-monoms indices monoms) result))))
    result))

(defun get-intersection-dim (poly basis)
  (length (get-diagram-intersection-polys 
           (algebra::poly-get-support poly)
           basis)))

;; (defun check-convex-dim (poly basis hash)
;;   (let* ((gauss-env (gauss::new-env hash))
;;          (support (algebra::poly-get-support poly))
;;          (mb (make-monomial-basis support))
;;          (complement (complement-to-convex support))
;;          (dim))
;;     (format t "~%Considering polynom ")
;;     (poly-io::poly-print poly)
;;     (format t "~%  ~a monoms in Young diagram~%" (length mb))
;;     (format t "  ~a monoms in Newton diagram~%" (+ (length mb) (length complement)))
;;     (dolist (monom mb)
;;       (gauss::add-polynom-to-matrix
;;                        (algebra::normal-form (list (cons 1 monom)) basis)
;;                        gauss-env t))
;;     (setq dim (gauss::get-space-dim gauss-env))
;;     (when (/= 1 dim)
;;       (format t "????? dimension of Young diagram: dim = ~a~%" dim))
;;     (dolist (monom complement)
;;       (gauss::add-polynom-to-matrix 
;;        (algebra::normal-form (list (cons 1 monom)) basis) gauss-env t)
;;       (format t "  added ")
;;       (poly-io::poly-print (list (cons 1 monom)))
;;       (setq dim (gauss::get-space-dim gauss-env))
;;       (when (> dim 1)
;;         (format t ", dimension = ~a" dim))
;;       (format t "  ~%"))))
