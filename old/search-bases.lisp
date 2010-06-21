(defpackage search-bases
  (:use :common-lisp 
        :algebra
        :external-helpers
        :poly-io
        :gauss2
        :cone2)
  (:export search-for-bases search-for-bases-2))

(in-package search-bases)

(defvar *normal-forms*)
(defvar *basis*)

;; (defun normal-form-2 (poly basis)
;;   (format t "poly: ~a~%" poly)
;;   (format t "basis: ~a~%" basis)
;;   (loop while poly
;;      do
;;        (let (new-poly)
;;          (loop named alpha
;;             for term in poly
;;             do
;;               (loop
;;                  for basepoly in basis
;;                  do
;;                    (when (term-divides (car basepoly) term)
;;                      (setf
;;                       new-poly
;;                       (poly-sub
;;                        poly
;;                        (poly-mult-term
;;                         basepoly
;;                         (term-divide term (car basepoly)))))
;;                      (return-from alpha)))
;;             finally
;;               (return-from normal-form-2 poly))
;;          (format t "new-poly: ~a~%" new-poly)
;;          (when new-poly
;;            (dolist (basepoly basis)
;;              (dolist (baseterm (cdr basepoly))
;;                (let* ((dir (mapcar #'- (cdar basepoly) (cdr baseterm)
;;                                    ))
;;                       (tmp1 (format t "dir: ~a~%" dir))
;;                       (res1 (reduce
;;                              #'max
;;                              (mapcar (lambda (x)
;;                                        (reduce #'+
;;                                                (mapcar #'* (cdr x) dir)))
;;                                      poly)))
;;                       (res2 (reduce
;;                              #'max
;;                              (mapcar (lambda (x)
;;                                        (reduce #'+
;;                                                (mapcar #'* (cdr x) dir)))
;;                                      new-poly))))
;;                  (when (< res1 res2)
;;                    (return-from normal-form-2 "shit"))
;;                  ))))         
;;          (setf poly new-poly)
;;          ))
;;   nil)


(defun buchberger-check-2 (basis)
  (and (get-basis-directions-2 basis)
       (buchberger-check basis)))



(defun get-normal-form (monom)
  (or (gethash monom *normal-forms*)
      (let ((nf (normal-form
                 (list (cons 1 monom)) *basis*)))
        (setf (gethash monom *normal-forms*) nf)
        nf)))

(defun add-outer-corner (outer-corners new-corner)
  (let ((other-outer-corners
         (delete new-corner outer-corners :test #'equalp))
        (new-outer-corners))
    (algebra::for-all-increments
     new-corner
     (lambda (new-outer-corner)
       (unless
           (algebra::find-monom-divisor new-outer-corner other-outer-corners)
         (push new-outer-corner new-outer-corners))))
    (append other-outer-corners new-outer-corners)))

(defun add-inner-corner (inner-corners new-corner)
  (cons new-corner
        (remove-if
         (lambda (x)
           (algebra::monom-divides x new-corner))
         inner-corners)))

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
  (let ((gauss2::*matrix* (make-array '(0 0)))
        (gauss2::*sqmatrix* (make-array '(0 0)))
        (gauss2::*hmatrix* (make-array 0))
        (gauss2::*hash* (make-hash-table :test #'equalp))
        (gauss2::*n-polys* 0)
        (gauss2::*n-monoms* 0)
        (inner)
        (outer (list (algebra::make-zero-monom)))
        (seq))
    (loop
       do
         (when (> gauss2::*n-polys* 100)
           (format t "GIVING UP ~a~%" coideal)
           (return-from find-poly nil))
         (unless outer
           (return-from find-poly nil))
         (let ((new-corner (closest-to-origin outer)))
           (push new-corner seq)
           (let ((coef
                  (gauss2:add-polynom (get-normal-form new-corner)))
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
           (setf inner (add-inner-corner inner new-corner))
           (setf outer
                 (delete-if
                  (lambda (x)
                    (find-divisor x coideal))
                  (add-outer-corner outer new-corner)))
           ))))

(defun detach-nondivisor-terms-sub (polys all-polys)
  (unless polys
    (return-from detach-nondivisor-terms-sub nil))
  (let ((divisor-terms)
        (nondivisor-terms))
    (dolist (term (car polys))
      (if
       (block alpha
         (dolist (poly all-polys)
           (dolist (term-i poly)
             (when (monom-divides-strong (cdr term) (cdr term-i))
               (return-from alpha t)))))
       (push term divisor-terms)
       (push term nondivisor-terms)))
    (cons (list nondivisor-terms divisor-terms)
          (detach-nondivisor-terms-sub (cdr polys) all-polys))))

(defun detach-nondivisor-terms (polys)
  (detach-nondivisor-terms-sub polys polys))

(defun for-all-marked-polys (detached-polys func)
  (unless detached-polys
    (apply func (list nil))
    (return-from for-all-marked-polys))
  (let ((nondivisor-terms (caar detached-polys))
        (divisor-terms (cadar detached-polys))
        (nondivisor-acc nil))
    (unless nondivisor-terms
      (return-from for-all-marked-polys nil))
    (loop while nondivisor-terms do
         (for-all-marked-polys
          (cdr detached-polys)
          (lambda (x) (apply func (list (cons (append nondivisor-terms
                                                      nondivisor-acc
                                                      divisor-terms)
                                              x)))))
         (push (car nondivisor-terms) nondivisor-acc)
         (setf nondivisor-terms (cdr nondivisor-terms)))))

(defun check-all-marked-different (polys)
  (unless polys
    (return-from check-all-marked-different t))
  (and (not (find-if 
             (lambda (x)
                     (equalp (cdar x) (cdaar polys)))
             (cdr polys)))
       (check-all-marked-different (cdr polys))))

(defun check-reduced (polys)
  (unless polys
    (return-from check-reduced t))
  (and (not (find-if
             (lambda (x)
               (or (find-if
                    (lambda (y)
                      (monom-divides (cdaar polys) (cdr y)))
                    x)
                   (find-if
                    (lambda (y)
                      (monom-divides (cdar x) (cdr y)))
                    (car polys))))
             (cdr polys)))
       (check-reduced (cdr polys))))

(defun coideal-included (smaller bigger)
  (not (find-if (lambda (s)
                  (not (find-divisor s smaller)))
                bigger)))

(defun search-for-bases (vars generators poly-func)
  (let* ((poly-io:*vars* vars)
         (algebra:*dimension* (length (cdaar generators)))
         (*basis* (basis-normalize-lex-keep-marked (get-groebner-base generators)))
         (*normal-forms* (make-hash-table :test #'equalp))
         ;;(dim-qa (n-monoms-under (leading-monoms *basis*)))
         (diagrams)
         (polys)
         (coideals)
         (gfan (mapcar #'basis-normalize-lex-keep-marked
                       (get-gfan generators))))
    ;;(format t "GB: ")
    ;;(polys-print *basis*)
    ;;(format t "~%")
    (loop
       do
         ;;(format t "~%diagrams: ~a~%" diagrams)
         ;;(format t "polys: ")
         ;;(polys-print polys)
         ;;(format t "~%")
          (for-all-subsets-2
          polys
          (lambda (polys-sub)
            (for-all-marked-polys
             (detach-nondivisor-terms polys-sub)
             (lambda (polys-i) 
               (when (and polys-i
                          (check-all-marked-different polys-i)
                          (check-reduced polys-i)
                          ;(buchberger-check-2 polys-i)
                          ;(equalp
                          ; (basis-normalize-lex-keep-marked (get-groebner-base polys-i))
                          ; *basis*)
                          (find (basis-normalize-lex-keep-marked polys-i)
                                gfan :test #'equalp)
                          )
                   ;;(format t "basis: ")
                   ;;(polys-print polys-i)
                   ;;(format t "~%")
                 (push (mapcar #'cdar polys-i) coideals))))))

         (block alpha
           (for-all-representatives
            diagrams
            (lambda (coideal)
              (unless (find-if
                       (lambda (bigger-coideal)
                         (coideal-included coideal bigger-coideal))
                       coideals)
                ;;(format t "coideal: ~a~%" coideal)
                (let ((poly (find-poly coideal)))
                  (when poly
                    (funcall poly-func poly)
                    (push poly polys)
                    (push (mapcar #'cdr poly) diagrams)
                    (return-from alpha))))))
           (return)))))

(defun search-for-bases-2 (vars generators poly-func)
  (let* ((poly-io:*vars* vars)
         (algebra:*dimension* (length (cdaar generators)))
         (*basis* (basis-normalize-lex-keep-marked (get-groebner-base generators)))
         (*normal-forms* (make-hash-table :test #'equalp))
         (diagrams)
         (coideals)
         (gfan (mapcar #'basis-normalize-lex-keep-marked
                       (get-gfan generators)))
         (gfan-polys-norm (make-hash-table :test #'equalp))
         )
    (loop
       for basis in gfan
       do
         (loop
            for poly in basis
            do
              (let ((poly-norm (poly-normalize-lex poly)))
                (unless (gethash poly-norm gfan-polys-norm)
                  (setf (gethash poly-norm gfan-polys-norm) 1)
                  (push (mapcar #'cdr poly) diagrams)
                  (funcall poly-func poly))))
         (push (mapcar #'cdar basis) coideals))
    (format t "~%DBG: GFAN FINISHED~%")
    (loop
       do
         (block alpha
           (for-all-representatives-2
            diagrams
            (lambda (coideal)
              (unless (find-if
                       (lambda (bigger-coideal)
                         (coideal-included coideal bigger-coideal))
                       coideals)
                (let ((poly (find-poly coideal)))
                  (when poly
                    (funcall poly-func poly)
                    (push (mapcar #'cdr poly) diagrams)
                    (return-from alpha))))))
           (return)))))
