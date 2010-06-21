(defpackage algebra
  (:use :common-lisp :dimension)
  (:export
   arrfixnum
   min-lex
   monom-divides
   monom-divides-strong
   find-monom-divisor
   monom-is-power
   poly-normalize-lex
   basis-normalize-lex-keep-marked
   basis-normalize-lex-no-monomial-order
   poly-normalize-lex-keep-marked
   leading-monoms n-monoms-under
   poly-composition
   hash-table-for-nf
   for-all-monoms-under
   for-young-diagram
   for-all-increments
   for-all-decrements
   for-all-subsets
   for-all-subsets-2
   for-all-pairs
   for-all-cuts
   for-all-max-subsets
   for-total-young-diagram
   for-all-representatives
   for-all-representatives-2
   normal-form
   lex-less
   zero-monom
   make-zero-monom
   find-dividend
   find-divisor
   poly-get-support
   buchberger-check
   term-lcm
   term-divide
   term-divides
   poly-sub
   poly-mult-term
   is-zero-point
   ))

(in-package algebra)

(defun make-zero-monom (&optional (dim *dimension*))
  (make-list dim :initial-element 0))

(defun zero-monom (&optional (dim *dimension*))
  (make-list dim :initial-element 0))

(defun lex-more (monom1 monom2)
  (cond
    ((null monom1) nil)
    ((> (the fixnum (car monom1)) (the fixnum (car monom2))) t)
    ((< (the fixnum (car monom1)) (the fixnum (car monom2))) nil)
    (t (lex-more (cdr monom1) (cdr monom2)))))

(defun lex-less (monom1 monom2)
  (cond
    ((null monom1) nil)
    ((> (the fixnum (car monom2)) (the fixnum (car monom1))) t)
    ((< (the fixnum (car monom2)) (the fixnum (car monom1))) nil)
    (t (lex-less (cdr monom1) (cdr monom2)))))

(defun monom-divides (monom1 monom2)
  (cond ((null monom1) t)
        ((> (the fixnum (car monom1)) (the fixnum (car monom2))) nil)
        (t (monom-divides (cdr monom1) (cdr monom2)))))

(defun monom-divides-strong (monom1 monom2)
  (and (monom-divides monom1 monom2)
       (not (monom-divides monom2 monom1))))

(defun find-monom-divisor (monom monoms)
  (declare (list monoms))
  (find-if (lambda (divisor)
	     (monom-divides divisor monom))
	   monoms))

(defun leading-monoms (basis)
  (sort
   (mapcar (lambda (poly) (cdar poly))
           basis)
   #'lex-less))

(defun is-zero-point (point)
  (declare (list point))
  (not (find-if (lambda (x) (not (zerop (the fixnum x))))
		point)))

(defun for-all-monoms-under-sub (dim fixed corners func)
  (when (zerop dim)
    (apply func (list (reverse fixed)))
    (return-from for-all-monoms-under-sub t))
  (let ((lim))
    (dolist (corner corners)
      (when (is-zero-point (cdr corner))
        (when (or (null lim) (> lim (car corner)))
          (setf lim (car corner)))))
    (if lim
        (let ((sorted-corners (sort (copy-list corners)
                                    (lambda (a b) (< (car a) (car b)))))
              (new-corners))
          (loop for i from 0 below lim do
               (loop while (and sorted-corners (<= (caar sorted-corners) i)) do
                    (push (cdar sorted-corners) new-corners)
                    (setf sorted-corners (cdr sorted-corners)))
               (unless (for-all-monoms-under-sub
                        (1- dim) (cons i fixed) new-corners func)
                 (return-from for-all-monoms-under-sub nil)))
          t)
        nil)))

(defun for-all-monoms-under (corners func)
  (for-all-monoms-under-sub (length (car corners)) nil corners func))

(defun for-young-diagram (corners func)
  (if (car corners)
      (loop for i from 0 do
           (let ((corners-new))
             (dolist (corner corners)
               (when (<= i (car corner))
                 (push (cdr corner) corners-new)))
             (unless corners-new (return))
             (for-young-diagram corners-new
                                (lambda (x) (apply func (list (cons i x)))))))
      (apply func (list nil))))

(defun for-total-young-diagram (n dim func)
  (if (zerop dim)
      (apply func (list nil))
      (loop for i from 0 below n do
           (let ((k (floor (/ n (1+ i)))))
             (for-total-young-diagram
              k (1- dim)
              (lambda (x)
                (apply func (list (cons i x)))))))))

(defun n-monoms-under (corners)
  (let ((res 0))
    (when (for-all-monoms-under
           corners
           (lambda (monom) (declare (ignore monom)) (incf res)))
      res)))

(defun hash-table-for-nf (basis)
  (let ((leading (leading-monoms basis))
        (i 0)
        (result (make-hash-table :test 'equalp)))
    (declare (fixnum i))
    (for-all-monoms-under
     leading
     (lambda (monom)
       (setf (gethash monom result) i)
       (incf i)))
    result))

(defun term-divides (term1 term2)
  (monom-divides (cdr term1) (cdr term2)))

(defun poly-sub (poly1 poly2)
  (when (null poly2)
    (return-from poly-sub poly1))
  (let ((term2 (car poly2))
	(result nil) (coef 0) (found nil))
    (loop for term1 in poly1 do
	  (if (equalp (cdr term1) (cdr term2))
	      (progn
		(setq found t)
		(setq coef (- (car term1) (car term2)))
		(when (not (zerop coef))
		  (setq result (cons (cons coef (cdr term1))
				     result))))
	    (setq result (cons term1 result))))
    (unless found
      (setq result (cons (cons (- (car term2))
			       (cdr term2))
			 result)))
    (poly-sub result (cdr poly2))))

(defun poly-negate (poly)
  (mapcar (lambda (term) (cons (- (car term)) (cdr term))) poly)) 

(defun poly-sum (poly1 poly2)
  (poly-sub poly1 (poly-negate poly2)))

(defun monom-mult (monom1 monom2)
  (mapcar '+ monom1 monom2))

(defun term-mult (term1 term2)
  (cons (* (car term1) (car term2))
	(monom-mult (cdr term1) (cdr term2))))

(defun poly-mult-term (poly term)
  (mapcar (lambda (poly-term) (term-mult poly-term term)) poly))

(defun poly-mult-poly (poly1 poly2)
  (let ((res nil))
    (dolist (term2 poly2)
      (setq res (poly-sum res (poly-mult-term poly1 term2))))
    res))

(defun poly-composition (poly composition)
  (let ((res nil))
    (dolist (term poly)
      (let ((cur (list
                  (cons (car term)
                        (make-zero-monom
                         (1- (length (caar composition))))))))
        (loop for degree in (cdr term)
              for subst-poly in composition do
              (loop for i from 0 below degree do
                   (setq cur (poly-mult-poly cur subst-poly))))
        (setq res (poly-sum res cur))))
    res))

(defun monom-divide (monom1 monom2)
  (mapcar '- monom1 monom2))

(defun vector-sub (vec1 vec2)
  (mapcar '- vec1 vec2))

(defun term-divide (term1 term2)
  (cons (/ (car term1) (car term2))
	(monom-divide (cdr term1) (cdr term2))))

(defun normal-form (poly base)
  (if (null poly) nil
      (progn
        (loop for term in poly
           do
             (loop for basepoly in base
                do
                  (if (term-divides (car basepoly) term)
                      (return-from normal-form
                        (normal-form
                         (poly-sub
                          poly
                          (poly-mult-term
                           basepoly
                           (term-divide term (car basepoly))))
                         base)))))
        poly)))

(defun monom-mult-to-var-no (monom varno)
  (let ((res (copy-list monom)))
    (incf (the fixnum (nth varno res)))
    res))

(defun term-mult-to-var-no (term varno)
  (cons (car term)
        (monom-mult-to-var-no (cdr term) varno)))

(defun poly-mult-to-var-no (poly varno)
  (mapcar (lambda (term)
                  (term-mult-to-var-no term varno))
          poly))


(defun poly-normalize (poly)
  (let ((lead-coef (caar poly)))
    (mapcar (lambda (term)
              (cons (/ (car term) lead-coef)
                    (cdr term)))
            poly)))

(defun poly-normalize-lex (poly)
  (poly-normalize
   (sort
    (copy-list poly)
    (lambda (x y) (lex-more (cdr x) (cdr y))))))

(defun poly-normalize-lex-keep-marked (poly)
  (poly-normalize
   (cons (car poly)
         (sort
          (copy-list (cdr poly))
          (lambda (x y) (lex-more (cdr x) (cdr y)))
          ))))

(defun basis-normalize-lex-keep-marked (basis)
  (sort (copy-list (mapcar #'poly-normalize-lex-keep-marked basis))
        (lambda (a b) (lex-more (cdar a) (cdar b)))))

(defun basis-normalize-lex-no-monomial-order (basis)
  (labels ((poly-more (poly1 poly2)
             (unless poly1
               (return-from poly-more nil))
             (unless poly2
               (return-from poly-more t))
             (when (lex-more (cdar poly1) (cdar poly2))
               (return-from poly-more t))
             (when (lex-less (cdar poly1) (cdar poly2))
               (return-from poly-more nil))
             (poly-more (cdr poly1) (cdr poly2))))
    (sort (copy-list (mapcar #'poly-normalize-lex basis))
          #'poly-more)))

(defun vector-orient-from-origin (vec)
  (declare (list vec))
  (if (find-if (lambda (x) (declare (fixnum x)) (> x 0)) vec)
      vec
    (mapcar #'- vec)))

(defun for-all-subsets (list-set size func)
  (declare (fixnum size) (list list-set) (function func))
  (if (zerop size)
      (funcall func nil)
      (when list-set
        (for-all-subsets
         (cdr list-set) (1- size)
         (lambda (s) (funcall func (cons (car list-set) s))))
        (when (< size (length list-set))
          (for-all-subsets (cdr list-set) size func)))))

(defun for-all-subsets-2 (list func)
  (declare (list list) (function func))
  (unless list
    (funcall func nil))
  (when list
    (for-all-subsets-2
     (cdr list) 
     (lambda (s) (funcall func (cons (car list) s))))
    (for-all-subsets-2 (cdr list) func)))

(defun for-all-max-subsets (set func)
  (if set
      (let ((saved))
        (for-all-max-subsets
         (cdr set)
         (lambda (s)
           (when (funcall func (cons (car set) s))
             (push s saved))))
        (for-all-max-subsets
         (cdr set)
         (lambda (s)
           (or (position s saved :test #'equalp)
               (funcall func s)))))
      (progn
        (funcall func nil)
        nil)))

(defun for-all-cuts (list func)
  (declare (list list) (function func))
  (when list
    (funcall func (car list) (cdr list)))
  (when (cdr list)
    (for-all-cuts (cdr list)
		  (lambda (elem cut)
		    (funcall func elem (cons (car list) cut))))))

(defun for-all-increments (point func)
  (declare (function func))
  (apply func (list (cons (the fixnum (1+ (the fixnum (car point))))
			  (cdr point))))
  (when (cdr point)
    (for-all-increments
     (cdr point)
     (lambda (x) (apply func (list (cons (car point) x))))))
  nil)

(defun for-all-decrements (point func)
  (declare (function func))
  (when (not (zerop (car point)))
    (apply func (list (cons (the fixnum (1- (the fixnum (car point))))
                            (cdr point)))))
  (when (cdr point)
    (for-all-decrements
     (cdr point)
     (lambda (x) (apply func (list (cons (car point) x)))))))

(defun find-dividend (monom corners)
  (declare (list corners))
  (find-if (lambda (corner)
	     (monom-divides monom corner))
	   corners))

(defun find-divisor (monom corners)
  (declare (list corners))
  (find-if (lambda (corner)
	     (monom-divides corner monom))
	   corners))

(defun poly-get-support (poly)
  (let ((res nil))
    (loop for (coef . monom) in poly do
         (when (not (find-dividend monom res))
           (setq res (remove-if (lambda (res-monom)
                                  (monom-divides res-monom monom))
                                res))
           (push monom res)))
    (sort res #'lex-less)))

(defun for-all-pairs (set func)
  (dolist (elem (cdr set))
    (funcall func (car set) elem))
  (when (cdr set)
    (for-all-pairs (cdr set) func)))

(defun monom-lcm (a b)
  (mapcar #'max a b))

(defun term-lcm (a b)
  (cons (* (car a) (car b))
        (monom-lcm (cdr a ) (cdr b))))

(defun min-lex (monoms)
  (let ((res (car monoms)))
    (if (cdr monoms)
        (let ((tmp (min-lex (cdr monoms))))
          (if (lex-less res tmp)
              res
              tmp))
        res)))

(defun buchberger-check (basis)
  (for-all-pairs
   basis
   (lambda (u v)
     (let* ((lcm (term-lcm (car u) (car v)))
            (u-mult (term-divide lcm (car u)))
            (v-mult (term-divide lcm (car v)))
            (diff
             (poly-sub
              (poly-mult-term u u-mult)
              (poly-mult-term v v-mult))))
       (when (normal-form diff basis)
         (return-from buchberger-check nil)))))
  t)

(defun make-unit-vector (i)
  (let ((result (make-list *dimension* :initial-element 0)))
    (incf (nth i result))
    result))

(defun make-unit-vectors ()
  (let ((result))
    (loop for i from 0 below *dimension* do
          (push (make-unit-vector i) result))
    result))

(defun degenerate (vectors)
  (let* ((dim (length (car vectors)))
         (n (length vectors))
         (matr (make-array (list n dim)))
         (vmatr (make-array n :initial-element nil)))
    (loop for vector in vectors
          for i from 0 do
          (loop for coord in vector
                for j from 0 do
                (setf (aref matr i j) coord)))
    (loop for i from 0 below dim do
          (loop for j from 0 below n do
                (when (and (not (aref vmatr j))
                           (not (zerop (aref matr j i))))
                  (setf (aref vmatr j) t)
                  (loop for k from 0 below n do
                        (when (/= k j)
                          (let ((c1 (aref matr j i))
                                (c2 (aref matr k i)))
                            (loop for l from i below dim do
                                  (setf (aref matr k l)
                                        (- (* c1
                                              (aref matr k l))
                                           (* c2
                                              (aref matr j l))))
                                  ))))
                  (return))
                
                finally (return-from degenerate t)))
    nil))

(defun monom-is-power (monom)
  (let ((idx nil))
  (loop for deg in monom for i from 0 do
       (when (> deg 0)
         (if idx
             (return-from monom-is-power nil)
             (setf idx i))))
  idx))

(defun for-all-representatives (lists func)
  (if (car lists)
    (dolist (elem (car lists))
      (for-all-representatives
       (cdr lists)
       (lambda (x)
         (funcall func (cons elem x)))))
    (funcall func nil)))

(defun for-all-representatives-2 (lists func)
  (labels ((sub (already lists func)
             (if (car lists)
                 (if (find-if
                          (lambda (x)
                            (find-if (lambda (y) (monom-divides x y)) (car lists)))
                          already)
                     (sub already (cdr lists) func)
                     (dolist (elem (car lists))
                       (sub
                        (cons elem already)
                        (cdr lists)
                        (lambda (x)
                          (funcall func (cons elem x))))))
                 (funcall func nil))))
    (sub nil lists func)))

(deftype arrfixnum () '(simple-vector *))