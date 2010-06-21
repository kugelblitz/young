(defpackage algebra
  (:use :common-lisp :dimension)
  (:export
   arrfixnum
   lex-more
   lex-less
   monom-divides
   min-lex
   poly-sub
   poly-mult-term
   normal-form
   find-divisor
   term-divide
   term-divides
   ))

(in-package algebra)

(deftype arrfixnum () '(simple-vector *))

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

(defun min-lex (monoms)
  (let ((res (car monoms)))
    (if (cdr monoms)
        (let ((tmp (min-lex (cdr monoms))))
          (if (lex-less res tmp)
              res
              tmp))
        res)))

(defun monom-divides (monom1 monom2)
  (cond ((null monom1) t)
        ((> (the fixnum (car monom1)) (the fixnum (car monom2))) nil)
        (t (monom-divides (cdr monom1) (cdr monom2)))))

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

(defun monom-divide (monom1 monom2)
  (mapcar '- monom1 monom2))

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

(defun find-divisor (monom corners)
  (declare (list corners))
  (find-if (lambda (corner)
	     (monom-divides corner monom))
	   corners))
