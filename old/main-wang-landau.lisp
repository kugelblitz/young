(defpackage main
  (:use :common-lisp :algebra :preimage
        #+cmu :ext
        #+sbcl :sb-ext))

(in-package main)

(defun copy-env (env)
  (list :outer (copy-list (getf env :outer))
        :inner (copy-list (getf env :inner))))

(defun for-all-decrements (point func)
  (declare (function func))
  (unless (zerop (car point))
    (apply func (list (cons (the fixnum (1- (the fixnum (car point))))
                            (cdr point)))))
  (when (cdr point)
    (for-all-decrements
     (cdr point)
     (lambda (x) (apply func (list (cons (car point) x))))))
  nil)

(defun remove-inner-monom (env monom)
  (setf (getf env :inner)
        (remove-if
         (lambda (m) (equalp m monom))
         (getf env :inner)))
  (for-all-decrements
   monom
   (lambda (new-inner)
     (unless (find-monom-divisor new-inner (getf env :inner))
       (push new-inner (getf env :inner)))))
  (setf (getf env :outer)
        (remove-if
         (lambda (m)
           (monom-divides m monom))
         (getf env :outer)))
  (push monom (getf env :outer)))

(defun walk-one-step (env p-arr n-arr)
  (let* ((k (+ (length (getf env :outer))
               (length (getf env :inner))))
         (r (random k))
         (new-env (copy-env env))
         (n-old (n-monoms-under (getf env :inner)))
         (n-new)
         (prob))
    (if (<= r (length (getf env :outer)))
        (let ((monom (nth r (getf env :outer))))
          (make-outer-monoms monom new-env)
          (add-inner-monom monom new-env))
        (let ((monom (nth (- r (length (getf env :outer))) (getf env :inner))))
          (remove-inner-monom new-env monom)))
    (setf n-new (n-monoms-under (getf new-env :inner)))
    (setf prob (min (aref p-arr n-old) (aref p-arr n-new)))
    (when (< (random (aref p-arr n-new)) prob)
      (setf (getf env :outer) (getf new-env :outer))
      (setf (getf env :inner) (getf new-env :inner)))))


