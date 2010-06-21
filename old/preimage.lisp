(defpackage preimage
  (:use :common-lisp :algebra)
  (:export young-diagram-preimage
           young-diagram-image
           make-outer-monoms
           add-inner-monom
           make-env))

(in-package preimage)

(defun make-outer-monoms (new-monom env)
  (algebra:for-all-increments
   new-monom
   (lambda (new-outer-monom)
     (unless
         (algebra::find-monom-divisor
          new-outer-monom
          (getf env :outer))
       (setf (getf env :outer) (copy-list (cons (copy-list new-outer-monom) (getf env :outer))))
       )))
  nil)

(defun add-inner-monom (new-monom env)
  (let ((inner-corners-new
         (cons new-monom
               (remove-if
                (lambda (x)
                  (algebra::monom-divides x new-monom))
                (getf env :inner)))))
    (setf (getf env :inner)
          (sort (copy-list inner-corners-new)
                #'algebra::lex-less)))
    nil)

(defun make-env ()
  (list :outer nil :inner nil))

(defun diagram-belongs (diag-a diag-b)
  (loop for monom in diag-a do
       (unless (algebra:find-dividend monom diag-b)
         (return-from diagram-belongs nil)))
  t)

(defun try-grow-diagram (env inner-corners composition)
  (let* ((new-monom (copy-list (car (getf env :outer))))
         (new-poly (algebra:poly-composition
                    (list (cons 1 new-monom)) composition)))
    (pop (getf env :outer))
    (unless (diagram-belongs (algebra:poly-get-support new-poly)
                             inner-corners)
      (return-from try-grow-diagram nil))
    (make-outer-monoms new-monom env)
    (add-inner-monom new-monom env)
    ))

(defun young-diagram-preimage (inner-corners composition)
  (let ((env (make-env))
        (algebra:*dimension* (length composition)))
    (push (algebra:make-zero-monom) (getf env :outer))
    (loop while (getf env :outer) do
         (try-grow-diagram env inner-corners composition))
    (getf env :inner)))

(defun young-diagram-image (inner-corners composition)
  (let ((env (make-env)))
    (for-young-diagram
     inner-corners
     (lambda (monom)
       (let ((diagram-c
              (algebra:poly-get-support
               (algebra:poly-composition
               (list (cons 1 monom)) composition))))
         (dolist (monom-c diagram-c)
           (add-inner-monom monom-c env)))))
    (sort (getf env :inner) #'algebra:lex-less)))
