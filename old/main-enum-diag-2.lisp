(defpackage main
  (:use :common-lisp :external-helpers :algebra :enum-diag :calculus
        :dim-representation :enum-diag :young-diagram :random-grow
        #+cmu :ext
        #+sbcl :sb-ext))

(in-package main)

(defvar *xi*)
(defvar *avg-dim*)

(defun for-all-betas (func)
  (loop for beta from 2 to 2 by 2
     do
       (funcall func beta)))

(defun main (args)
  (let* ((nfound 0)
         (dim (parse-integer (first args)))
         (n (parse-integer (second args)))
         (nord-max 0)
         (lnf (log-n-fact n))
         (nfact (factorial n))
         )
    (setf *xi* nil)
    (setf *avg-dim* nil)
    (for-all-betas (lambda (beta) (push 0 *xi*) (push 0 *avg-dim*)))
    (flet ((cb-diagram (inner outer)
             (incf nfound)
             (when (zerop (mod nfound 100000))
               (print nfound))
             (let ((nord
                    (calc-orderings-in-diagram
                     dim
                     inner
                     nil))
                   (xi-iter *xi*)
                   (avg-dim-iter *avg-dim*))
               (when (> nord nord-max)
                 (setf nord-max nord)
                 (print inner))
               (for-all-betas
                (lambda (beta)
                  (unless (zerop (mod beta 2))
                    (error "sorry, only even betas allowed"))
                  (setf beta (/ beta 2))
                  (let ((bval (expt (/ (* nord nord) nfact) beta))
                        (lval (- (log nord) (* lnf 0.5))))
                    (incf (car avg-dim-iter) (* bval lval))
                    (incf (car xi-iter) bval)
                    (setf avg-dim-iter (cdr avg-dim-iter))
                    (setf xi-iter (cdr xi-iter))
                    )))
               )))
      ;(enum-diag::enumerate-symmetric-diagrams-2d n #'cb-diagram)
      (enum-diag::enumerate-diagrams dim n #'cb-diagram)
      )
    (print nfound)
    (print (/ (- (log nord-max) (* lnf 0.5)) (sqrt n)))
    
    (format t "~%AVG:~%")
    (let ((xi-iter *xi*)
          (avg-dim-iter *avg-dim*))
      (for-all-betas
       (lambda (beta)
         (format t "~a (~a) : ~a~%" beta (car xi-iter) (/ (car avg-dim-iter) (* (sqrt n) (car xi-iter))))
         (setf avg-dim-iter (cdr avg-dim-iter))
         (setf xi-iter (cdr xi-iter)))))
  nil))

(main (get-args))
(quit)
