(defpackage main
  (:use :common-lisp :dimension
        :algebra :poly-io :external-helpers :find-eugb :cl-ppcre
        #+cmu :ext
        #+sbcl :sb-ext))

(in-package main)

(defun main ()
  (let ((vars)
        (basis)
        (poly-io:*vars* nil)
        )
    
    ;(setf all-polys-hash (make-hash-table :test 'equalp))
    
    (format t "Input list of variables:")
    (force-output)
    (setf vars (cl-ppcre:split " " (read-line)))

    (setf *dimension* (length vars))
    (setf poly-io:*vars* vars)
    
    (format t "Input generators:")
    (force-output)
    (setf basis (parse-polys (read-line)))

    (setf basis (get-groebner-base basis))

    (format t "Got Groebner basis: ")
    (polys-print basis)
    (format t "~%")

    (let ((find-eugb:*on-poly*
           (lambda (poly) (poly-print poly) (format t "~%") ))
          (find-eugb:*on-give-up*
           (lambda (coideal)
             (format t "GIVING UP [ ")
             (dolist (monom coideal)
               (term-print (cons 1 monom))
               (format t " "))
             (format t "]~%"))))
      (find-eugb basis))))
