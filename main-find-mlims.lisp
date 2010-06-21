(defpackage main
  (:use :common-lisp :dimension
        :algebra :poly-io :external-helpers :find-mlims :cl-ppcre
        #+cmu :ext
        #+sbcl :sb-ext))

(in-package main)

(defun main ()
  (let ((vars)
        (basis)
        (poly-io:*vars* nil))
    
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

    (let ((find-mlims:*on-mlims*
           (lambda (func) (format t "~a~%" (funcall func :corners))))
          )
      (find-mlims basis))))
