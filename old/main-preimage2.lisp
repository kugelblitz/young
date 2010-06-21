(defpackage main
  (:use :common-lisp
        :algebra
        :poly-io :external-helpers :search-bases :preimage :search-bases
        #+cmu :ext
        #+sbcl :sb-ext))

(in-package main)

(defun main ()
  (let ((basis)
        (basis-c)
        (all-polys-hash)
        (algebra:*dimension* nil)
        (poly-io:*vars* nil)
        (vars)
        (vars-c)
        (composition)
        (preimage-basis)
        (dim1-polys)
        (dim1-polys-image)
        )
    
    (setf all-polys-hash (make-hash-table :test 'equalp))
    
    (format t "Input list of variables:")
    (force-output)
    (setf vars (cl-ppcre:split " " (read-line)))

    (setf algebra:*dimension* (length vars))
    (setf enumerate::*dimension* (length vars))
    
    (format t "Input basis:")
    (force-output)
    (let ((poly-io:*vars* vars))
      (setf basis (parse-polys (read-line))))

    (format t "Input list of variables for substitution:")
    (force-output)
    (setf vars-c (cl-ppcre:split " " (read-line)))
    
    (dolist (var vars)
      (format t "Input substitution for ~a:" var)
      (force-output)
      (let ((poly-io:*vars* vars-c))
        (push (parse-poly (read-line)) composition)))
    (setf composition (nreverse composition))

    (format t "~%Substitution:~%")
    (loop
       for var in vars
       for poly in composition do
         (format t "~a -> " var)
         (let ((poly-io:*vars* vars-c))
           (poly-print poly))
         (format t "~%"))

    (format t "~%Basis after composition:~%~%")
    (dolist (poly basis)
      (let ((poly-c
             (poly-composition poly composition)))
        (let ((poly-io:*vars* vars))
          (poly-print poly))
        
        (format t "  --> ")
          
        (let ((poly-io:*vars* vars-c))
          (poly-print poly-c))
        
        (format t "~%")
        (push poly-c basis-c)))
    
    (format t "~%")
    (format t "Groebner basis:~%" )
    (let ((poly-io:*vars* vars-c))
      (setf basis-c (get-groebner-base basis-c))
      (polys-print basis-c "~%"))
    (format t "~%~%")

    (setf preimage-basis
          (calculate-preimage
           vars vars-c composition basis-c))

    (format t "preimage:~%")
    (let ((poly-io:*vars* vars))
      (polys-print preimage-basis "~%"))
    (format t "~%~%")

    (flet ((on-poly (poly)
             (let ((poly-image) (dim))
               
               (format t "found polynom in preimage: ")
               (let ((poly-io::*vars* vars))
                 (poly-print poly))
               (format t "~%")
               (setf poly-image
                     (poly-composition poly
                                       composition))
               (format t "  poly image: ")
               (let ((poly-io::*vars* vars-c))
                 (poly-print poly-image))
               (format t "~%")
               (when poly-image
                 (setf dim
                       (dimcalc:get-intersection-dim poly-image basis-c)))
               
               (format t "  dim=~a ~%" dim)
               (when (equalp 1 dim)
                 (push poly dim1-polys)
                 (push poly-image dim1-polys-image))
               (format t "~%")))
           )

      (search-for-bases-2 vars preimage-basis #'on-poly)
      )

    (loop
       for poly in dim1-polys
       for poly-image in dim1-polys-image
       do
         (format t "preimage poly: ")
         (let ((poly-io:*vars* vars))
           (poly-print poly))
         (format t "~%   image poly: ")
         (let ((poly-io:*vars* vars-c))
           (poly-print poly-image))
         (format t "~%~%"))
    
    
    (format t "~%Groebner basis of found preimage polys:~%" )
    (let* ((poly-io:*vars* vars)
           (basis-dim1 (get-groebner-base dim1-polys)))
      (polys-print basis-dim1 "~%"))
    (format t "~%~%Groebner basis of found image polys:~%" )
    (let* ((poly-io:*vars* vars-c)
           (basis-dim1 (get-groebner-base dim1-polys-image)))
      (polys-print basis-dim1 "~%"))
  
    ))
    
;(main)
;(quit)
