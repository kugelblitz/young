(defpackage main
  (:use :common-lisp :algebra :enumerate :poly-io :external-helpers
        #+cmu :ext
        #+sbcl :sb-ext))

(in-package main)

(defun main ()
  (let ((basis)
        (gfan-bases)
        (gfan-bases-hash)
        (all-bases-hash)
        (all-polys-hash)
        (n-monoms-under)
        (algebra:*dimension* nil)
        (poly-io:*vars* nil)
        (poly-idx 0)
        (n-gfan-polys 0)
        (basis-after-composition)
        (composition))
    
    (setf gfan-bases-hash (make-hash-table :test 'equalp))
    (setf all-polys-hash (make-hash-table :test 'equalp))
    (setf all-bases-hash (make-hash-table :test 'equalp))
    
    (format t "Input Groebner fan: ")
    (force-output)
    (setf gfan-bases (parse-gfan-bases))

    (dolist (var poly-io:*vars*)
      (format t "Input substitution for ~a:" var)
      (force-output)
      (push (parse-poly (read-line)) composition))
    (setf composition (nreverse composition))

    (setf algebra:*dimension* (length poly-io:*vars*))
    (format t "~%~%Got Groebner fan:~%")
    (dolist (gfan-basis gfan-bases)
      (let ((gfan-basis-norm (basis-normalize-lex gfan-basis)))
        (setf (gethash gfan-basis-norm gfan-bases-hash) gfan-basis)
        (setf (gethash gfan-basis-norm all-bases-hash) t)
        (dolist (poly gfan-basis)
          ;;(print poly)
          (let ((poly-norm (poly-normalize-lex poly)))
            ;;(print poly-norm)
            (unless (gethash poly-norm all-polys-hash)
              (setf (gethash poly-norm all-polys-hash) poly-idx)
              (incf poly-idx)
              (incf n-gfan-polys))))
        (polys-print gfan-basis-norm)
        (format t "~%")))

    (format t "~%Bases after composition:~%~%")
    (dolist (gfan-basis gfan-bases)
      (setf basis-after-composition nil)
      (dolist (poly gfan-basis)
        (let ((poly-after-composition
               (poly-composition poly composition)))
          (poly-print poly)
          (format t "  --> ")
          (poly-print poly-after-composition)
          (format t "~%")
          (push poly-after-composition basis-after-composition)))
      (format t "~%")
      (format t "Groebner basis: " )
      (polys-print ;;(basis-normalize-lex-keep-marked
                    (get-groebner-base basis-after-composition)
                    ;;)
                   )
      (setf basis-after-composition (get-groebner-base basis-after-composition))
      (format t "~%~%"))
    
    (setf basis (basis-normalize-lex (car gfan-bases)))
    
    (format t "~%~%Considering basis ")
    (polys-print basis)
    (format t "~%")

    (setq n-monoms-under (n-monoms-under (leading-monoms basis)))
    (when (null n-monoms-under)
      (format t "Bad basis!~%")
      (quit))

    (format t "~a monoms under basis" n-monoms-under)
  
    (format t "~%Enumeration...~%~%")

    (flet ((on-finish (env)
             (let ((length (length (getf env :seq)))
                   (subspace-basis)
                   (subspace-groebner-basis))
               (when (/= length n-monoms-under)
                 (format t "Small subspace found (~a monoms) :~%" length))
               (dolist (node (getf env :finished))
                 (let ((poly (getf node :basis-poly)))
                   (push poly subspace-basis)
                   (poly-print poly)
                   (let ((poly-norm (poly-normalize-lex poly)))
                     (unless (gethash poly-norm all-polys-hash)
                       (setf (gethash poly-norm all-polys-hash) poly-idx)
                       (incf poly-idx)))
                   (format t "~%")))

               (setf subspace-basis (basis-normalize-lex subspace-basis))
               (setf subspace-groebner-basis
                     (basis-normalize-lex (get-groebner-base subspace-basis)))

               (setf (gethash subspace-basis all-bases-hash) t)
               (unless (gethash subspace-basis gfan-bases-hash)
                 (format t "--> Is not present in GFan~%")
               
                 (unless (equalp subspace-groebner-basis basis)
                   (format t "--> Smaller ideal: ")
                   (polys-print subspace-groebner-basis)
                   (format t "~%")))
               
               (format t "~%"))))
      (enumerate-max-diagrams (length poly-io:*vars*) basis #'on-finish))
  
    (format t "~%Polynoms found:~%")
  
    (loop for poly being the hash-keys of all-polys-hash do
         (poly-print poly)
         (format t " (dim = ~a) " (dimcalc:get-intersection-dim poly basis))
         (let ((poly-comp (algebra:poly-composition poly composition)))
           ;;(poly-print poly-comp)
           (format t "(dim after composition = ~a)~%"
                   (dimcalc:get-intersection-dim
                    poly-comp
                    basis-after-composition))))
         

    (format t "~%Not in universal basis:~%")

    (if (= poly-idx n-gfan-polys)
        (format t "<none>~%")
        (loop for poly being the hash-keys of all-polys-hash do
             (when (>= (gethash poly all-polys-hash) n-gfan-polys)
               (poly-print poly)
               (format t "~%")
               )))))
    
(main)
(quit)
