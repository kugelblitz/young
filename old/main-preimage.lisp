(defpackage main
  (:use :common-lisp :algebra :enumerate :poly-io :external-helpers
        #+cmu :ext
        #+sbcl :sb-ext))

(in-package main)

(defun main ()
  (let ((basis)
        (gfan)
        (gfan-lex)
        (basis-c)
        (gfan-c)
        (all-polys-hash)
        (gfan-polys-hash)
        (gfan-polys-hash-c)
        (n-monoms-under)
        (algebra:*dimension* nil)
        (poly-io:*vars* nil)
        (poly-idx 0)
        (vars)
        (vars-c)
        (composition)
        (preimage-basis))
    
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
    (format t "Groebner basis: " )
    (let ((poly-io:*vars* vars-c))
      (setf basis-c (get-groebner-base basis-c))
      (polys-print basis-c))
    (format t "~%~%")

    (setf preimage-basis
          (calculate-preimage
           vars vars-c composition basis-c))

    (format t "preimage: ")
    (let ((poly-io:*vars* vars))
      (polys-print preimage-basis))
    (format t "~%~%")

    (setf gfan-polys-hash (make-hash-table :test 'equalp))
    (setf gfan-polys-hash-c (make-hash-table :test 'equalp))

    (let ((poly-io:*vars* vars))
      (setf gfan (get-gfan preimage-basis)))
    (let ((poly-io:*vars* vars-c))
      (setf gfan-c (get-gfan basis-c)))

    (format t "preimage gfan:~%")
    (dolist (gfan-basis gfan)
      (let ((poly-io::*vars* vars)) (polys-print gfan-basis))
      (format t "~%")
      (dolist (poly gfan-basis)
        (setf (gethash (poly-normalize-lex poly) gfan-polys-hash) t)))
    (format t "~%")

    (setf gfan-lex (mapcar #'basis-normalize-lex-no-monomial-order gfan))

    (format t "preimage gfan (LEX) :~%")

    (let ((poly-io::*vars* vars))
      (dolist (basis gfan-lex)
        (polys-print basis)
        (format t "~%"))
      (format t "~%"))

    
    (format t "image gfan:~%")
    (dolist (gfan-basis gfan-c)
      (let ((poly-io::*vars* vars-c)) (polys-print gfan-basis))
      (format t "~%")
      (dolist (poly gfan-basis)
        (setf (gethash (poly-normalize-lex poly) gfan-polys-hash-c) t)))
    (format t "~%")

    (setq n-monoms-under (n-monoms-under (leading-monoms basis-c)))
    (when (null n-monoms-under)
      (format t "Bad basis!~%")
      (quit))

    (format t "~a monoms under basis" n-monoms-under)
  
    (format t "~%Enumeration...~%~%")

    (flet ((on-finish (env)
             (let ((polys)
                   (poly-pre)
                   (polys-pre))
               (dolist (node (getf env :finished))
                 (let ((poly (getf node :basis-poly)))
                   (push poly polys)
                   (let ((poly-io:*vars* vars-c))
                     (poly-print poly))
                   (let ((poly-norm (poly-normalize-lex poly)))
                     (unless (gethash poly-norm all-polys-hash)
                       (setf (gethash poly-norm all-polys-hash) poly-idx)
                       (incf poly-idx)))
                   (format t "~%")))
               (format t "~%")
               (dolist (poly polys)
                 (let ((preimage-polys
                        (dimcalc:get-diagram-intersection-polys
                         (preimage:young-diagram-preimage
                          (algebra:poly-get-support poly)
                          composition)
                         preimage-basis)))
                   (when (= 1 (length preimage-polys))
                     (setf poly-pre (poly-normalize-lex (car preimage-polys)))
                     (when (equalp
                            (poly-normalize-lex (poly-composition poly-pre composition))
                            (poly-normalize-lex poly))
                       (unless (find poly polys-pre :test #'equalp)
                         (push poly polys-pre))
                       ))
                   ))
               (setf polys-pre (basis-normalize-lex-no-monomial-order polys-pre))
               (let ((poly-io::*vars* vars))
                   (polys-print polys-pre))
               (format t "~%")
               (format t "~%")
               (when (find polys-pre gfan-lex :test #'equalp)
                 (format t "~%!!!!!!!~%")))))
      
      (enumerate-max-diagrams (length vars-c) basis-c #'on-finish))
  
    (format t "~%Polynoms found:~%~%")
  
    (loop for poly being the hash-keys of all-polys-hash do
         (let ((poly-io:*vars* vars-c))
           (poly-print poly)
           (format t "~%"))
         (if
          (gethash (poly-normalize-lex poly) gfan-polys-hash-c)
          (format t "  (in gfan)~%")
          (format t "  (not in gfan)~%"))
         (unless (= 1 (dimcalc:get-intersection-dim poly basis-c))
           (error "dim != 1"))
         (let ((preimage-polys
                (dimcalc:get-diagram-intersection-polys
                 (preimage:young-diagram-preimage
                  (algebra:poly-get-support poly)
                  composition)
                 preimage-basis)))
           (format t "  (dim on preimage = ~a)~%" (length preimage-polys))
           (dolist (poly-pre preimage-polys)
             (let ((poly-io:*vars* vars))
               (format t "  preimage polynom: ")
               (poly-print poly-pre)
               (if (equalp (poly-normalize-lex
                              (poly-composition poly-pre composition))
                             (poly-normalize-lex poly))
                  (format t " (REALLY preimage) ")
                  (format t " (not preimage) "))
               
               (if
                (gethash (poly-normalize-lex poly-pre)
                         gfan-polys-hash)
                (format t " (in gfan)~%")
                (format t " (not in gfan)~%"))
;;               (format t "   ~a ~a"
;;                       (algebra:poly-get-support
;;                        (algebra:poly-composition poly-pre composition))
;;                       (preimage:young-diagram-image
;;                        (algebra:poly-get-support poly-pre)
;;                        composition))
               )
               ))
           (format t "~%"))))
    
(main)
(quit)
