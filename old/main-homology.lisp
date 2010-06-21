(defpackage main
  (:use :common-lisp :algebra :enumerate :poly-io :external-helpers
        #+cmu :ext
        #+sbcl :sb-ext))

(in-package main)

(defun get-simplices-str (polys-hash bases)
  (with-output-to-string (*standard-output*)
    (format t "[")
    (dolist (basis bases)
      (format t "[")
      (dolist (poly basis)
        (format t "~a," (gethash (poly-normalize-lex poly) polys-hash)))
      (format t "],"))
    (format t "]")
    ))

(defun check-connectivity-sub (basis1 basis2)
  (do ((polys1 basis1 (cdr polys1)))
      ((null polys1))
    (do ((polys2 basis2 (cdr polys2)))
        ((not polys2))
      (when (and (equalp (poly-normalize-lex (car polys1))
                         (poly-normalize-lex (car polys2)))
                 (not (equalp (cdaar polys1)
                              (cdaar polys2))))
        (return-from check-connectivity-sub t))))
  ;;(format t "no connectivity:~%")
  ;;(polys-print basis1)
  ;;(format t "~%")
  ;;(polys-print basis2)
  ;;(format t "~%")
  nil)

(defun check-connectivity (gfan-bases)
  (do ((bases1 gfan-bases (cdr bases1)))
      ((null bases1))
    (let ((connected nil))
      (do ((bases2 gfan-bases (cdr bases2)))
          ((null bases2))
        (unless (equalp (car bases1) (car bases2))
          (when (check-connectivity-sub (car bases1) (car bases2))
            (setf connected t))))
      (unless connected
        (print "UNCONNECTED BASIS")
        (polys-print (car bases1))))))

(defun main ()
  (let ((basis)
        (gfan-bases)
        (gfan-bases-hash)
        (all-bases)
        (all-bases-hash)
        (all-polys-hash)
        (n-monoms-under)
        (algebra:*dimension* nil)
        (poly-io:*vars* nil)
        (poly-idx 0)
        (n-gfan-polys 0))
    
    (setf gfan-bases-hash (make-hash-table :test 'equalp))
    (setf all-polys-hash (make-hash-table :test 'equalp))
    (setf all-bases-hash (make-hash-table :test 'equalp))
    
    (format t "Input Groebner fan: ")
    (force-output)
    (setf gfan-bases (parse-gfan-bases))
    (setf gfan-bases (mapcar #'basis-normalize-lex-keep-marked gfan-bases))

    (setf algebra:*dimension* (length poly-io:*vars*))
    (format t "~%~%Got Groebner fan:~%")
    (dolist (gfan-basis gfan-bases)
      (setf (gethash gfan-basis gfan-bases-hash) gfan-basis)
      (setf (gethash gfan-basis all-bases-hash) t)
      (dolist (poly gfan-basis)
        (let ((poly-norm (poly-normalize-lex poly)))
          (unless (gethash poly-norm all-polys-hash)
            (setf (gethash poly-norm all-polys-hash) poly-idx)
            (incf poly-idx)
            (incf n-gfan-polys))))
      (polys-print gfan-basis)
      (format t "~%"))
    
    (setf basis (basis-normalize-lex-keep-marked (car gfan-bases)))

    (check-connectivity gfan-bases)
    
    (format t "~%~%Considering basis ")
    (polys-print basis)
    (format t "~%")

    (print (leading-monoms basis))

    (setq n-monoms-under (n-monoms-under (leading-monoms basis)))
    (when t ;;(null n-monoms-under)
      (format t "~%~%Groebner fan simplices:~%")  
      (print-homologies (get-simplices-str all-polys-hash gfan-bases))
      (format t "Bad basis!~%")
      (quit))

    (format t "~a monoms under basis" n-monoms-under)
  
    (format t "~%Enumeration...~%~%")

    (flet ((on-finish (env)
             (let ((length (length (getf env :seq)))
                   (subspace-basis)
                   (subspace-groebner-basis)
                   (n-polys-in-gfan 0))
               (when (/= length n-monoms-under)
                 (return-from on-finish))
               (dolist (node (getf env :finished))
                 (let ((poly (getf node :basis-poly)))
                   (push poly subspace-basis)))

               (setf subspace-basis
                     (basis-normalize-lex-keep-marked subspace-basis))
               
               (setf subspace-groebner-basis
                     (basis-normalize-lex-keep-marked
                      (get-groebner-base subspace-basis)))

               (unless (equalp subspace-groebner-basis basis)
                 (return-from on-finish))

               (dolist (poly subspace-basis)
                 (poly-print poly)
                 (let ((poly-norm (poly-normalize-lex poly)))
                   (unless (gethash poly-norm all-polys-hash)
                     (setf (gethash poly-norm all-polys-hash) poly-idx)
                     (incf poly-idx))
                   (when (< (gethash poly-norm all-polys-hash) n-gfan-polys)
                     (incf n-polys-in-gfan)))
                 (format t "~%"))

               (format t "have ~a of GFan polys~%" n-polys-in-gfan)
               (when (zerop n-polys-in-gfan)
                 (error "WOW!"))
               (setf (gethash subspace-basis all-bases-hash) t)
               (unless (gethash subspace-basis gfan-bases-hash)
                 (format t "--> Is not present in GFan~%"))
               
               (format t "~%"))))
      (enumerate-max-diagrams (length poly-io:*vars*) basis #'on-finish))
  
    (format t "~%Polynoms found:~%")
  
    (loop for poly being the hash-keys of all-polys-hash do
         (poly-print poly)
         (format t "~%"))

    (format t "~%Not in universal basis:~%")

    (if (= poly-idx n-gfan-polys)
        (format t "<none>~%")
        (loop for poly being the hash-keys of all-polys-hash do
             (when (>= (gethash poly all-polys-hash) n-gfan-polys)
               (poly-print poly)
               (format t "~%"))))
    
    (format t "~%~%Groebner fan simplices:~%")  
    (print-homologies (get-simplices-str all-polys-hash gfan-bases))
    
    (format t "~%~%Extended Groebner fan simplices:~%")
    (loop for basis being the hash-keys of all-bases-hash do
         (push basis all-bases))
    (print-homologies (get-simplices-str all-polys-hash all-bases)))
  (quit))

(main)
