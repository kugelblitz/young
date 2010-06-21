(defpackage main
  (:use :common-lisp :poly-io :algebra :enumerate
        #+cmucl :ext
        #+sbcl :sb-ext))

(in-package main)

(defun main ()
  (let ((bases-hash))
    
    (setf bases-hash (make-hash-table :test 'equalp))
    (format t "Input Groebner fan: ")
    (force-output)

    (multiple-value-bind (gfan-bases vars)
        (poly-io::parse-gfan-bases)
    
      (let ((poly-io::*vars* vars))
        (format t "~%~%Got Groebner fan:~%")
        (dolist (gfan-basis gfan-bases)
          (poly-io::polys-print gfan-basis)
          (format t "~%"))
        
        (enumerate::enumerate-admissible-orderings-3
         (lambda (seq basis)
           (format t "got sequence ~a~%" seq)
           (push seq (gethash basis bases-hash)))
         :bases gfan-bases
         )
        
        (loop for basis being the hash-keys of bases-hash do
             (format t "~%")
             (poly-io::polys-print basis)
             (format t "~%")
             (dolist (seq (gethash basis bases-hash))
               (format t "~a~%" seq)))
        ))))
    
(main)
(quit)
