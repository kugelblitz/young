(defpackage external-helpers
  (:use :common-lisp :poly-io
        #+cmu :ext
        #+sbcl :sb-ext)
  (:export get-groebner-base print-homologies calculate-preimage get-gfan get-args))

(in-package external-helpers)

(defun get-args ()
  #+clisp ext:*args*
  #+cmu (nthcdr 5 lisp::lisp-command-line-list)
  #+gcl (nthcdr 3 si::*command-args*)
  #+acl (system:command-line-arguments t)   
  #+sbcl (cdr sb-ext:*posix-argv*))

(defparameter *current-directory*
  #+cmu (cdr (assoc :pwd  *environment-list*))
  #+sbcl (posix-getenv "PWD"))

(defun get-groebner-base-string (polys)
  (let ((polys-str (with-output-to-string (*standard-output*)
                     (polys-print polys)))
        (vars-str (with-output-to-string (*standard-output*)
                    (vars-print)))
        (result))
    (setf result
          (with-output-to-string (out)
            (run-program
             "/bin/sh"
             (list
              (concatenate 'string *current-directory* "/gb.sh")
              polys-str
              vars-str) :output out)
            out))
    (subseq result 0 (length result))))

(defun get-groebner-base (polys)
  (let* ((groebner-base-string (get-groebner-base-string polys))
         (matches (cl-ppcre:all-matches-as-strings
                   "\\[\\[(\\n|.)*\\]\\]" groebner-base-string)))
    (parse-basis-maxima (car matches))))

(defun print-homologies (simplices-str)
  (let ((script))
    (setf script
          (with-output-to-string (*standard-output*)
            (format t "RequirePackage(\"homology\");~%")
            (format t "complex := ~a;~%" simplices-str)
            (format t "Display(SimplicialHomology(complex));~%")
            (format t "quit;~%")))
            (with-input-from-string (in script)
              (run-program "/usr/bin/gap" (list "-q" "-b")
                           :output *standard-output*
                           :input in
                           #+sbcl :environment
                           #+sbcl (list "LD_LIBRARY_PATH=.")))))

(defun calculate-preimage (vars vars-c composition basis)
  (let ((poly-io:*vars* vars-c)
        (new-basis-str)
        (all-vars-str)
        (comma)
        (result)
        (matches))
    (setf new-basis-str
          (with-output-to-string (*standard-output*)
            (loop for var in vars
               for poly in composition do
                 (format t "-~a+" var)
                 (poly-print poly)
                 (format t ","))
            (polys-print basis)))
    (setf all-vars-str
          (with-output-to-string (*standard-output*)
            (loop for var in vars-c do
                 (when comma
                   (format t ","))
                 (format t var)
                 (setf comma t))
            (loop for var in vars do
                 (format t ",")
                 (format t var))))
    (setf result
          (with-output-to-string (out)
            (run-program
             "/bin/sh"
             (list
              (concatenate 'string *current-directory* "/elim.sh")
              new-basis-str
              (write-to-string (length vars-c))
              all-vars-str) :output out)
            out))
    (setf matches (cl-ppcre:all-matches-as-strings
                   "\\[\\[(\\n|.)*\\]\\]" result))
    (let ((poly-io:*vars* vars))
      (parse-basis-maxima (car matches)))))

(defun get-gfan (polys)
  (let ((gfan-input
         (with-output-to-string (*standard-output*)
           (format t "Q[")
           (vars-print)
           (format t "] {")
           (polys-print polys)
           (format t "}")))
        (gfan-output))
    (with-input-from-string (in gfan-input)
      (setf
       gfan-output
       (with-output-to-string (out)
         (run-program
          "/usr/bin/gfan"
          nil
          :output out
          :input in))))
    (with-input-from-string (*standard-input* gfan-output)
      (parse-gfan-bases))))
  
