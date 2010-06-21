(defpackage main
  (:use :common-lisp :calculus :dimension :algebra :young :diagram-shape
        #+cmu :ext
        #+sbcl :sb-ext))

(in-package main)

(defvar *n* 100)

(defvar *n-diagrams*)

(defvar *shape-file-name*)

(defun main ()

  (format t "Input dimension: ")
  (force-output)
  (setf *dimension* (parse-integer (read-line)))

  (format t "Input diagram size: ")
  (force-output)
  (setf *n* (parse-integer (read-line)))

  (format t "Input number of diagrams to generate: ")
  (force-output)
  (setf *n-diagrams* (parse-integer (read-line)))

  (format t "Input file name to write average shape: ")
  (force-output)
  (setf *shape-file-name* (read-line))

  (diagram-shape:init)

  (loop for i from 1 to *n-diagrams* do
       (diagram-shape:add-diagram (random-diagram-richardson *n*))
       (when (zerop (mod i 100))
         (format t "done ~a diagrams so far~%" i)))

  (diagram-shape:complete)
  (diagram-shape:write-average-shape *shape-file-name*)

  (format t "n=~a (~a diagrams), center-deviation: ~a, center-dev-norm: ~a~%"
          *n* *n-diagrams* (car diagram-shape:*deviation*)
          (/ (car diagram-shape:*deviation*) (sqrt *n*)))
  )

;(main)
;(quit)
