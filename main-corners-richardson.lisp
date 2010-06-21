(defpackage main
  (:use :common-lisp :dimension :algebra :calculus :young
        #+cmu :ext
        #+sbcl :sb-ext))

(in-package main)

(defvar *n* 100)

(defvar *n-diagrams* 10000)

(defvar *corners-file-name*)

(defvar *corner-counters*)

(defun on-diagram (func)
  (incf (aref *corner-counters* (funcall func :size))
        (length (the list (funcall func :corners))))
  )

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

  (format t "Input file name to write corners statistics: ")
  (force-output)
  (setf *corners-file-name* (read-line))

  (setf *corner-counters* (make-array (1+ *n*) :initial-element 0))
  (setf *on-diagram* #'on-diagram)
  
  (loop for i from 1 to *n-diagrams* do
       (random-diagram-richardson *n*)
       (when (zerop (mod i 100))
         (format t "done ~a diagrams so far~%" i)))

  (with-open-file
      (*standard-output* *corners-file-name* :direction :output :if-exists :supersede)
    (loop for i from 1 to *n* do
         (format t "~a ~a~%" i
                 (float (/ (aref *corner-counters* i)
                           *n-diagrams*)))
    ))
  )

;(main)
;(quit)