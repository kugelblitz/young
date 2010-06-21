(defpackage diagram-shape
  (:use :common-lisp :algebra :calculus)
  (:export
   init
   add-diagram
   complete
   *deviation*
   write-average-shape))

(in-package diagram-shape)

(defun shoot-ray (origin corners)
  (max 0
       (1+ (apply
            #'max
            (mapcar
             (lambda (corner)
               (apply #'min (mapcar #'- corner origin)))
             corners)))))

(defun bounding-box (corners)
  (apply #'mapcar (cons #'max corners)))

(defun for-box (box func)
  (declare (function func))
  (if box
      (loop for i of-type fixnum from 0 to (car box) do
           (for-box (cdr box)
                    (lambda (point)
                      (apply func (list (cons i point))))))
      (apply func (list nil))))

(defun for-pocket (box func)
  (declare (function func))
  (when box
    (for-box (cdr box) (lambda (point) (apply func (list (cons 0 point)))))
    (for-pocket (cdr box)
                (lambda (point)
                  (loop for i of-type fixnum from 1 to (car box) do
                       (apply func (list (cons i point))))))))

(defvar *ray-reach*)
(defvar *average-shape*)
(defvar *deviation*)
(defvar *n-diagrams*)

(defun init ()
  (setf *n-diagrams* 0)
  (setf *ray-reach* (make-hash-table :test #'equalp)))

(defun inc-reach (origin val)
  (declare (fixnum val))
  (let* ((h (gethash origin *ray-reach*))
         (diff 0))
    (declare (fixnum diff))
    (unless h
      (setf (gethash origin *ray-reach*)
            (list val (make-array 1 :initial-element 1)))
      (return-from inc-reach))
    (let ((arr (cadr h)))
      (declare (type simple-vector arr))
      (setf diff (the fixnum (- val (the fixnum (car h)))))
      (when (>= diff 0)
        (when (>= diff (length arr))
          (setf arr (adjust-array arr (1+ diff) ))
          (setf (cadr h) arr))
        (incf (aref arr diff))
        (return-from inc-reach))
      (setf arr (concatenate 'simple-vector
                             (make-array (- diff) ) arr))
      (incf (aref arr 0))
      (setf (cadr h) arr)
      (setf (car h) val))))

(defun add-diagram (corners)
  (incf *n-diagrams*)
  (for-pocket
   (bounding-box corners)
   (lambda (origin)
     (inc-reach origin
                (shoot-ray origin corners)))))

(defun make-average-shape ()
  (setf *average-shape* nil)
  (setf *deviation* nil)
  
  (maphash
   (lambda (origin val)
     (let ((start (car val))
           (arr (cadr val))
           (sum 0)
           (total 0)
           (deviation 0)
           (avg-length))
       (loop
          for i from 0 below (length arr) do
            (incf sum (* i (aref arr i)))
            (incf total (aref arr i)))

       (setf avg-length (/ sum total))

       (loop
          for i from 0 below (length arr) do
            (incf deviation (* (aref arr i) (sqr (- avg-length i)))))

       (setf deviation (sqrt (/ deviation total)))
       (incf avg-length start)
         
       (push (mapcar (lambda (x)
                       (float (+ x avg-length)))
                     origin)
             *average-shape*)

       (push deviation *deviation*)))
   *ray-reach*)
  (setf *deviation* (nreverse *deviation*))
  (setf *average-shape* (nreverse *average-shape*)))

(defun write-average-shape (filename &optional (func #'identity))
  (with-open-file
      (*standard-output* filename :direction :output :if-exists :supersede)
    (dolist (point *average-shape*)
      (dolist (val point)
        (format t "~a~A" (funcall func val) #\,))
      (format t "~%"))))

(defun complete ()
  (make-average-shape))
