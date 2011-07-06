(defpackage calculus
  (:use :common-lisp)
  (:export log-n-fact factorial sqr random-permutation))

(in-package calculus)

(defun log-n-fact (n)
  (let ((res 0))
    (loop for i from 2 to n do
         (incf res (log i)))
    res))

(defun factorial (n)
  (let ((res 1))
    (loop for i from 2 to n do
         (setf res (* res i)))
    res))

(defmacro sqr (x)
  `(let ((temp ,x))
     (* temp temp)))

(defun nshuffle-vector (vector)
  "Fisher-Yates algorithm; implementation taken from
   Practical Common Lisp by Peter Siebel"
  (loop for idx downfrom (1- (length vector)) to 1
        for other = (random (1+ idx))
        do (unless (= idx other)
             (rotatef (aref vector idx) (aref vector other))))
  vector)

(defun random-permutation (n)
  (let ((arr (make-array n :element-type 'fixnum)))
    (loop for i of-type fixnum from 0 below n
       do
         (setf (aref arr i) i))
    (concatenate 'list (nshuffle-vector arr))))
