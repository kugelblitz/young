(defpackage combinatorics
  (:use :common-lisp)
  (:export
   for-all-subsets
   for-all-subsets-of-size
   for-all-max-subsets
   for-all-cuts
   for-all-pairs
   for-all-partitions
   n-points-under
   for-all-representatives))

(in-package combinatorics)

(defun for-all-subsets (list func)
  (declare (list list) (function func))
  (unless list
    (funcall func nil))
  (when list
    (for-all-subsets
     (cdr list) 
     (lambda (s) (funcall func (cons (car list) s))))
    (for-all-subsets (cdr list) func)))

(defun for-all-subsets-of-size (list-set size func)
  (declare (fixnum size) (list list-set) (function func))
  (if (zerop size)
      (funcall func nil)
      (when list-set
        (for-all-subsets-of-size
         (cdr list-set) (1- size)
         (lambda (s) (funcall func (cons (car list-set) s))))
        (when (< size (length list-set))
          (for-all-subsets-of-size (cdr list-set) size func)))))

(defun for-all-max-subsets (set func)
  (if set
      (let ((saved))
        (for-all-max-subsets
         (cdr set)
         (lambda (s)
           (when (funcall func (cons (car set) s))
             (push s saved))))
        (for-all-max-subsets
         (cdr set)
         (lambda (s)
           (or (position s saved :test #'equalp)
               (funcall func s)))))
      (progn
        (funcall func nil)
        nil)))

(defun for-all-cuts (list func)
  (declare (list list) (function func))
  (when list
    (funcall func (car list) (cdr list)))
  (when (cdr list)
    (for-all-cuts (cdr list)
		  (lambda (elem cut)
		    (funcall func elem (cons (car list) cut))))))

(defun for-all-pairs (set func)
  (dolist (elem (cdr set))
    (funcall func (car set) elem))
  (when (cdr set)
    (for-all-pairs (cdr set) func)))

(defun for-all-partitions (n func &optional min)
  (unless min
    (setf min 1))
  (cond
    ((zerop n)
     (funcall func nil))
    ((>= n min)
     (loop for i from min to n
        do
          (for-all-partitions
           (- n i)
           (lambda (ptn) (funcall func (cons i ptn)))
           i)))))

(defun for-all-points-under (corners func
                             &optional (dimension (length (car corners))) fixed)
  (when (zerop dimension)
    (apply func (list (reverse fixed)))
    (return-from for-all-points-under t))
  (let ((lim))
    (dolist (corner corners)
      (when (every #'zerop (cdr corner))
        (when (or (null lim) (> lim (car corner)))
          (setf lim (car corner)))))
    (if lim
        (let ((sorted-corners (sort (copy-list corners)
                                    (lambda (a b) (< (car a) (car b)))))
              (new-corners))
          (loop for i from 0 below lim do
               (loop while (and sorted-corners (<= (caar sorted-corners) i)) do
                    (push (cdar sorted-corners) new-corners)
                    (setf sorted-corners (cdr sorted-corners)))
               (unless (for-all-points-under
                        new-corners func (1- dimension) (cons i fixed))
                 (return-from for-all-points-under nil)))
          t)
        nil)))

(defun n-points-under (corners)
  (let ((res 0))
    (when (for-all-points-under corners
                                (lambda (point)
                                  (declare (ignore point)) (incf res)))
      res)))

(defun for-all-representatives (lists func)
  (unless (car lists)
    (apply func (list nil)))
  (when (car lists)
    (dolist (elem (car lists))
      (for-all-representatives
       (cdr lists)
       (lambda (x)
         (apply func (list (cons elem x))))))))
