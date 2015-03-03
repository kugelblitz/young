(defpackage diagram-hooks
  (:use :common-lisp :algebra)
  (:export for-diagram-hooks))

(in-package diagram-hooks)

(defun for-hooks (width height func)
  (let ((w+h (+ width height)))
    (loop for i from 0 below width
       do
         (loop for j from 0 below height
            do
              (funcall func i j (- w+h i j 1))))))

(defun for-diagram-hooks (corners func)
  (let ((widths (make-array (length corners) :element-type 'fixnum))
        (heights (make-array (length corners) :element-type 'fixnum))
        (hh 0))
    (setf corners (sort (copy-list corners) #'lex-less))
    (do ((iter corners (cdr iter))
         (i 0 (1+ i))
         (prev-x -1 (caar iter)))
        ((null iter))
      (setf (aref widths i)
            (- (caar iter) prev-x)))
    (do ((iter corners (cdr iter))
         (i 0 (1+ i)))
        ((null iter))
      (setf (aref heights i)
            (- (cadar iter) (if (cdr iter) (cadadr iter) -1))))
    (loop
       for iter-i in corners
       for i from 0 
       do
         (let ((y (- (second iter-i) (aref heights i) -1))
               (x 0)
               (h hh)
               (w (1+ (first iter-i))))
           (loop
              for iter-j in corners
              for j from 0
              do
                (decf w (aref widths j))
                (for-hooks (aref widths j)
                           (aref heights i)
                           (lambda (i j hook)
                                 (funcall func (+ i x) (+ j y) (+ hook w h))))
                (decf h (aref heights j))
                (incf x (aref widths j))
                (when (equalp iter-j iter-i) (return))))
         (incf hh (aref heights i)))
    ))

(defun get-hook-length (pt corners)
  (let ((maxrays (make-list (length pt) :initial-element 0)))
    (dolist (corner corners)
      (when (monom-divides pt corner)
        (let ((pt-iter pt)
              (corner-iter corner)
              (maxrays-iter maxrays))
          (loop
             while pt-iter do
               (let ((diff (- (car corner-iter) (car pt-iter))))
                 (when (> diff (car maxrays-iter))
                   (setf (car maxrays-iter) diff)))
               (setf pt-iter (cdr pt-iter))
               (setf corner-iter (cdr corner-iter))
               (setf maxrays-iter (cdr maxrays-iter)))
          )))
    (1+ (apply #'+ maxrays))))
