(defpackage main
  (:use :common-lisp :external-helpers :algebra :enum-diag :calculus
        :dim-representation :enum-diag
        #+cmu :ext
        #+sbcl :sb-ext))

(in-package main)

(defun for-hooks (width height func)
  (let ((w+h (+ width height)))
    (loop for i from 0 below width
       do
         (loop for j from 0 below height
            do
              (funcall func i j (- w+h i j 1))))))

(defun for-diagram-hooks (inner func)
  (let ((widths (make-array (length inner) :element-type 'fixnum))
        (heights (make-array (length inner) :element-type 'fixnum))
        (hh 0))
    (setf inner (sort inner #'algebra::lex-less))
    (do ((iter inner (cdr iter))
         (i 0 (1+ i))
         (prev-x -1 (caar iter)))
        ((null iter))
      (setf (aref widths i)
            (- (caar iter) prev-x)))
    (do ((iter inner (cdr iter))
         (i 0 (1+ i)))
        ((null iter))
      (setf (aref heights i)
            (- (cadar iter) (if (cdr iter) (cadadr iter) -1))))
    (loop
       for iter-i in inner
       for i from 0 
       do
         (let ((y (- (second iter-i) (aref heights i) -1))
               (x 0)
               (h hh)
               (w (1+ (first iter-i))))
           (loop
              for iter-j in inner
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

(defun for-all-betas (func)
  (loop for beta from 1 to 50
     do
       (funcall func beta)))

(defvar *values*)

(defun main (args)
  (let* ((diagrams nil)
         (nfound 0)
         (dim (parse-integer (first args)))
         (n (parse-integer (second args)))
         (nord 0)
         (betas nil)
         (lnf (log-n-fact n))
         )
    (setf *values* nil)
    (for-all-betas (lambda (beta) (push beta betas ) (push 0 *values*)))
    (setf betas (reverse betas))
    (flet ((cb-diagram (inner outer)
             (let ()
               ;;(print diagram)
               ;;(push diagram diagrams)
               (incf nfound)
               (when (zerop (mod nfound 10000))
                 (print nfound))
               (let ((nord
                      (calc-orderings-in-diagram
                       dim
                       inner
                       outer))
                     (values-iter *values*))
                 (for-all-betas (lambda (beta)
                                  (incf (car values-iter)
                                        (expt nord beta))
                                  (setf values-iter (cdr values-iter))))))))
      (enum-diag::enumerate-diagrams dim n #'cb-diagram))
    (print nfound)
    (setf *values* (mapcar (lambda (beta val)
                             (/ (- (log val)
                                   (* beta lnf 0.5))
                                (sqrt n))) betas *values*))
  nil))

(defun write-values (filename)
  (with-open-file
      (*standard-output* filename :direction :output :if-exists :supersede)
    (let ((iter *values*))
      (for-all-betas
       (lambda (beta)
         (format t "~a ~a~%" beta (car iter)) (setf iter (cdr iter)))))))

(main (get-args))
(write-values (concatenate 'string "energy-" (second (get-args)) ".txt"))
(quit)
