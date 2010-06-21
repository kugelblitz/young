(defpackage main
  (:use :common-lisp :algebra :random-grow :young-diagram
        :dim-representation :calculus :external-helpers :enum-diag
        #+cmu :ext
        #+sbcl :sb-ext))

(in-package main)

(defvar *dim* 2)

(defvar *n* 100)

(defvar *n-diagrams* 10000)

(defvar *diagram-func* #'random-diagram)

(defvar *sum*)

(defvar *denom*)

(defvar *lnf*)

(defvar *nfact*)

(defvar *beta* 2)

(defun cb-orderings (inner outer)
  (let* ((nord
          (calc-orderings-in-diagram
           *dim* inner outer))
         (bval (expt (/ nord (sqrt *nfact*)) *beta*))
         (lval (- (log nord) (* *lnf* 0.5))))
    (incf *sum* (* bval lval))
    (incf *denom* bval)))

(defun calc-c (nord)
  (- (/ (- (log nord) (* *lnf* 0.5)) (sqrt *n*))))

(defvar *sum-c*)
(defvar *sum-csq*)
(defvar *diagrams-so-far*)

(defun cb-orderings-rsk (inner outer)
  (let* ((nord
          (calc-orderings-in-diagram
           *dim* inner outer))
         (c (calc-c nord)))
    (incf *sum* nord)
    (incf *denom*)
    (incf *sum-c* c)
    (incf *sum-csq* (* c c))
    (incf *diagrams-so-far*)
    ))

(defvar *h* nil)

(defun main (n nd)
  (setf *n-diagrams* nd)
  (setf *n* n)
  (setf *nfact* (factorial *n*))
  (setf *lnf* (log-n-fact *n*))
  (setf *sum* 0)
  (setf *denom* 0)
  (setf *h* (make-hash-table :test #'equalp))
  (setf *sum-c* 0)
  (setf *sum-csq* 0)
  (setf *diagrams-so-far* 0)
  (young-diagram::init *dim* *n*)
  ;;(random-grow::init *dim* *n*)
  (random-grow::init *dim* *n*)
  (loop for i from 0 below *n-diagrams* do
       ;;(print (random-diagram))
       (let ((inner (random-diagram-rsk)))
         (cb-orderings-rsk inner nil)
         ;(if (gethash inner *h*)
         ;  (incf (gethash inner *h*))
         ;  (setf (gethash inner *h*) 1))
         )
       ;;(young-diagram::add-diagram (funcall *diagram-func*))
       
       (when (zerop (mod i 100))
         (let ((avg-c (/ *sum-c* *diagrams-so-far*))
               (avg-csq (/ *sum-csq* *diagrams-so-far*)))
         (format t "i=~a: cc = ~a, c = ~a, disp = ~a~%"
                 i
                 (- (/ (- (- (log *sum*) (log *denom*)) (* *lnf* 0.5)) (sqrt n)))
                 avg-c (- avg-csq (* avg-c avg-c)))
                 
         ;;(format t "~a skipped~%" random-grow::*nskipped*)
         ;;(print (car young-diagram::*ray-reach*))
         )))

  ;(setf *sum* (/ *sum* (* (sqrt n) *denom*)))
  ;(format t "~a (~a): ~a~%" n nd *sum*)

  
  ;(print (- (/ (- (- (log *sum*) (log *denom*)) (* *lnf* 0.5)) (sqrt n))))
  
  ;(maphash (lambda (k v)
  ;           (format t "~a: ~a~%" k v))
  ;         *h*)
  
  ;(loop for i from 0 below *n-diagrams* do
  ;     (young-diagram::check-diagram (random-diagram *dim* *n*))
  ;     (young-diagram::check-diagram-axes (random-diagram *dim* *n*))
  ;     (when (zerop (mod i 1000))
  ;       (print i))
  ;     )
  ;(print (float (/ young-diagram::*n-diagrams-fit* young-diagram::*n-diagrams*)))
  ;(print (float (/ young-diagram::*n-diagrams-axes-fit* young-diagram::*n-diagrams*)))
  nil
  )

