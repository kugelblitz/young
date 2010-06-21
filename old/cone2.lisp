(defpackage cone2
  (:use :common-lisp :algebra)
  (:export get-basis-directions get-basis-directions-2))

(in-package cone2)

(defvar *dimension*)

(defvar *num-normales*)

(defun get-line-isect-hyperplanes (normales)
  (let* ((sign 1)
         (*dimension* (length (car normales)))
         (matr (make-array (list (1- *dimension*) (1- *dimension*))
                           :element-type 'fixnum))
         (result))
    (loop for i from 0 below *dimension* do
          (loop for normale in normales
                for w from 0 do
                (let ((k 0))
                  (loop for coord in normale
                        for j from 0 do
                        (unless (= j i)
                          (setf (aref matr w k) coord)
                          (incf k)))))
          (push (* sign (algebra::determinant matr (1- *dimension*)))
                result)
          (setf sign (- sign)))
    (if (algebra::is-zero-point result)
        nil
      (nreverse result))))

(defun point-in-cone (point normales)
  (not (find-if (lambda (normale)
                  (< (algebra::dot-product point normale) 0))
                normales)))

(defun point-in-cone-2 (point normales)
  (let ((prods (mapcar (lambda (x) (dot-product point x)) normales)))
    (print prods)
    (and (not (find-if (lambda (p) (< p 0)) prods))
         (find-if (lambda (p) (> p 0)) prods))))

(defun cone-get-directions (normales)
  (let ((*dimension* (length (car normales)))
        (result))
    (algebra::for-all-subsets
     normales
     (1- *dimension*)
     (lambda (normales-subset)
       (let ((line (algebra::vector-orient-from-origin
                    (get-line-isect-hyperplanes normales-subset))))
         (when line
           (when (point-in-cone line normales)
             (when (not (find line result :test #'equalp))
               
               (push line result)))))))
    result))

(defun cone-exclude-redundant-normales-2 (normales directions)
  (let ((result))
    (dolist (normale normales)
      (when (not (algebra::find-dependent-co normale result))
        (when (find-if (lambda (direction)
                         (= 0 (algebra::dot-product direction normale)))
                       directions)
          (push normale result))))
    result))

(defun cone-get-directions-2 (normales)
  (let* ((dirs (cone-get-directions normales))
         (normales-new (cone-exclude-redundant-normales-2 normales dirs))
         (result))
    (print normales)
    (print dirs)
    (print normales-new)
    (dolist (dir dirs)
      (when (point-in-cone-2 dir normales-new)
        (push dir result)))
    result))

(defun cone-exclude-redundant-normales (normales directions)
  (let ((result))
    (dolist (normale normales)
      (when (not (algebra::find-dependent normale result))
        (when (find-if (lambda (direction)
                         (= 0 (algebra::dot-product direction normale)))
                       directions)
          (push normale result))))
    result))


(defun get-basis-directions (basis)
  (let ((algebra::*dimension* (1- (length (caar basis)))))
    (cone-get-directions (append
                          (algebra::make-unit-vectors)
                          (cone::get-basis-walls basis)))))

(defun get-basis-directions-2 (basis)
  (let ((algebra::*dimension* (1- (length (caar basis)))))
    (cone-get-directions-2 (append
                            (algebra::make-unit-vectors)
                            (cone::get-basis-walls basis)))))
