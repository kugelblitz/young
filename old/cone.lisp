(defpackage cone
  (:use :common-lisp :algebra))

(in-package cone)

(defun get-poly-walls (poly)
  (let ((lm (cdar poly)))
    (mapcar (lambda (term) (algebra::monom-divide lm (cdr term)))
            (cdr poly))))

(defun eliminate-dependent (normales)
  (let ((independent))
    (labels ((eliminate-sub (rest)
             (let ((normale (car rest)))
               (when normale
                 (unless (find-if (lambda (norm-i)
                                    (algebra::dependent-co norm-i normale))
                                  independent)
                   (push normale independent))
                 (eliminate-sub (cdr rest))))))
      (eliminate-sub normales))
    independent))

(defun get-basis-walls (basis)
  (let ((result))
    (dolist (poly basis)
      (let ((walls (get-poly-walls poly)))
        (dolist (normale walls)
          (push normale result))))
    (eliminate-dependent result)))

(defun project-line-to-triangle (normale)
  (let ((a (first normale))
        (b (second normale))
        (c (third normale)))
    (list (/ (- a b) (sqrt 2))
          (/ (- (+ a b) (* 2 c)) (sqrt 6))
          (/ (+ a b) -2))))

(defun isect-line-line (line1 line2)
  (let* ((a1 (first line1))
         (b1 (second line1))
         (c1 (third line1))
         (a2 (first line2))
         (b2 (second line2))
         (c2 (third line2))
         (denom (- (* a1 b2) (* a2 b1))))
    (if (< (abs denom) 1e-5)
        nil
        (list (/ (- (* b1 c2) (* c1 b2)) denom)
              (/ (- (* c1 a2) (* a1 c2)) denom)))))

(defun line-by-two-points (point1 point2)
  (let ((x1 (first point1))
        (x2 (first point2))
        (y1 (second point1))
        (y2 (second point2)))
    (list (- y2 y1)
          (- x1 x2)
          (+ (* x1 (- y1 y2))
             (* y1 (- x2 x1))))))

(defun get-angle-x-ccw (normale)
  (let* ((a (first normale))
         (b (second normale)))
    (atan b a)))

(defun sort-lines-by-angle (lines)
  (sort lines (lambda (line1 line2)
                (> (get-angle-x-ccw line1)
                   (get-angle-x-ccw line2)))))

(defun isect-lines-loop (lines)
  (let ((result))
    (when (and (cdr lines) (cddr lines)) ; 3 lines minimum
      (do ((line lines (cdr line)))
          ((null line))
        (let* ((cur (car line))
               (next (if (cdr line) (cadr line) (car lines)))
               (isect (isect-line-line cur next)))
          (unless isect
            (error "no intersection"))
          (push isect result))))
    result))

(defun point-above-line (point line)
  (> (+ (* (first point) (first line))
        (* (second point) (second line))
        (third line))
     0))

(defun isect-lseg-line (lseg line)
  (isect-line-line (line-by-two-points (first lseg) (second lseg))
                   line))

(defun isect-polygon-halfplane (polygon line)
  (let ((cur polygon)
        (result))
    (loop named looop do
         (let* ((next (cdr cur))
                (cur-above) (next-above))
           (unless next
             (setf next polygon))
           (setf cur-above (point-above-line (car cur) line))
           (setf next-above (point-above-line (car next) line))
           (when cur-above
             (push (car cur) result)
             (unless next-above
               (let ((new-pt (isect-lseg-line (list (car cur) (car next)) line)))
                 (when new-pt (push new-pt result)))))
           (unless cur-above
             (when next-above
               (let ((new-pt (isect-lseg-line (list (car cur) (car next)) line)))
                 (when new-pt (push new-pt result)))))
           (setf cur next)
           (when (eq cur polygon)
             (return-from looop))))
    (reverse result)))

(defconstant triangle (list (list (sqrt 1/2) 0)
                            (list (- (sqrt 1/2)) 0)
                            (list 0 (sqrt 3/2))))

(defun get-basis-polygon (basis)
  (let ((result triangle)
        (walls (get-basis-walls basis)))
    (dolist (wall walls)
      (setf result
            (isect-polygon-halfplane result (project-line-to-triangle wall))))
    result))

(defun get-polygon-lsegs (polygon)
  (let ((cur polygon)
        (result))
    (loop named looop do
         (when (cdr cur)
           (push (list (car cur) (cadr cur)) result))
         (unless (cdr cur)
           (push (list (car cur) (car polygon)) result)
           (return-from looop result))
         (setf cur (cdr cur)))))

(defun get-basis-lsegs (basis)
  (get-polygon-lsegs (get-basis-polygon basis)))

(defun transform-point (point size)
  (let ((coef (* size (sqrt 2/3))))
    (setf coef (* coef 0.8))
    (let ((x (first point))
          (y (second point)))
      (list (+ (* x coef) (/ size 2))
            (- size (* y coef))))))
  
(defun transform-polygon (polygon size)
  (mapcar (lambda (point)
            (transform-point point size))
          polygon))

(defun transform-lseg (lseg size)
  (list (transform-point (first lseg) size)
        (transform-point (second lseg) size)))

(defun transform-lsegs (lsegs size)
  (mapcar (lambda (lseg) (transform-lseg lseg size)) lsegs))