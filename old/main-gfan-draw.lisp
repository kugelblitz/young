(defpackage main
  (:use :common-lisp :poly-io :cone :draw-poly :algebra :cl-cairo2
        #+cmu :ext
        #+sbcl :sb-ext))

(in-package main)

(defun make-basis-groups (bases)
  (let ((group)
        (head-base (car bases))
        (bases-rest))
    (when head-base
      (dolist (base bases)
        (if (equalp (algebra::basis-normalize-lex-no-monomial-order base)
                    (algebra::basis-normalize-lex-no-monomial-order head-base))
            (push base group)
            (push base bases-rest)))
      (cons group (make-basis-groups bases-rest)))))

(defun point-dist (point1 point2)
  (let ((d1 (- (first point1) (first point2)))
        (d2 (- (second point1) (second point2))))
    (sqrt (+ (* d1 d1) (* d2 d2)))))

(defun points-equal (point1 point2)
  (< (point-dist point1 point2) 0.01))

(defun lsegs-equal (lseg1 lseg2)
  (or (and (points-equal (first lseg1) (first lseg2))
           (points-equal (second lseg1) (second lseg2)))
      (and (points-equal (first lseg1) (second lseg2))
           (points-equal (first lseg2) (second lseg1)))))

(defun separate-group (bases)
  (let ((lsegs)
        (single-lsegs)
        (double-lsegs))
    (dolist (basis bases)
      (setf lsegs (append lsegs (cone::get-basis-lsegs basis))))
    (labels ((xyzzy (lseglist)
               (when lseglist
                 (if (find-if (lambda (lseg)
                                (lsegs-equal lseg (car lseglist)))
                              (cdr lseglist))
                     (push (car lseglist) double-lsegs)
                     (push (car lseglist) single-lsegs))
                 (xyzzy (remove-if (lambda (lseg)
                                     (lsegs-equal lseg (car lseglist)))
                                   (cdr lseglist))))))
      (xyzzy lsegs))
    (list single-lsegs double-lsegs)))

(defun main ()
  (let ((gfan-bases)
        (gfan-groups)
        (all-polys-hash)
        (size 500)
        (npolys 0))
    
    (setf all-polys-hash (make-hash-table :test 'equalp))
    (format t "Input Groebner fan: ")
    (force-output)
    (setf gfan-bases (poly-io::parse-gfan-bases))
    (setf gfan-groups (make-basis-groups gfan-bases))

    (dolist (base gfan-bases)
      (dolist (poly base)
        (incf npolys)
        (setf (gethash (algebra::poly-normalize-lex poly) all-polys-hash) 1)))
    
    (format t "~%~a polynoms ~%" npolys)
    (format t "~a bases ~%" (length gfan-bases))
    (format t "~a unique polynoms~%" (hash-table-count all-polys-hash))
    
    (draw-poly::start size "gg.ps")
    (dolist (group gfan-groups)
      (let ((sep (separate-group group)))
        (cl-cairo2::set-line-width 1)
        (cl-cairo2::set-source-rgb 0 0 0)
        (draw-poly::draw-lsegs (cone::transform-lsegs (first sep) size))
        (cl-cairo2::set-source-rgb 0.2 0.2 1)
        (draw-poly::draw-lsegs (cone::transform-lsegs (second sep) size))))
    (draw-poly::finish)))
    
(main)
(quit)
