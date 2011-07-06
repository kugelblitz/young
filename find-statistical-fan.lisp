(defpackage find-statistical-fan
  (:use :common-lisp :combinatorics :dimension :algebra :geometry :young :gauss)
  (:export
   find-fan
   *on-sequence*
   *use-symmetry*
   *use-coordinate-repetitions*))

(in-package find-statistical-fan)

(defvar *design*)
(defvar *evaluated-monomials*)
(defvar *on-sequence*)
(defvar *boundary-dimples*)
(defvar *use-symmetry* nil)
(defvar *use-coordinate-repetitions* t)

(defvar *symmetries*)
(defvar *permutations*)
(defvar *handled-diagrams*)

(defun numbers-from-zero (n)
  (let ((res))
    (loop for i from 0 below n
       do
         (push i res))
    (nreverse res)))

(defun permute-points (points pair)
  (let ((points-perm)
        (point-perm))
    (loop
       for point in points
       do
         (setf point-perm (copy-list point))
         (rotatef (nth (first pair) point-perm)
                  (nth (second pair) point-perm))
         (push point-perm points-perm))
    points-perm))

(defun find-symmetries (design)
  (let ((indices (numbers-from-zero (length (car design))))
        (res))
    (for-all-distinct-pairs
     indices
     (lambda (pair)
       (let ((design-perm (permute-points design pair)))
         (when (equalp (sort (copy-list design) #'lex-less-gen)
                       (sort (copy-list design-perm) #'lex-less-gen))
           (push pair res)))))
    res))

(defun merge-symmetries-sub (lists)
  (when lists
    (let* ((head (car lists))
           (tail))
      (dolist (list (cdr lists))
        (let ((joined (remove-duplicates
                       (concatenate 'list head list) :test #'equalp)))
          (if
           ;; list and head have common elements?
           (< (length joined) (+ (length list) (length head)))
           (setf head joined)
           ;; no?
           (push list tail))))
      (cons head
             (merge-symmetries-sub tail)))))

(defun merge-symmetries (lists)
  (let ((res (merge-symmetries-sub lists)))
    (if (= (length lists) (length res))
        res
        (merge-symmetries res))))

(defun evaluate-monomial-in-point (monom pt)
  (reduce #'* (mapcar #'expt pt monom)))

(defun evaluate-monomial (monom)
  (mapcan (lambda (x)
            (let ((value (evaluate-monomial-in-point monom x)))
              (unless (zerop value)
                (list (cons value (copy-list x))))))  *design*))

(defun evaluate-monomial-use-cache (monom)
  (or (gethash monom *evaluated-monomials*)
      (let ((evaluated (evaluate-monomial monom)))
        (setf (gethash monom *evaluated-monomials*) evaluated)
        evaluated)))

(defun apply-perm (perms point)
  (let ((point-copy (copy-list point)))
    (dolist (perm perms)
      (let ((perm-sorted (sort (copy-list perm) #'<)))
        (loop
           for i in perm-sorted
           for j in perm
             do
             (setf (nth i point-copy)
                   (nth j point)))))
    point-copy))

(defun yield-sequence (seq polys)
  (when (equalp (length seq) (length *design*))
    (when *on-sequence*
      (if *use-symmetry*
          (let ((all-seqs (make-hash-table :test #'equalp)))
            (for-all-permutations-lists
             *permutations*
             (lambda (perms)
               (let ((newseq
                      (sort
                       (mapcar (lambda (elem) (apply-perm perms elem)) seq)
                       #'lex-less)))
                 (unless (gethash newseq all-seqs)
                   (funcall *on-sequence* (reverse newseq) polys)
                   (setf (gethash newseq all-seqs) 1))))))
          (funcall *on-sequence* (reverse seq) polys)))))

(defun lex-lex-more (seq1 seq2)
  (if seq1
      (if seq2
          (if (lex-more (car seq1) (car seq2))
              t
              (if (equalp (car seq1) (car seq2))
                  (lex-lex-more (cdr seq1) (cdr seq2))
                  nil))
          t)
      nil))

(defun find-fan-sub (seq dimples corners polys)
  (let ((sorted-corners (sort (copy-list corners)
                              #'lex-more)))
    ;; Reverse search: check if we will (or had) come to this diagram by another sequence
    (unless *use-symmetry*
      (unless (equalp (car seq) (car sorted-corners))
        (return-from find-fan-sub nil)))

    (when *use-symmetry*
      ;; Check if we had come to this diagram by another sequence
      (when (gethash sorted-corners *handled-diagrams*)
        (return-from find-fan-sub nil))
    
      ;; Check if we will (or had) come to a diagram symmetric to this one
      (dolist (symmetry *symmetries*)
        (let ((sorted-corners-perm
               (sort (permute-points corners symmetry) #'lex-more)))
          (when (lex-lex-more sorted-corners-perm sorted-corners)
            (return-from find-fan-sub nil)))))
    (setf (gethash sorted-corners *handled-diagrams*) 1))
  
  (let ((*boundary-dimples* *boundary-dimples*)
        (remaining-dimples)
        (remaining-dimples-sorted))
    (dolist (dimple dimples)
      (let ((gauss:*matrix* gauss:*matrix*)
            (gauss:*sqmatrix* gauss:*sqmatrix*)
            (gauss:*hmatrix* gauss:*hmatrix*)
            (gauss:*n-polys* gauss:*n-polys*)
            (gauss:*n-monoms* gauss:*n-monoms*)
            (gauss:*hash* gauss:*hash*))
        (let ((coef (gauss:add-polynom (evaluate-monomial-use-cache dimple))))
          (if coef
            (progn
              (let ((seq (copy-list seq))
                    (poly))
                (push dimple seq)
                (setf seq (reverse seq))
                (loop
                   for c in coef
                   for monom in seq
                   do
                     (unless (zerop c)
                       (push (cons c monom) poly)))
                (push poly polys))
              (push dimple *boundary-dimples*))
            (push dimple remaining-dimples)))))
    (setf remaining-dimples-sorted (sort (copy-list remaining-dimples) #'lex-more))
    (if remaining-dimples
        (for-all-cuts
         remaining-dimples-sorted
         (lambda (dimple other-dimples)
           (let ((gauss:*matrix* gauss:*matrix*)
                 (gauss:*sqmatrix* gauss:*sqmatrix*)
                 (gauss:*hmatrix* gauss:*hmatrix*)
                 (gauss:*n-polys* gauss:*n-polys*)
                 (gauss:*n-monoms* gauss:*n-monoms*)
                 (gauss:*hash* gauss:*hash*))
             (when (gauss:add-polynom (evaluate-monomial-use-cache dimple))
               (error "internal error"))
             (find-fan-sub
              (cons dimple seq)
              (update-dimples other-dimples dimple)
              (add-corner corners dimple)
              polys))))
        (yield-sequence seq polys))))

(defun remove-nth (list n)
  (remove-if (constantly t) list :start n :end (1+ n)))

(defun rep-counter (seq)
  (let ((seq-sorted (sort seq #'<))
)
    max-rep))

(defun rep-counters (design)
  (let* ((dimension (length (car design)))
         (counters (make-array dimension :initial-element 0)))
    (loop for i from 0 below dimension
       do
         (let ((design-projection
                (mapcar
                 (lambda (x) (remove-nth (copy-list x) i))
                 design))
               (prev nil)
               (max-rep 0)
               (cur-rep 0))
           (setf design-projection (sort design-projection #'lex-less-gen))
           (dolist (point design-projection)
             (if (equalp point prev)
                 (incf cur-rep)
                 (setf cur-rep 1))
             (setf prev point)
             (setf max-rep (max max-rep cur-rep)))
           (setf (aref counters i) max-rep)))
    counters))

(defun find-fan (design)
  (let ((*design* design)
        (*dimension* (length (car design)))
        (*symmetries* (find-symmetries design))
        (*permutations*)
        (*handled-diagrams* (make-hash-table :test #'equalp))
        (*evaluated-monomials* (make-hash-table :test #'equalp))
        (*boundary-dimples*)
        (gauss:*hash* (make-hash-table :test #'equalp))
        (gauss:*matrix* (make-array '(0 0)))
        (gauss:*sqmatrix* (make-array '(0 0)))
        (gauss:*hmatrix* (make-array 0))
        (gauss:*n-polys* 0)
        (gauss:*n-monoms* 0))
    (setf *permutations* (merge-symmetries *symmetries*))
    (let ((seq nil)
          (dimples (list (zero-vector)))
          (corners nil))
      (when *use-coordinate-repetitions*
        (setf seq (list (zero-vector)))
        (setf corners (list (zero-vector)))
        (setf dimples (update-dimples nil (car seq)))
        (gauss:add-polynom (evaluate-monomial-use-cache (zero-vector)))
        (let ((rep-counters (rep-counters design)))
          (loop for i from 0 below *dimension*
             do
               (loop for j from 1 below (aref rep-counters i)
                  do
                    (let ((newpt (zero-vector)))
                      (setf (nth i newpt) j)
                      (push newpt seq)
                      (setf corners (add-corner corners newpt))
                      (setf dimples (add-dimple dimples newpt))
                      (when (gauss:add-polynom (evaluate-monomial-use-cache newpt))
                        (error "unexpected: rep-counters")))))))
      (find-fan-sub seq dimples corners nil))))
