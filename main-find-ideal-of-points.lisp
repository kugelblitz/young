(defpackage main
  (:use :common-lisp :dimension
        :algebra :poly-io :external-helpers :find-statistical-fan :experimental-design
        :young :combinatorics :gauss
        #+cmu :ext
        #+sbcl :sb-ext))

(in-package main)
(defvar *count*)
(defvar *gfan*)
(defvar *gfan-under*)
(defvar *pairs*)
(defvar *matr-d*)
(defvar *expected-deps*)
(defvar *deps-matched-gfan*)
(defvar *deps-matched-eugb*)

(defun leading-terms (polys)
  (sort (mapcar #'cdar polys) #'lex-less))

(defun monoms-under (corners)
  (let ((result))
    (for-all-points-under corners
                          (lambda (monom)
                            (setf result (cons monom result))))
    (sort result #'lex-less)))

(defun num-rows (matrix)
  "Return the number of rows of a matrix"
  (array-dimension matrix 0))

(defun num-cols (matrix)
  "Return the number of rows of a matrix"
  (array-dimension matrix 1))

(defun copy-matrix (matrix)
  "Return a copy of the matrix."
  (let* ((rows (num-rows matrix))
         (cols (num-cols matrix))
         (copy (make-array (list rows cols))))
    (dotimes (row rows copy)
      (dotimes (col cols)
        (setf (aref copy row col) (aref matrix row col))))))

(defun multiply-two (m1 m2)
  (let* ((rows1 (num-rows m1))
         (cols1 (num-cols m1))
         (cols2 (num-cols m2))
         (result (make-array (list rows1 cols2) :initial-element 0)))
    (dotimes (row rows1 result)
      (dotimes (col cols2)
        (dotimes (i cols1)
          (setf (aref result row col)
                (+ (aref result row col)
                   (* (aref m1 row i)
                      (aref m2 i col)))))))))

(defun invert-matrix (matrix)
  (let* ((result (copy-matrix matrix))
         (size (num-rows matrix))
         (result-r (make-array (list size size) :initial-element 0))
         (temp 0)
         (nonzero-idx 0)
         )

    (dotimes (i size)
      (setf (aref result-r i i) 1))
    
    (dotimes (i size)
      (do ((idx i (1+ idx)))
          ((progn
             (setf nonzero-idx idx)
             (or (= idx size) (not (zerop (aref result idx i)))))))
      ; swap columns
      (unless (= i nonzero-idx)
        (dotimes (j size)
          (rotatef (aref result i j) (aref result nonzero-idx j))
          (rotatef (aref result-r i j) (aref result-r nonzero-idx j))))
      (setf temp (aref result i i))
      (dotimes (j size)
        (setf (aref result i j) (/ (aref result i j) temp))
        (setf (aref result-r i j) (/ (aref result-r i j) temp)))
      (dotimes (j size)
        (unless (= j i)
          (setf temp (aref result j i))
          (dotimes (k size)
            (setf (aref result j k)
                  (- (aref result j k)
                     (* temp (aref result i k))))
            (setf (aref result-r j k)
                  (- (aref result-r j k)
                     (* temp (aref result-r i k))))
            )))
      )
    
    result-r))

(defun extract-dependencies (seq coefs)
  (let ((deps (make-array (length *vars*) :initial-element 0)))
    (loop
       for monom in seq
       for coef in coefs
       do
         (unless (zerop coef)
           (loop
              for var in monom
              for var-idx from 0
              do
                (unless (zerop var)
                  (setf (aref deps var-idx) 1)))))
    (loop
       for flag in (coerce deps 'list)
       for var-idx from 0
       unless (zerop flag) collect var-idx)))

(defun analyze-basis (seq)
  (format t "(")
  (dolist (monom seq)
    (unless (equalp monom (car seq))
      (format t ", "))
    (term-print (cons 1 monom)))
  (format t ")~%")
  (let* ((size (length seq))
         (matr (make-array (list size size) :initial-element 0))
         (deps-match t)
         )
    (loop
       for pair in *pairs*
       for i from 0
       do
         (loop
            for term in seq
            for j from 0
            do
              (let ((val (reduce #'* (mapcar #'expt (car pair) term))))
                (setf (aref matr i j) val))))
    (dotimes (idx (length *vars*))
      (let* ((inverted-matr (invert-matrix matr))
             (right-side
              (loop
                 for pair in *pairs*
                 collect (nth idx (second pair))))
             (coefs
              (loop
                 for i from 0 below size
                 collect
                   (mod
                    (loop
                       for rs in right-side
                       for j from 0
                       sum
                         (* (aref inverted-matr i j) rs))
                    2))))
        (format t "~a = " (nth idx *vars*))
        (loop
           for coef in coefs
           for monom in seq
           do
             (unless (zerop coef)
               (when (> coef 0)
                 (format t "+"))
               (when (= coef -1)
                 (format t "-"))
               (unless (or (= coef 1) (= coef -1))
                 (format t "~a" coef))
               (term-print (cons 1 monom))
               (format t " ")))
        (let ((deps (extract-dependencies seq coefs)))
          ;(format t "deps: ~a~%" deps)
          (unless (equalp deps (aref *expected-deps* idx))
            (setf deps-match nil)
            )
          ;(loop
          ;   for var-idx in deps
          ;   do
          ;     (incf (aref *matr-d* idx var-idx)
          ;           (/ 1 (length deps))))
          )
        (format t "~%")
        ;(print coefs)
        ))
    (when deps-match
      (format t "DEPS MATCHED~%"))
    deps-match
    )  )

(defun on-sequence (seq polys)
  (when (zerop *count*)
    (loop
       for i from 0 below (length *vars*) do
         (let ((vec1 (make-array (length *vars*) :initial-element 0))
               (vec2 (make-array (length *vars*) :initial-element 0)))
           (setf (aref vec1 i) 1)
           (setf (aref vec2 i) 2)
           (setf polys (cons (list (cons 1 (coerce vec1 'list))
                                   (cons -1 (coerce vec2 'list)))
                             polys))))
    (format t "~%Groebner fan consists of...")
    (let ((*vars* *vars*))
      (print polys)
      (setf *gfan* (get-gfan polys "Z/2Z"))
      (setf *gfan-under* nil))
    (format t " ~a bases~%" (length *gfan*))
    (dolist (polys *gfan*)
      (when
          (analyze-basis (monoms-under (mapcar #'cdar polys)))
        (setf *deps-matched-gfan* t))
      (push (monoms-under (mapcar #'cdar polys)) *gfan-under*))
    (format t "~%"))
  (incf *count*)
  ;(return-from on-sequence)
  (setf seq (reverse seq))
  (when
      (analyze-basis seq)
    (setf *deps-matched-eugb* t))
  (format t "~%")
  (unless (find (sort (copy-list seq) #'lex-less) *gfan-under* :test #'equalp)
    (format t "  (not in algebraic fan)~%"))
  )

(defvar *transitions*)

(defun extract-pairs (transitions)
  (reduce #'append
          (mapcar
           (lambda (transition)
             (do ((iter transition (cdr iter))
                  (result nil))
                 ((null (cdr iter)) result)
               (setf result (cons (list (first iter) (second iter)) result))
              ))
           transitions)))

(defun extract-points (pairs)
  (remove-duplicates (mapcar #'first pairs)
                     :test #'equalp))

;; (setf *transitions*
;;       '(((0 18 0 0 10 7 4)
;;          (0 10 1 0 4 4 1)
;;          (10 60 1 1 2 10 10)
;;          (23 30 14 26 12 18 8)
;;          )))

;; (setf *transitions*
;;       '(((0 0 0 0 1 0)
;;          (0 1 0 0 0 0)
;;          (1 1 0 0 0 1)
;;          (1 1 1 1 1 1)
;;          (1 0 1 1 1 1)
;;          (0 1 1 0 1 1)
;;          (0 0 1 0 0 0))))

 ;; (setf *transitions*
 ;;       '(((0 1 0 0 0) (0 0 0 0 0) (0 0 0 0 0))
 ;;         ((1 1 0 0 0) (0 1 0 0 1) (0 0 0 1 0) (0 0 0 1 0))
 ;;         ((1 0 1 1 0) (1 1 1 1 1))
 ;;         ((0 1 0 1 0) (0 0 1 0 0))
        
 ;;         ;((1 0 0 1 0) (0 1 0 1 1) (0 0 1 1 0) (1 0 1 1 0) (1 1 1 1 1) (1 1 1 1 1))
 ;;         ;((0 1 1 0 0) (1 0 1 0 0) (1 1 1 0 1) (1 1 1 1 1))
 ;;         )
 ;;       )

 ;; (setf *transitions*
 ;;       '(((0 1 0 0 0) (0 0 0 0 0); (0 0 0 0 0)
 ;;          )
 ;;         ((1 1 0 0 0) (0 1 0 0 1) (0 0 0 1 0) (0 0 0 1 0)
 ;;          )
 ;;         ;((1 0 1 1 0) (1 1 1 1 1))
 ;;         ((0 1 0 1 0); (0 0 1 0 0)
 ;;          )
        
 ;;         ((1 0 0 1 0) (0 1 0 1 1) (0 0 1 1 0) (1 0 1 1 0) (1 1 1 1 1) (1 1 1 1 1))
 ;;         ((0 1 1 0 0) (1 0 1 0 0); (1 1 1 0 1) (1 1 1 1 1)
 ;;          )
 ;;         )
 ;;       )

;; (setf *transitions*
;;       '(((1 1 1 1 1)
;;          (1 1 0 1 1)
;;          (0 1 1 1 1)
;;          (1 0 0 1 0)
;;          ;(0 1 0 1 1)
;;          ;(1 0 1 1 0)
;;          ;(1 1 1 1 1)
;;          )
;;         ((0 0 1 0 0)
;;          (1 0 1 0 0)
;;          (1 1 1 0 1))))

(defun reference-func (point)
  (let ((m (nth 0 point))
        (b (nth 1 point))
        (a (nth 2 point))
        (l (nth 3 point))
        (p (nth 4 point)))
    (mapcar (lambda (x) (mod x 2))
            (list a m (+ a (* b l))
                  (+ l p (* b l))
                  m))))

(setf *transitions*
      (mapcar
       (lambda (pt) (list pt (reference-func pt)))
       '((1 0 0 0 0)
         (1 0 0 1 1)
         (1 0 1 1 1)
         (0 1 0 0 1)
         (0 0 1 0 1)
         (1 1 1 1 0)
         (0 1 0 1 0)
         )))

(setf *expected-deps*
      #((2)
        (0)
        (1 2 3)
        (1 3 4)
        (0)
        ))

(defun vector-2 (length idx)
  (let ((result
         (make-list length :initial-element 0)))
    (setf (nth idx result) 2)
    result))


(defun main ()
  (let* (
         (poly-io:*vars* '("m" "b" "a" "l" "p"))
         ;(poly-io:*vars* '("a" "b" "c" "d" "e" "f"))

         (*on-sequence* #'on-sequence)
         (gauss:*finite-field* 2)
         (*pairs* (extract-pairs *transitions*))
         (points (extract-points *pairs*))
         (*matr-d* (make-array (list (length *vars*)
                                     (length *vars*)) :initial-element 0))
         (*deps-matched-gfan* nil)
         (*deps-matched-eugb* nil))
    ;(setf *vars* (subseq *vars* 0 5))
    ;(setf find-statistical-fan:*use-symmetry* t)
    (let ((*count* 0)
          (*boundary-dimples* (loop for i from 0 below (length *vars*)
                                 collect (vector-2 (length *vars*) i)))
          )
      ;(print (length points))
      (find-fan points)
      (format t "Found ~a models~%" *count*)
      (format t "GFAN: ~a, EUGB: ~a~%" *deps-matched-gfan*
              *deps-matched-eugb*)
      (return-from main)
      (loop for i from 0 below (length *vars*)
         do
           (loop for j from 0 below (length *vars*)
              do
                (setf (aref *matr-d* i j)
                      (/ (aref *matr-d* i j) *count*))
                (format t "~a " (float (aref *matr-d* i j))))
           (format t "~%"))
      ;(format t "D = ~%~a~%" *matr-d*)
      ;; (loop for i from 0 below (length *vars*)
      ;;    do
      ;;      (let ((avg (/ (loop for j from 0 below (length *vars*)
      ;;                       sum (aref *matr-d* i j))
      ;;                    (length *vars*))))
      ;;        (format t "~a <- " (nth i *vars*))
      ;;        (loop for j from 0 below (length *vars*)
      ;;           do
      ;;             (when (>= (aref *matr-d* i j) avg)
      ;;               (format t "~a " (nth j *vars*))))
      ;;        (format t "~%")
      ;;        ))
      )
   ))
