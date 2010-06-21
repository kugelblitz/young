(defpackage young
  (:use :common-lisp :rsk :combinatorics :calculus
        :algebra :geometry :queue :dimension :diagram-hooks)
  (:export
   add-dimple
   add-corner
   update-dimples
   enumerate-weak-orderings
   enumerate-convex-orderings
   enumerate-admissible-orderings
   *on-sequence*
   *on-diagram*
   *boundary-dimples*
   calc-orderings-in-diagram
   random-diagram-richardson
   random-diagram-plancherel-markov
   random-diagram-rsk
   enumerate-diagrams
   enumerate-symmetric-diagrams-2d))

(in-package young)

(defvar *on-sequence* nil)
(defvar *on-diagram* nil)
(defvar *boundary-dimples* nil)

(defun yield-tableau (sequence)
  (when *on-sequence*
    (funcall *on-sequence* (lambda () (reverse sequence)))))

(defun yield-diagram (size corners dimples)
  (when *on-diagram*
    (funcall *on-diagram* (lambda (name)
                            (cond
                              ((equalp name :corners) corners)
                              ((equalp name :dimples) dimples)
                              ((equalp name :size) size))))))

(defun update-dimples (dimples new-cell)
  (for-all-increments
   new-cell
   (lambda (new-dimple)
     (unless (find-divisor new-dimple dimples)
       (unless (find-divisor new-dimple *boundary-dimples*)
         (push new-dimple dimples)))))
  dimples)

(defun add-dimple (dimples new-cell)
  (update-dimples (remove new-cell dimples :test #'equalp) new-cell))

(defun add-corner (corners new-corner)
  (cons new-corner
        (remove-if
         (lambda (x)
           (monom-divides x new-corner))
         corners)))

(defun is-correct-hyperplane (hp dimples)
  (if (is-zero-vector (cdr hp))
      nil
      (every #'(lambda (point)
                 (>= (the fixnum (point-sign point hp)) 0))
             dimples)))

(defun make-hyperplanes (dimples)
  (let ((result))
    (for-all-subsets-of-size
     dimples *dimension*
     (lambda (subset)
       (let ((hp (hyperplane-orient-from-origin
                  (hyperplane subset))))
         (if (is-correct-hyperplane hp dimples)
             (push hp result)))))
    result))

(defun is-canonical-diagram (sequence corners)
  (equalp (car sequence) (min-lex corners)))

(defun enumerate-weak-orderings
    (&optional
     n
     sequence
     (dimples (list (zero-vector)))
     (canonical-diagram *on-diagram*)
     corners)
  (when (and canonical-diagram corners)
    (setf canonical-diagram
          (is-canonical-diagram sequence corners)))
  (if (or (and n (zerop n)) (null dimples))
      (progn
        (yield-tableau sequence)
        (when canonical-diagram
          (yield-diagram nil corners dimples)))
      (for-all-cuts
       dimples
       (lambda (dimple other-dimples)
         (enumerate-weak-orderings
          (when n (1- n))
          (cons dimple sequence)
          (update-dimples
           other-dimples
           dimple)
          canonical-diagram
          (when canonical-diagram
            (add-corner corners dimple)))))))

(defun are-corners-above-hyperplanes (corners hps)
  (dolist (corner corners)
    (when
        (block beta
          (dolist (hp hps)
            (when (< (point-sign corner hp) 0)
              (return-from beta nil)))
          t)
      (return-from are-corners-above-hyperplanes nil)))
  t)

(defun enumerate-convex-orderings
    (n &optional sequence (dimples (list (zero-vector)))
     corners diagram-hash)
  (unless diagram-hash
    (when *on-diagram*
      (setf diagram-hash (make-hash-table :test #'equalp))))
  (if (zerop n)
      (progn
        (yield-tableau sequence)
        (when diagram-hash
          (let ((sorted (sort (copy-list dimples) #'lex-less)))
            (unless (gethash sorted diagram-hash)
              (setf (gethash sorted diagram-hash) 1)
              (yield-diagram nil corners sorted)))))
      (for-all-cuts
       dimples
       (lambda (dimple other-dimples)
         (let* ((new-dimples
                 (update-dimples other-dimples dimple))
                (new-corners
                 (add-corner corners dimple))
                (new-hps
                 (make-hyperplanes new-dimples)))
           (when (are-corners-above-hyperplanes new-corners new-hps)
             (enumerate-convex-orderings
              (1- n)
              (cons dimple sequence)
              new-dimples
              new-corners
              diagram-hash)))))))

(defun admissible-check (corner divisors front)
  (loop
     for coord of-type fixnum in corner
     for i of-type fixnum from 0 below *dimension* do
       (unless (zerop coord)
         (dolist (front-elem (car front))
           (let ((front-monom (getf front-elem :monom))
                 (front-have-nei (getf front-elem :have-nei)))
             (if (equalp corner (increment-point front-monom i))
                 (progn
                   (setf (aref divisors i) front-elem)
                   (return))
                 (unless (aref front-have-nei i)
                   (return-from admissible-check nil)))))))
  t)

(defun admissible-update (divisors corner front-new)
  (loop for i of-type fixnum from 0 below *dimension* do
       (let ((divisor (aref divisors i)))
         (when divisor
           (setf (aref (getf divisor :have-nei) i) t)
           (incf (getf divisor :nei-count)))))
  (loop while (and (car front-new)
                   (= *dimension*
                      (getf (caar front-new) :nei-count))) do
       (dequeue front-new))
  
  (let* ((have-nei (make-array *dimension*
                               :element-type 'boolean
                               :initial-element nil))
         (q-elem (list
                  :monom corner
                  :have-nei have-nei
                  :nei-count 0)))
    (enqueue q-elem front-new)))

(defun copy-front (front)
  (let ((front-new (make-queue)))
    (dolist (elem (car front))
      (let ((elem-copy (copy-list elem)))
        (setf (getf elem-copy :have-nei)
              (copy-seq (getf elem :have-nei)))
        (enqueue elem-copy front-new)))
    front-new))

(defun enumerate-admissible-orderings (n &optional sequence
                                       (dimples (list (zero-vector)))
                                       (front (make-queue)) diagram-hash)
  (unless diagram-hash
    (when *on-diagram*
      (setf diagram-hash (make-hash-table :test #'equalp))))
  (if (zerop n)
      (progn
        (yield-tableau sequence)
        (when diagram-hash
          (let ((sorted (sort (copy-list dimples) #'lex-less)))
            (unless (gethash sorted diagram-hash)
              (setf (gethash sorted diagram-hash) 1)
              (yield-diagram n nil sorted)))))
      (for-all-cuts
       dimples
       (lambda (dimple other-dimples)
         (let ((divisors (make-array *dimension* :initial-element nil)))
           (let ((front-new (copy-front front)))
             (when (admissible-check dimple divisors front-new)
               (admissible-update divisors dimple front-new)
               (enumerate-admissible-orderings
                (1- n)
                (cons dimple sequence)
                (update-dimples other-dimples dimple)
                front-new
                diagram-hash))))))))

(defun select-random-element (lst)
  (declare (list lst))
  (nth (random (length lst)) lst))

(defun select-random-dimple (dimples)
  (copy-list (select-random-element dimples)))

(defun random-diagram-richardson (n)
  (let ((dimples (list (zero-vector)))
        (corners nil))
    (loop for i of-type fixnum from 1 to n do
         (let ((dimple (select-random-dimple dimples)))
           (setf dimples (add-dimple dimples dimple))
           (setf corners (add-corner corners dimple))
           (yield-diagram i corners dimples)))
    corners))

(defun calc-orderings-in-diagram (corners dimples)
  (when (= *dimension* 2)
    (let ((res 1)
          (cnt 0))
      (for-diagram-hooks corners
                         (lambda (i j hook)
                           (declare (ignore i j))
                           (incf cnt)
                           (setf res (* res (/ cnt hook)))))
      (return-from calc-orderings-in-diagram res)))
  (let* ((count 0)
         (*on-sequence* (lambda (seq) (declare (ignore seq)) (incf count)))
         (*on-diagram* nil)
         (*boundary-dimples* dimples))
    (enumerate-weak-orderings 10000)
    count))

(defun random-diagram-plancherel-markov (n)
  (let ((dimples (list (zero-vector)))
        (corners nil))
    (loop for i of-type fixnum from 1 to n do
         (let* ((outer-dims
                 (mapcar
                  (lambda (dimple)
                    (calc-orderings-in-diagram
                     (cons dimple
                           (remove-if
                            (lambda (x)
                              (monom-divides x dimple))
                            corners))
                     nil))
                  dimples))
                (r (random (apply #'+ outer-dims)))
                (iter 0)
                (new-corner nil))
           (loop
              for dim in outer-dims
              for corner in dimples do
                (incf iter dim)
                (when (< r iter)
                  (setf new-corner corner)
                  (return)))
           
           (setf dimples
                 (update-dimples
                  (remove new-corner dimples :test #'equalp)
                  new-corner))
           (setf corners (add-corner corners new-corner))
           (yield-diagram i corners dimples)))
    corners))

(defun diagram-corners-by-partition (ptn)
  (let ((res)
        (i 0))
    (mapl
     (lambda (list)
       (when (or (not (cdr list)) (/= (car list) (cadr list)))
         (push (list i (1- (car list))) res))
       (incf i))
     ptn)
    (reverse res)))

(defun random-diagram-rsk (n)
  (let ((rsk-table)
        (heights))
    (setf rsk-table (make-table-rsk (random-permutation n)))
    (setf heights (mapcar #'length rsk-table))
    (diagram-corners-by-partition heights)))

(defun enumerate-diagrams-2d (n)
  (for-all-partitions
   n
   (lambda (ptn)
     (yield-diagram n (diagram-corners-by-partition (reverse ptn)) nil))))

(defun enumerate-diagrams (n &optional corners
                           (dimples (list (zero-vector))))
  (if (= *dimension* 2)
      (enumerate-diagrams-2d n)
      (if (zerop n)
          (yield-diagram nil corners dimples)
          (for-all-cuts
           dimples
           (lambda (dimple other-dimples)
             (let ((new-corners
                    (sort (add-corner corners dimple) #'lex-less)))
               (when (equalp dimple (car new-corners))
                 (enumerate-diagrams (1- n) new-corners
                                     (update-dimples other-dimples dimple)))))))))

(defun for-diagram-increments (n inner)
  (if inner
      (let ((x 0))
        (dolist (pt inner)
          (let ((y (1+ (second pt))))
            (when (>= y x)
              (yield-diagram n (add-corner inner (list x y)) nil))
            (setf x (1+ (first pt))))))
      (yield-diagram n '((0 0)) nil)))

(defun enumerate-symmetric-diagrams-2d-sub (n func level prev-h n-cur)
  (let ((k
         (+ (* 2 (- n-cur (/ (* level (1+ level)) 2)))
            level)))
    (cond
      ((> k n)
       nil)
      ((= k n)
       (funcall func nil t))
      ((= k (1- n))
       (funcall func nil nil))
      (t
       (loop for i from level to prev-h
          do
            (enumerate-symmetric-diagrams-2d-sub
             n (lambda (ptn sym)
                 (funcall func
                          (cons (1+ i) ptn) sym))
             (1+ level) i (+ n-cur (1+ i))))))))


(defun mirror-diagram (inner)
  (let ((appendix)
        (iter inner))
    (when iter
      (loop
         do
           (let ((x (caar iter))
                 (y (cadar iter)))
             (when (< x y)
               (push (list y x) appendix)))
           (unless (cdr iter)
             (return))
           (setf iter (cdr iter)))
      (setf (cdr iter) appendix))
    inner))


(defun enumerate-symmetric-diagrams-2d (n)
  (enumerate-symmetric-diagrams-2d-sub
   n
   (lambda (half is-symmetric)
     (let ((inner (mirror-diagram (diagram-corners-by-partition half))))
       (if is-symmetric
           (yield-diagram n inner nil)
           (for-diagram-increments n inner))))
   0 (1- n) 0))
