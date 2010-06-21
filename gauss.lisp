(defpackage gauss
  (:use :common-lisp)
  (:export
   :add-polynom
   *matrix*
   *sqmatrix*
   *hmatrix*
   *hash*
   *n-polys*
   *n-monoms*))

(in-package gauss)

(defvar *matrix*)
(defvar *hmatrix*)
(defvar *sqmatrix*)
(defvar *hash*)
(defvar *n-polys*)
(defvar *n-monoms*)



(defun copy-hash-table (table &key key test size
                                   rehash-size rehash-threshold)
  "Returns a copy of hash table TABLE, with the same keys and values
as the TABLE. The copy has the same properties as the original, unless
overridden by the keyword arguments.

Before each of the original values is set into the new hash-table, KEY
is invoked on the value. As KEY defaults to CL:IDENTITY, a shallow
copy is returned by default."
  (setf key (or key 'identity))
  (setf test (or test (hash-table-test table)))
  (setf size (or size (hash-table-size table)))
  (setf rehash-size (or rehash-size (hash-table-rehash-size table)))
  (setf rehash-threshold (or rehash-threshold (hash-table-rehash-threshold table)))
  (let ((copy (make-hash-table :test test :size size
                               :rehash-size rehash-size
                               :rehash-threshold rehash-threshold)))
    (maphash (lambda (k v)
               (setf (gethash k copy) (funcall key v)))
             table)
    copy))


(defun add-polynom (poly)
  (incf *n-polys*)
  (setf *matrix*
        (adjust-array
         *matrix* (list *n-polys* *n-monoms*)
         :initial-element 0))
  (setf *sqmatrix*
        (adjust-array
         *sqmatrix* (list *n-polys* *n-polys*)
         :initial-element 0))
  (setf (aref *sqmatrix* (1- *n-polys*) (1- *n-polys*)) 1)
  (setf *hash* (copy-hash-table *hash*))
  
  (loop for term in poly do
       (let ((hh (gethash (cdr term) *hash*)))
         (unless hh
           (incf *n-monoms*)
           (setf hh (hash-table-count *hash*))
           (setf (gethash (cdr term) *hash*) hh)
           (setf *matrix*
                 (adjust-array
                  *matrix* (list *n-polys* *n-monoms*)
                  :initial-element 0))
           (setf *hmatrix*
                 (adjust-array
                  *hmatrix* *n-monoms*
                  :initial-element -1)))
         (setf (aref *matrix* (1- *n-polys*) hh) (car term))))

  (loop for i from 0 below *n-monoms*
     do
       (let ((row (aref *hmatrix* i))
             (val (aref *matrix* (1- *n-polys*) i)))
         (when (and (/= -1 row) (/= 0 val))
           (loop for j from 0 below *n-monoms*
              do
                (decf (aref *matrix* (1- *n-polys*) j)
                      (* val (aref *matrix* row j))))
           (loop for j from 0 below *n-polys*
              do
                (decf (aref *sqmatrix* (1- *n-polys*) j)
                      (* val (aref *sqmatrix* row j)))))))
  (loop for i from 0 below *n-monoms*
     do
       (when (and (= -1 (aref *hmatrix* i))
                  (/= 0 (aref *matrix* (1- *n-polys*) i)))
         (setf (aref *hmatrix* i) (1- *n-polys*))
         (let ((val (aref *matrix* (1- *n-polys*) i)))
           (loop for j from 0 below *n-monoms* do
                (setf (aref *matrix* (1- *n-polys*) j)
                      (/ (aref *matrix* (1- *n-polys*) j) val)))
           (loop for j from 0 below *n-polys* do
                (setf (aref *sqmatrix* (1- *n-polys*) j)
                      (/ (aref *sqmatrix* (1- *n-polys*) j) val)))
           (loop for k from 0 below (1- *n-polys*) do
                (setq val (aref *matrix* k i))
                (loop for j from 0 below *n-monoms* do
                     (decf (aref *matrix* k j)
                           (* val (aref *matrix* (1- *n-polys*) j))))
                (loop for j from 0 below *n-polys* do
                     (decf (aref *sqmatrix* k j)
                           (* val (aref *sqmatrix* (1- *n-polys*) j))))))
         (return))
     finally
       (return-from add-polynom
         (let ((result))
           (loop for i from 0 below *n-polys*
              do
                (push (aref *sqmatrix* (1- *n-polys*) i) result))
           (nreverse result))))
  nil)

