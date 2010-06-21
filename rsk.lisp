(defpackage rsk
  (:use :common-lisp )
  (:export make-table-rsk))

(in-package rsk)

(defun insert-to-row (row pair)
  (let ((number (car pair)))
    (if (or (null row) (< number (caar row)))
        (return-from insert-to-row
          (values (cons pair (cdr row)) (car row)))
        (do ((iter row (cdr iter)))
            ((null iter))
          (when (null (cdr iter))
            (setf (cdr iter) (list pair))
            (return-from insert-to-row (values row nil)))
          (when (< number (caadr iter))
            (let ((removed (cdr iter)))
              (setf (cdr iter) (cons pair (cddr iter)))
              (return-from insert-to-row (values row (car removed)))))))
    (error "must not come here")))

(defun table-insert (rows pair)
  (if rows
      (multiple-value-bind (new-row new-pair)
          (insert-to-row (car rows) pair)
        (cons new-row
              (if new-pair
                  (table-insert (cdr rows) new-pair)
                  (cdr rows))))
      (list (list pair))))

(defun make-table-rsk (numbers)
  (let ((table)
        (counter 0))
    (dolist (number numbers)
      (setf table (table-insert table (list number counter)))
      (incf counter))
    table))
