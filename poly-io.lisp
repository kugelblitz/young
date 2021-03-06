(asdf:operate 'asdf:load-op :cl-ppcre)

(defpackage poly-io
  (:use :common-lisp :cl-ppcre)
  (:export parse-basis-maxima parse-gfan-basis parse-gfan-bases
           term-print poly-print polys-print vars-print parse-poly parse-polys
           *vars*))

(in-package poly-io)

(defvar *vars* nil)

(defun parse-term (str-term str-vars)
  (let ((result (make-sequence
                 'list (1+ (length str-vars)) :initial-element 0))
        (parts)
        (str-var)
        (degree))
    (setf str-term (remove #\Space str-term))
    (setf str-term (remove #\Newline str-term))
    (setf (car result) 1)
    (when (string= "+" (subseq str-term 0 1))
      (setf str-term (subseq str-term 1)))
    
    (when (string= "-" (subseq str-term 0 1))
      (setf (car result) -1)
      (setf str-term (subseq str-term 1)))
    (setq parts (cl-ppcre:split "\\*" str-term))
    (when (numberp (read-from-string (car parts)))
      (setf (car result) (* (car result)
			   (read-from-string (car parts))))
      (setq parts (cdr parts)))
    (loop for part in parts do
         (setq str-var part)
         (setq degree 1)
         (when (find #\^ part)
           (let ((l (cl-ppcre:split "\\^" part)))
             (setq str-var (first l))
             (setq degree (parse-integer (second l)))))
         (incf (nth (1+ (position
                         str-var
                         str-vars :test 'equalp)) result) degree))
    result))

(defun parse-term-list (str-poly str-vars)
  (setq str-poly (remove #\[ str-poly))
  (setq str-poly (remove #\] str-poly))
  ;(setq str-poly (subseq str-poly 1 (1- (length str-poly))))
  (let ((str-terms (cl-ppcre:split "," str-poly)))
    (mapcar (lambda (str-term) (parse-term str-term str-vars))
	    str-terms)))

(defun parse-basis-maxima (str-polys)
  (let* ((result))
    (setq str-polys (remove #\Newline str-polys))
    (setq str-polys (remove #\Space str-polys))
    (setq str-polys (subseq str-polys 1 (1- (length str-polys))))
    (setq str-polys (cl-ppcre:split "]," str-polys))
    (setf result (mapcar (lambda (str-poly)
			    (parse-term-list str-poly *vars*))
			 str-polys))
    result))

(defun parse-poly (str-poly)
  (let ((str-terms (cl-ppcre:all-matches-as-strings
		    "(\\+|\\-|)[^\\+\\-]+" str-poly)))
    (mapcar (lambda (str-term) (parse-term str-term *vars*))
	    str-terms))
  )

(defun parse-polys (str-polys)
  (mapcar #'parse-poly (cl-ppcre:split "," str-polys)))

(defun read-lines-until-slash ()
  (with-output-to-string (out)
    (loop
     (multiple-value-bind (line nl)
	 (read-line *standard-input* nil *standard-input*)
       (when (eq line *standard-input*)
         (return))
       (when (string= line "/")
	 (return))
       (write-string line out)
       (unless nl
         (write-char #\Newline out))))))

(defun parse-gfan-basis ()
  (let* ((header (read-line))
         (str-vars
          (cl-ppcre:register-groups-bind
           (vars nil)
           ("\\w+\\[((\\w|,)+)\\]" header) vars))
         (vars (cl-ppcre:split "," str-vars))
         (str-polys (read-lines-until-slash))
         (result))
    (setf *vars* vars)
    (setf str-polys (remove #\{ str-polys))
    (setf str-polys (remove #\} str-polys))
    (setf str-polys (remove #\Newline str-polys))
    (setf str-polys (cl-ppcre:split "," str-polys))
    (setf result (mapcar (lambda (str-poly)
                           (parse-poly str-poly))
			 str-polys))
    (values result vars)))

(defun parse-gfan-bases ()
  (let* ((header (read-line))
         (str-vars (cl-ppcre:register-groups-bind (vars nil)
                       ("\\w+\\[((\\w|,)+)\\]" header) vars))
         (vars (cl-ppcre:split "," str-vars))
         (str-bases)
         (result))
    (setf *vars* vars)
    (setf str-bases (read-lines-until-slash))
    (setf str-bases (remove #\Newline str-bases))
    (setf str-bases (cl-ppcre:register-groups-bind (str nil)
                        ("\\{(.*)\\}" str-bases) str))
    (cl-ppcre:do-matches (s e "\\{([^\\{\\}]*)\\}" str-bases nil)
      (let* ((str-base (subseq str-bases (1+ s) (1- e)))
             (str-polys (cl-ppcre:split "," str-base))
             (basis))
        (dolist (str-poly str-polys)
          (push (parse-poly str-poly) basis))
        (push (nreverse basis) result)))
    (values (nreverse result) vars)))

(defun term-print (term &optional plus)
  (let ((coef (car term))
        (degrees (cdr term))
        (hang "")
        (allzero t))
    (when (= coef 0)
      (format t "0"))
    (when (> coef 0)
      (when plus
        (format t "+")))
    (when (< coef 0)
      (format t "-")
      (setq coef (- coef)))
    (when (> coef 0)
      (when (/= coef 1)
        (format t "~a" coef)
        (setf hang "*"))
      (loop for degree in degrees
         for var in *vars* do
           (when (> degree 0)
             (format t "~a~a" hang var)
             (setf hang "*")
             (setf allzero nil)
             (when (> degree 1)
               (format t "^~a" degree))))
      (when allzero
        (when (= coef 1)
          (format t "~a" coef))))))

(defun poly-print (poly)
  (let ((plus nil))
    (dolist (term poly)
      (term-print term plus)
      (setf plus t))))

(defun polys-print (polys &optional (delimiter ","))
  (let ((comma nil))
    (dolist (poly polys)
      (when comma
        (format t comma))
      (poly-print poly)
      (setf comma delimiter))))

(defun vars-print ()
  (let ((comma nil))
    (dolist (var *vars*)
      (when comma
        (format t comma))
      (format t var)
      (setf comma ","))))
