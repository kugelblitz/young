;(declaim (optimize (speed 0) (safety 3) (debug 3)))
(declaim (optimize (speed 3) (safety 0) (debug 0)))
(progn #+cmu (setq ext:*gc-verbose* nil))

