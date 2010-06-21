(./gf.sh "$@") | \
    lisp \
         -load declaim.lisp \
	 -load algebra.lisp \
         -load gauss.lisp \
         -load queue.lisp \
         -load cone.lisp \
         -load cone2.lisp \
         -load enumerate.lisp \
         -load poly-io.lisp \
	 -load external-helpers.lisp \
         -load main-homology.lisp 2>/dev/null
