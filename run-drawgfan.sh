(./gf.sh "$@") | \
    sbcl --noinform --noprint \
         --load declaim.lisp \
         --load algebra.lisp \
         --load poly-io.lisp \
         --load cone.lisp \
         --load draw-poly.lisp \
         --load main-gfan-draw.lisp 2>/dev/null
