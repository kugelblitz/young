for i in `seq 90 10 200`; do 
    (time sbcl --load load-all.lisp --load main-enum-diag-2.lisp 2 $i)  > max-$i.txt 2>&1;
    done
