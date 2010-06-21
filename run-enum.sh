export RUN="sbcl --noinform --noprint \
                --load declaim.lisp
                --load algebra.lisp \
		--load poly-io.lisp \
                --load queue.lisp \
                --load gauss.lisp \
                --load cone.lisp \
                --load cone2.lisp \
                --load enumerate.lisp \
		--load external-helpers.lisp \
                --load main-enum.lisp "

function run_weak
{
    (time $RUN weak $1 $2 2>/dev/null) >results/weak_$1_$2.txt 2>>results/weak_$1_$2.txt
}

function run_concave
{
    (time $RUN concave $1 $2 2>/dev/null) >results/concave_$1_$2.txt 2>>results/concave_$1_$2.txt
}

function run_strong
{
    (time $RUN strong $1 $2 2>/dev/null) >results/admissible_$1_$2.txt 2>>results/admissible_$1_$2.txt
}

function run_adm
{
    (time $RUN adm $1 $2 2>/dev/null) >results/adm_$1_$2.txt 2>>results/adm_$1_$2.txt
}

run_weak 4 3
#run_strong 2 100
#run_concave 2 10

#for n in `seq 4 16`; do run_weak 2 $n; done
#for n in `seq 4 12`; do run_weak 3 $n; done
#for n in `seq 4 16`; do run_convex 2 $n; done
#for n in `seq 4 12`; do run_convex 3 $n; done

#for n in `seq 14 14`; do run_convex 3 $n; done
#for n in `seq 21 21`; do run_weak 2 $n; done

#run_adm 2 7
#for n in `seq 3 20`; do run_adm 3 $n; done

#for n in `seq 4 14`; do run_strong 3 $n; done
#for n in `seq 400 400`; do run_strong 2 $n; done

#run_convex_random 2 80 100000
#for n in `seq 4 18`; do run_weak 2 $n; done
#run_concave 3 12
#run_weak 2 4
# for i in `seq 4 20`; do f=weak_2_$i.txt; echo -n "$i &" `fgrep ")))" $f | wc -l` "&" `head -n 1 $f` "& & \\\\"; echo; done
#run_strong 2 4
#for n in `seq 4 20; seq 30 10 50`; do run_strong 2 $n; done
#for n in `seq 4 20; seq 30 10 50`; do f=strong_3_$n.txt; echo $n "&" `tail -n 5 $f | head -n 1` "& \\\\" ; done
#for n in `seq 4 20; echo 50; seq 100 100 200`; do run_strong 3 $n; done
#for n in '40'; do run_strong 3 $n; done

#for i in `seq 3 20`; do F=results/adm_2_$i.txt; echo -n "$i ";L=`wc -l $F | awk ' { print $1 } '`; echo -n `expr $L - 4` ""; tail -n 4 $F | head -n 1; done

#for i in `seq 3 20`; do F=results/adm_2_$i.txt; echo -n "$i ";L=`wc -l $F | awk ' { print $1 } '`; echo -n `expr $L - 5` ""; tail -n 4 $F | head -n 1; done

