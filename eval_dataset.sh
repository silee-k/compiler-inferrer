#!/bin/bash

# bash ./eval_dataset.sh binutils_obf .. '../obfus/binutils/binutils-*x86*'
# bash ./eval_dataset.sh binutils .. '../normal_dataset/binutils/binutils-*x86*_O2_*'
# bash ./eval_dataset.sh binutils_icc .. '../binutils_icc/*'
[ $CORES ] || CORES=84
bins=$(find $2 -path "$3" | xargs)
i=0
for bin in $bins; do
    bin_name=$(basename $bin)
    [ -f "$bin" ] || continue
    taskset -c $i bin/Debug/net6.0/CompilerInferrer $bin > $1/$bin_name &
    ((i++))
    if ((i % $CORES == 0)); then
        i=0
        wait
    fi
done 