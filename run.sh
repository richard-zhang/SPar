#!/bin/sh

SRC=benchmark/$1
K=$2
DATA=benchmark/$1/rawData_${K}.csv

echo "size,seed,k,time" > $DATA
# rsync -R $SRC/*_${K}_*/code.o dest 
# in linux: cp --parents $SRC/*_${K}_*/code.o dest 

for x in $SRC/*_${K}_*/code.o
do
    # size=$(echo $x | grep -o -P "[0-9]+" | head -1)
    size=$(echo $x | perl -nle'print $& while m{[0-9]+}g' | tail -3 | head -1)
    # seed=$(echo $x | grep -o -P "(\d+)(?!.*\d)")
    # seed=$(echo $x | perl -nle'print $& while m{(\d+)(?!.*\d)}g')
    seed=$(echo $x | perl -nle'print $& while m{[0-9]+}g' | tail -1)
    echo $size $seed
    time=$(./$x)
    echo $size,$seed,$K,$time >> $DATA
done