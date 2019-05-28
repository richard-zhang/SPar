#!/bin/sh

SRC=benchmark/$1
DATA=benchmark/$1/rawData.csv
K=$2

echo "size,seed,k,time" > $DATA
# rsync -R $SRC/*_${K}_*/code.o dest 
# in linux: cp --parents $SRC/*_${K}_*/code.o dest 

for x in $SRC/*_${K}_*/code.o
do
    # size=$(echo $x | grep -o -P "[0-9]+" | head -1)
    size=$(echo $x | perl -nle'print $& while m{[0-9]+}g' | head -1)
    # seed=$(echo $x | grep -o -P "(\d+)(?!.*\d)")
    seed=$(echo $x | perl -nle'print $& while m{(\d+)(?!.*\d)}g')
    time=$(./$x)
    echo $size,$seed,$K,$time >> $DATA
done