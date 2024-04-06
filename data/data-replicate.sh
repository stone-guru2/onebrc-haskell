#!/bin/bash

DATA_FILE=./weather_stations.csv
TARGET_FILE=./weather_big.csv

n=1816
i=0
while [ $i -lt $n ] ; do
    echo $i
    cat $DATA_FILE >> $TARGET_FILE
    i=$((i+1))
done      
