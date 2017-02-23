#!/bin/bash

filename="$1"
while read -r symbol
do
    wget "http://chart.finance.yahoo.com/table.csv?s=$symbol"
done < "$filename"
