#!/bin/bash

# StockDownloadScript.sh
# Downloads stock price data from Yahoo! for later analysis

# Syntax: ./StockDownloadScript.sh <StockList> <Download Directory>
# <StockList> is a text file with each stock symbol to be downloaded listed on a seperate line
# <Download Directory> is relative to the current directory, and defaults to the current directory if not specified

# Example: ./StockDownloadScript.sh SandPStockList StockData

filename="$1"

if [ -z "$2" ]
    then directory="."
else
    directory="$2"
fi

while read -r symbol
do
    wget "http://chart.finance.yahoo.com/table.csv?s=$symbol" -P "$directory"
done < "$filename"
