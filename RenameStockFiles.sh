#!/bin/bash

# RenameStockFiles.sh
# Removes the "table.csv?s=" prefix from stock data downloaded with StockDownloadScript.sh

# Syntax: ./RenameStockFiles.sh <Directory>
# <Directory> is where the files are stored relative to the current directory, and defaults to the current directory

# Example: ./RenameStockFiles.sh StockData

if [ -z "$1" ]
    then directory="."
else
    directory="$1"
fi

for file in "$directory"/table.csv?s=*; do
    mv "$file" "${file/table.csv?s=/}"
done
