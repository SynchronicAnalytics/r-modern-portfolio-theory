#!/bin/bash

for file in table.csv?s=*; do
    mv "$file" "${file/table.csv?s=/}"
done
