#!/bin/bash
touch log.txt
FILES=task/*
for f in $FILES
do
  echo "Processing $f file..."
  R -f $f >> log.txt
done
