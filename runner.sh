#!/bin/bash
FILES=task/*
for f in $FILES
do
  echo "Processing $f file..."
  R -f $f
done
