#!/bin/bash

jbuilder build dump.exe

./_build/default/dump.exe -i data/bunny.txt -d 1 > data/bunny_bst.txt

egrep ^0' ' data/bunny_bst.txt > data/bst_left_01.txt
egrep ^1' ' data/bunny_bst.txt > data/bst_right_01.txt

gnuplot -persist <<EOF
set ticslevel 0
set tics out nomirror
set view 360, 360
unset tics
set xlabel 'x'
set ylabel 'y'
set zlabel 'z'
set term postscript eps enhanced color
set output 'data/bunny.eps'
splot 'data/bst_left_01.txt'  u 2:3:4 w d not, \
      'data/bst_right_01.txt' u 2:3:4 w d not
EOF
