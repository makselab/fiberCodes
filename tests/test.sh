#!/bin/bash

cd ..
./run.pl -outputTo ./tests/out1.txt -inputFrom ./tests/test1.txt
./run.pl -outputTo ./tests/out2.txt -inputFrom ./tests/test2.txt -weighted
./run.pl -outputTo ./tests/out3.txt -inputFrom ./tests/test3.txt -directed
./run.pl -outputTo ./tests/out4.txt -inputFrom ./tests/test4.txt -weighted -directed
./run.pl -outputTo ./tests/out5.txt -inputFrom ./tests/test5.txt -weighted
./run.pl -outputTo ./tests/out6.txt -inputFrom ./tests/test6.txt -weighted -directed
cd ./tests
