#!/bin/sh
# This is a comment on my shell script.
pwd
#wget https://...
mkdir temporary
cd temporary
touch file.txt
wc -l file.txt

cd ..
ls
ls -al
rm -rf temporary


