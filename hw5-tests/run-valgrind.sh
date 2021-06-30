#! /bin/sh

for i in $(ls -v *.in | sed "s/\.in//g")
do
	valgrind --leak-check=full --show-leak-kinds=all ./hw5 < $i.in
done
