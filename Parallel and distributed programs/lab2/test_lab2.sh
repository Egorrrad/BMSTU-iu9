echo "" > results.txt
for num in 1 2 4 8 16
do
    mpirun  -n $num python lab2.py >> results.txt
done