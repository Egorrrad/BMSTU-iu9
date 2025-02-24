
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"
export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"

clang++ -fopenmp lab3.cpp -o lab3
./lab3 1
./lab3 2
./lab3 4
./lab3 8