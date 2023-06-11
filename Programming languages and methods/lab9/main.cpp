#include <iostream>
#include "Series.h"


int f(int n)
{
    return n*2;
}

int main() {
    Series<int> s= Series<int>(&f);
    s.setI(5);
    auto s1 = s+s;
    auto s2 = s*10;

    std::cout << s(5)<< std::endl;
    std::cout << s1(5)<< std::endl;
    std::cout << s2(5)<< std::endl;
    return 0;
}
