#include <iostream>
#include <iterator>
#include <vector>

using namespace std;

int main() {
    int n;
    n=9;
    vector <vector <int>> matrix(n, vector <int>(n));
    int c=1;
    for (int i=0;i<n;i++){
        matrix[0][i]=c;
        matrix[i][0]=c;
        matrix[i][i]=c;
        c++;
    }

    vector <vector <int>> ::iterator it= matrix.begin();

    while(it!=matrix.end()){
        vector <int>::iterator iter=(*it).begin();
        while(iter!=(*it).end()){
            ::printf("%d ",*iter);
            iter++;
        }
        it++;
        ::printf("\n");
    }
    return 0;
}
