#include <iostream>
#include <vector>
#include <stdio.h>
#include <set>

using namespace std;

const int inf = 1e9;

int dijkstra(int n,int s,int ves1, vector< pair<int, int> >g[]) {
    vector<int> d(n, inf);
    d[s] =ves1;
    set< pair<int, int> > q;
    q.insert({d[s], s});
    while (!q.empty()) {
        int v = q.begin()->second;
        q.erase(q.begin());
        for (auto [u, w] : g[v]) {
            if (d[u] > d[v] + w) {
                q.erase({d[u], u});
                d[u] = d[v] + w;
                q.insert({d[u], u});
            }
        }
    }
    return d[n-1];
}
int main(){
    int n;
    cin >> n;
    vector< pair<int, int> > g[n*n];
    vector<vector<int>> matrix(n,vector<int>(n));
    int count=0;
    for (int i=0;i<n;i++){
        for (int j=0;j<n;j++){
            ::scanf("%d",&matrix[i][j]);
        }
    }
    int c=0;
    for (int i=0;i<n;i++){
        for (int j=0;j<n;j++){
            if (i > 0){
                g[c].push_back({c-n,matrix[i-1][j]});
            }
            if (j > 0) {
                g[c].push_back({c-1,matrix[i][j-1]});
            }
            if (i < n-1) {
                g[c].push_back({c+n,matrix[i+1][j]});
            }
            if (j < n-1) {
                g[c].push_back({c+1,matrix[i][j+1]});
            }
            c++;
        }
    }


    int res= dijkstra(c,0,matrix[0][0],g);
    ::printf("%d",res);
    return 0;
}

//++++++++
