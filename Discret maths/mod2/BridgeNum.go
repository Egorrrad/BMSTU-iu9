package main

import (
	"fmt"
)

func min3(a, b int) int {
	if a < b {
		return a
	}
	return b
}
func dfs(v int, p int, vis []bool, t []int, f []int, array [][]int, count int, r int) int {
	vis[v] = true
	count += 1
	t[v] = count
	f[v] = count
	for i := 0; i < len(array[v]); i++ {
		e := array[v][i]
		if e == p {
			continue
		}
		if vis[e] {
			f[v] = min3(f[v], t[e])
		} else {
			r = dfs(e, v, vis, t, f, array, count, r)
			f[v] = min3(f[v], f[e])
			if f[e] > t[v] {
				r += 1
			}
		}
	}
	return r
}

func BridgeNum() {
	var n, m, u, v int
	fmt.Scan(&n)
	fmt.Scan(&m)
	mas := make([][]int, n, n)
	for i := 0; i < m; i++ {
		fmt.Scan(&u, &v)
		mas[u] = append(mas[u], v)
		mas[v] = append(mas[v], u)
	}
	t := make([]int, n)
	f := make([]int, n)
	r := 0
	vis := make([]bool, n)
	for i := 0; i < n; i++ {
		vis[i] = false
	}
	for i := 0; i < n; i++ {
		if !vis[i] {
			r += dfs(i, 0, vis, t, f, mas, 0, 0)
		}
	}
	fmt.Println(r)
}

func main000() {
	BridgeNum()
}

//++++
