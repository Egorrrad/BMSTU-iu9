package main

import (
	"fmt"
)

func BFS(s int, mas [][]int, d, mark []int) []int {
	var q []int
	q = append(q, s)
	d[s] = 0
	mark[s] = 1
	for len(q) > 0 {
		v := q[0]
		q = q[1:]
		for i := 0; i < len(mas[v]); i++ {
			if mark[mas[v][i]] == 0 {
				d[mas[v][i]] = d[v] + 1
				mark[mas[v][i]] = 1
				q = append(q, mas[v][i])
			}
		}
	}
	return d
}
func EqDist() {
	var n, m, k int
	fmt.Scanln(&n)
	fmt.Scanln(&m)
	mas := make([][]int, n)
	var opor []int
	for i := 0; i < m; i++ {
		var e1, e2 int
		fmt.Scanln(&e1, &e2)
		mas[e1] = append(mas[e1], e2)
		mas[e2] = append(mas[e2], e1)
	}
	fmt.Scanln(&k)
	for i := 0; i < k; i++ {
		var e int
		fmt.Scan(&e)
		opor = append(opor, e)
	}
	var res []int
	var r [][]int
	for i := 0; i < k; i++ {
		d := make([]int, n)
		mark := make([]int, n)
		d = BFS(opor[i], mas, d, mark)
		r = append(r, d)
		//fmt.Println(d)
	}
	for i := 0; i < n; i++ {
		e := r[0][i]
		c := 1
		for ; c < len(r); c++ {
			if e != r[c][i] || e == 0 {
				break
			}
		}
		if c == len(r) {
			res = append(res, i)
		}
	}

	if len(res) == 0 {
		fmt.Println("-")
		return
	}
	for i := 0; i < len(res); i++ {
		fmt.Printf("%d ", res[i])
	}
}

func main989() {
	EqDist()
}

//+++++
