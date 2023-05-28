package main

import (
	"fmt"
	"strconv"
)

func DFS(mas [][]int, visit []bool, now int, comp []int, reb []int, chas []int, com int) ([]bool, []int, []int, []int) {
	visit[now] = true
	comp[now] = com
	chas[com] += 1
	for i := 0; i < len(mas[now]); i++ {
		k := mas[now][i]
		if !visit[k] {
			visit, comp, reb, chas = DFS(mas, visit, k, comp, reb, chas, com)
		}
	}
	reb[com] += len(mas[now])
	return visit, comp, reb, chas
}
func min2(a, b int) int {
	if a < b {
		return a
	}
	return b
}
func maxcomponent() {
	var n, m, i int
	var e1, e2 int
	fmt.Scan(&n)
	mas := make([][]int, n)
	ver := make([][]int, m)
	fmt.Scan(&m)
	for i = 0; i < m; i++ {
		fmt.Scan(&e1, &e2)
		mas[e1] = append(mas[e1], e2)
		if e1 != e2 {
			mas[e2] = append(mas[e2], e1)
			var a []int
			a = append(a, e1, e2)
			ver = append(ver, a)
		}
	}
	comp := make([]int, n)
	reb := make([]int, n)
	vis := make([]bool, n)
	chas := make([]int, n)
	c := 0
	ma := 0
	for i = 0; i < n; i++ {
		if !vis[i] {
			vis, comp, reb, chas = DFS(mas, vis, i, comp, reb, chas, c)
			if chas[c] > ma {
				ma = chas[c]
			}
			c += 1
		}
	}

	fmt.Print("graph {\n")
	r := 0
	rebMax := 0
	for i = 0; i < c; i++ {
		if chas[i] == ma {
			reber := reb[i]
			if reber > rebMax {
				r = i
				rebMax = reber
			} else if reber == rebMax {
				r = min2(r, i)
			}
		}
	}

	for i = 0; i < n; i++ {
		if comp[i] == r {
			fmt.Print("\t" + strconv.Itoa(i) + " [color=red]\n")
		} else {
			fmt.Print("\t" + strconv.Itoa(i) + "\n")
		}
	}

	for i = 0; i < len(ver); i++ {
		if comp[ver[i][0]] == r {
			fmt.Print("\t" + strconv.Itoa(ver[i][0]) + " -- " + strconv.Itoa(ver[i][1]) + " [color=red]" + "\n")
		} else {
			fmt.Print("\t" + strconv.Itoa(ver[i][0]) + " -- " + strconv.Itoa(ver[i][1]) + "\n")
		}
	}
	fmt.Print("}")

}

func main0() {
	//start := time.Now()
	maxcomponent()
	//fmt.Println(time.Now().Sub(start))
}

//сложность не та все ещё
//а не надо все в одну строку было сохраянять
//+++++
