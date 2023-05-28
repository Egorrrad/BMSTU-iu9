package main

import (
	"fmt"
	"math"
)

func qs1(low, high int, less func(i, j int) bool,
	swap func(i, j int)) {
	if low < high {
		var i, j int
		i = low
		j = low
		for j < high {
			if less(j, high) {
				swap(i, j)
				i += 1
			}
			j += 1
		}
		swap(i, high)
		q := i
		if low < q-1 {
			qs1(low, q-1, less, swap)
		}
		if high > q+1 {
			qs1(q+1, high, less, swap)
		}
	}
}

func qsort1(n int,
	less func(i, j int) bool,
	swap func(i, j int)) {
	if n == 0 || n == 1 {
		return
	}
	qs1(0, n-1, less, swap)
}

type ver struct {
	x, y int
	num  int
}
type edge struct {
	u, v   int
	weight float64
}

func distance(p1 ver, p2 ver) float64 {
	var res float64
	res = math.Sqrt(float64((p2.x-p1.x)*(p2.x-p1.x) + (p2.y-p1.y)*(p2.y-p1.y)))
	return res
}

var par, ran []int

func find_set(v int) int {
	if v == par[v] {
		return v
	}
	par[v] = find_set(par[v])
	return par[v]
}

func union_sets(a, b int) {
	if a != b {
		if ran[a] < ran[b] {
			ran[a], ran[b] = ran[b], ran[a]
		}
		par[b] = a
		if ran[a] == ran[b] {
			ran[a] += 1
		}
	}

}

func Kruskal() {
	var n int
	var res float64
	fmt.Scan(&n)
	var mas []ver
	for i := 0; i < n; i++ {
		var p ver
		fmt.Scanln(&p.x, &p.y)
		p.num = i
		mas = append(mas, p)
	}
	if n == 2 {
		fmt.Printf("%.2f", distance(mas[0], mas[1]))
		return
	}
	//fmt.Println(mas)
	var edges []edge
	c := 0
	for i := 0; i < len(mas); i++ {
		for k := i + 1; k < n; k++ {
			var e edge
			e.v = mas[i].num
			e.u = mas[k].num
			e.weight = distance(mas[i], mas[k])
			//fmt.Printf("V: %d U: %d Weight: %f\n", e.v, e.u, e.weight)
			par = append(par, c)
			c++
			edges = append(edges, e)
		}
	}
	l := len(edges)
	ran = make([]int, l)
	qsort1(l,
		func(i, j int) bool {
			return edges[i].weight < edges[j].weight
		},
		func(i, j int) {
			edges[i], edges[j] = edges[j], edges[i]
		})

	for i := 0; i < l; i++ {
		e := edges[i]
		f1, f2 := find_set(e.u), find_set(e.v)
		if f1 != f2 {
			res += e.weight
			union_sets(f1, f2)
		}
	}

	fmt.Printf("%.2f", res)
}

func main0000() {
	Kruskal()
}

//++++ даже похожих посылок не найдено
