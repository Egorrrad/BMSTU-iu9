package main

import (
	"fmt"
)

func min(a vertex, b vertex) vertex {
	if a.weight < b.weight {
		return a
	}
	return b
}

type vertex struct {
	num    int
	weight int
}

func prim(mas [][]vertex, num int, bul []bool, otstov []int) int {
	res := 0
	bul[num] = true
	otstov = append(otstov, num)
	var mi vertex
	mi.weight = 10000000000000
	mi.num = 0
	c := 0
	for k := 0; k < len(otstov); k++ {
		num = otstov[k]
		for i := 0; i < len(mas[num]); i++ {
			if !bul[mas[num][i].num] {
				mi = min(mi, mas[num][i])
				c += 1
			}
		}
	}
	if c == 0 {
		return res
	}
	res = mi.weight
	//fmt.Println(mi)
	res += prim(mas, mi.num, bul, otstov)
	return res
}
func main121() {
	var n, m int
	fmt.Scan(&n)
	fmt.Scan(&m)
	mas := make([][]vertex, n)
	for i := 0; i < m; i++ {
		var vert1, vert2 vertex
		fmt.Scanln(&vert1.num, &vert2.num, &vert1.weight)
		vert2.weight = vert1.weight
		mas[vert1.num] = append(mas[vert1.num], vert2)
		mas[vert2.num] = append(mas[vert2.num], vert1)
	}
	bul := make([]bool, n)
	var otstov []int
	fmt.Println(prim(mas, 0, bul, otstov))

	// dot -Tpng gra.gv -ogra.png
}

//++++++++
