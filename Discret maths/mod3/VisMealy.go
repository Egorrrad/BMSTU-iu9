package main

import (
	"fmt"
	"strconv"
)

func VisMealy() {
	var n, m, q0, i, k int
	var y, x string
	fmt.Scan(&n)
	fmt.Scan(&m)
	fmt.Scan(&q0)
	var mas1 [][]string
	for i = 0; i < n; i++ {
		var mas []string
		for k = 0; k < m; k++ {
			fmt.Scanf("%s", &x)
			mas = append(mas, x)
		}
		mas1 = append(mas1, mas)
	}
	fmt.Print("digraph {\n\trankdir = LR\n")
	var s string
	for i = 0; i < n; i++ {
		for k = 0; k < m; k++ {
			fmt.Scan(&y)
			s = "\t" + strconv.Itoa(i) + " -> " + mas1[i][k] + " [label = \"" + string(97+k) + "(" + y + ")\"]\n"
			fmt.Print(s)
		}
	}
	fmt.Println("}")
}

func main00() {
	VisMealy()
}

//++++++
