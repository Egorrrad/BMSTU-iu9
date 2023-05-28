package main

import (
	"fmt"
	"sort"
	"strconv"
)

func dfs11(v int, g [][]int, used []bool, order []int) ([]bool, []int) {
	used[v] = true
	for i := 0; i < len(g[v]); i++ {
		if !used[g[v][i]] {
			used, order = dfs11(g[v][i], g, used, order)
		}
	}
	order = append(order, v)
	return used, order
}

func dfs22(v int, c int, color []int, gr [][]int, used []bool, component []int) ([]bool, []int) {
	used[v] = true
	component = append(component, v)
	color[v] = c
	for i := 0; i < len(gr[v]); i++ {
		if !used[gr[v][i]] {
			used, component = dfs22(gr[v][i], c, color, gr, used, component)
		}
	}
	return used, component
}

func union_com(a, b []int, n int) []int {
	var union []int
	included := make([]bool, n)
	for i := 0; i < len(a); i++ {
		if !included[a[i]] {
			union = append(union, a[i])
			included[a[i]] = true
		}
	}
	for i := 0; i < len(b); i++ {
		if !included[b[i]] {
			union = append(union, b[i])
			included[b[i]] = true
		}
	}
	return union
}

func dfs11_forcond(v int, g [][]int, used []bool, color []int) []bool {
	used[v] = true
	for i := 0; i < len(g[v]); i++ {
		if !used[color[g[v][i]]] {
			used = dfs11_forcond(color[g[v][i]], g, used, color)
		}
	}
	return used
}

func GraphBase() {
	var n, m int
	fmt.Scanln(&n)
	fmt.Scanln(&m)
	graph := make([][]int, n)
	graphT := make([][]int, n)
	used := make([]bool, n)
	color := make([]int, n)
	var order []int
	//cчитываем и зачем-то строим граф
	res := "digraph {\n\trankdir = LR\n"
	for i := 0; i < m; i++ {
		var v, u int
		fmt.Scan(&v, &u)
		graph[v] = append(graph[v], u)
		graphT[u] = append(graphT[u], v)
		res += "\t" + strconv.Itoa(v) + " -> " + strconv.Itoa(u) + "\n"

	}
	//находим сильную связность
	for i := 0; i < n; i++ {
		if !used[i] {
			used, order = dfs11(i, graph, used, order)
		}
	}
	used = make([]bool, n)
	var components [][]int
	c := 0
	var colorsVertex []int
	for i := 0; i < n; i++ {
		v := order[n-1-i]
		var component []int
		if !used[v] {
			used, component = dfs22(v, c, color, graphT, used, component)
			components = append(components, component)
			colorsVertex = append(colorsVertex, component[0])
			c++
		}
	}
	//делаем конденсацию
	var condgraph [][]int
	for i := 0; i < len(colorsVertex); i++ {
		var neib []int
		num := colorsVertex[i]
		for k := 0; k < len(components[i]); k++ {
			v := components[i][k]
			neib = union_com(neib, graph[v], n)
		}
		for k := 0; k < len(neib); k++ {
			if (neib[k] == num) || (color[neib[k]] == color[num]) {
				neib = append(neib[:k], neib[k+1:]...)
				k--
			} else {
				neib[k] = colorsVertex[color[neib[k]]]
			}
		}
		condgraph = append(condgraph, neib)
	}
	//condebsate is good
	//строим базу конденсации
	used = make([]bool, n)
	var result []int
	for i := 0; i < n; i++ {
		v := colorsVertex[color[order[n-1-i]]]
		if !used[color[v]] {
			used = dfs11_forcond(color[v], condgraph, used, color)
			result = append(result, v)
		}
	}
	//выводим результат
	sort.Ints(result)
	for i := 0; i < len(result); i++ {
		fmt.Print(result[i], " ")
	}

}

func main00000000() {
	GraphBase()
}

//++++ получилась как-то внезапно, удивлен что большие тесты прошло
