package main

import (
	"bufio"
	_ "container/heap"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type edge1 struct {
	num     int
	weight  int
	neibors []int
}

func neighbours(num, i, j int, l int) []int {
	var res []int
	if i > 0 {
		res = append(res, num-l)
	}
	if j > 0 {
		res = append(res, num-1)
	}
	if i < l-1 {
		res = append(res, num+l)
	}
	if j < l-1 {
		res = append(res, num+1)
	}
	return res
}

func djkstra(n int, graph []edge1, visited []bool, dist []int) {
	for i := 0; i < n; i++ {
		near := -1
		for v := 0; v < n; v++ {
			if !visited[v] && (near == -1 || dist[near] > dist[v]) {
				near = v
			}
		}
		visited[near] = true
		neib := graph[near].neibors
		for k := 0; k < len(neib); k++ {
			to := neib[k] // in int type
			weight := graph[to].weight
			if dist[to] > dist[near]+weight {
				dist[to] = dist[near] + weight
			}
		}
	}
	fmt.Println(dist[n-1])
}

func MapRoute() {
	var n int
	const inf int = 1000000000
	fmt.Scanln(&n)
	graph := make([]edge1, n*n)
	dist := make([]int, n*n)
	visited := make([]bool, n*n)

	c := 0
	var ves int
	var s []string
	var stroka string
	reader := bufio.NewReader(os.Stdin)
	for i := 0; i < n; i++ {
		stroka, _ = reader.ReadString('\n')
		s = strings.Fields(stroka)
		for k := 0; k < n; k++ {
			ves, _ = strconv.Atoi(s[k])
			graph[c].num = c
			graph[c].weight = ves
			graph[c].neibors = neighbours(c, i, k, n)
			dist[c] = inf
			c++
		}
	}
	dist[0] = graph[0].weight
	djkstra(n*n, graph, visited, dist)
	//fmt.Println(graph)

}
func main00000000000() {
	//start := time.Now()
	MapRoute()
	//fmt.Println(time.Now().Sub(start))
}

//прошло на C++ c через set
