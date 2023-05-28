package main

import (
	"fmt"
	"sort"
)

var color []int

var spisok [][]int

func dfs1(v, col int, flag bool) bool {
	color[v] = col
	for i := 0; i < len(spisok[v]); i++ {
		u := spisok[v][i]
		if color[u] == 0 || (color[u] != col && color[u] != -col) {
			flag = dfs1(u, -col, flag)
		} else if color[u] != -col {
			flag = false
		}
	}
	return flag
}

var vis []bool

func del(mas []int, el int) []int {
	for i := 0; i < len(mas); i++ {
		if mas[i] == el {
			mas = append(mas[:i], mas[i+1:]...)
			break
		}
	}
	return mas
}
func Mars() {
	var n int
	fmt.Scan(&n)
	var i, j int
	spisok = make([][]int, n)
	color = make([]int, n)
	for i = 0; i < n; i++ {
		for j = 0; j < n; j++ {
			var x string
			fmt.Scan(&x)
			if x == "+" {
				spisok[i] = append(spisok[i], j)
			}
		}
	}

	vis = make([]bool, n)
	var c1, c2, res []int
	c := 1
	for i := 0; i < len(color); i++ {
		if color[i] == 0 {
			if !dfs1(i, c, true) {
				fmt.Println("No solution")
				return
			}
			c++
		}
	}
	if n == 2 {
		fmt.Println(1)
		return
	}
	var ostatok []int

	for i := 0; i < len(color); i++ {
		if color[i] == 1 {
			c1 = append(c1, i)
		} else if color[i] == -1 {
			c2 = append(c2, i)
		} else {
			ostatok = append(ostatok, i)
		}
	}

	var ostatok2 []int
	count := c
	c = 2
	for c < count {
		for i := 0; i < len(color); i++ {
			if color[i] == c {
				var l1, l2 []int //l1 - + l2 - -
				for k := 0; k < len(color); k++ {
					if color[k] == c {
						l1 = append(l1, k)
					} else if color[k] == -c {
						l2 = append(l2, k)
					}
				}
				if len(l2) == 0 {
					ostatok2 = append(ostatok2, l1...)
				} else {
					if l1[0] < l2[0] /*&& len(c1) < len(c2) может поможет*/ {
						c1 = append(c1, l1...)
						c2 = append(c2, l2...)
					} else {
						c2 = append(c2, l1...)
						c1 = append(c1, l2...)
					}
				}
				break
			}
		}
		c++
	}
	ostatok = ostatok2
	for len(ostatok) > 0 {
		//fmt.Println(c1, c2, ostatok)
		if len(c1) < len(c2) {
			e1 := ostatok[0]
			if len(ostatok) > 0 {
				ostatok = ostatok[1:]
			}
			col := color[e1]
			for i := 0; i < len(color); i++ {
				if color[i] == col {
					c1 = append(c1, i)
					ostatok = del(ostatok, i)
				}
			}
		} else if len(c2) <= len(c1) {
			e1 := ostatok[len(ostatok)-1]
			if len(ostatok) > 0 {
				ostatok = ostatok[:len(ostatok)-1]
			}
			col := color[e1]
			for i := len(color) - 1; i > 0; i-- {
				if color[i] == col {
					c2 = append(c2, i)
					ostatok = del(ostatok, i)
				}
			}
		}
	}

	// 0 1 5 7
	if len(c1) <= len(c2) {
		res = c1
	} else {
		res = c2
	}
	sort.Ints(res)
	for i := 0; i < len(res); i++ {
		fmt.Printf("%d ", res[i]+1)
	}

}

func main000000000() {
	Mars()
}

//++++ это тоже все-таки прошло тесты, удивлен поражен обольщен

