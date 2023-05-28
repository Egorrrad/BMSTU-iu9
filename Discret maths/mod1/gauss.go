package main

import "fmt"

func operate(array [][]int, s1 int, s2 int, index int) [][]int {
	n1 := array[s1][index]
	n2 := array[s2][index]
	for i := 0; i < len(array[s1]); i++ {
		array[s2][i] *= n1
		array[s2][i] -= array[s1][i] * n2
	}
	return array
}
func max1(a int, b int) int {
	if a > b {
		return a
	}
	return b
}
func min1(a int, b int) int {
	if a < b {
		return a
	}
	return b
}
func swap1(array [][]int, e1 int, e2 int) [][]int {
	p := array[e1]
	array[e1] = array[e2]
	array[e2] = p
	return array
}
func checkSimilar(array [][]int) [][]int {
	for i := 0; i < len(array)-1; i++ {
		for k := i + 1; k < len(array); k++ {
			c := 0
			j := 0
			for j = 0; j < len(array[0]); j++ {
				if array[i][j] == array[k][j] && array[i][j] == 0 {
					continue
				} else if (array[i][j] == 0 && array[k][j] != 0) || (array[i][j] != 0 && array[k][j] == 0) {
					break
				} else {
					e1 := array[i][j]
					e2 := array[k][j]
					if c == 0 {
						c = max1(e1, e2) / min1(e1, e2)
					} else {
						if c != max1(e1, e2)/min1(e1, e2) {
							break
						}
					}
				}
			}
			if j == len(array[i]) {
				array = concut(array[:k], array[k+1:])
			}
		}
	}
	return array
}
func mod(elem int) int {
	if elem < 0 {
		elem = -elem
	}
	return elem
}

func nod(a int, b int) int {
	a = mod(a)
	b = mod(b)
	for a != b {
		if a > b {
			a = a - b
		} else {
			b = b - a
		}
	}
	return a
}

func prost_drob(a int, b int) (int, int) {
	del := nod(a, b)
	for del != 1 {
		del = nod(a, b)
		a = a / del
		b = b / del
	}
	if a < 0 && b < 0 {
		a *= -1
		b *= -1
	}
	return a, b
}

func concut(mas1 [][]int, mas2 [][]int) [][]int {
	mas := mas1
	for i := 0; i < len(mas2); i++ {
		mas = append(mas, mas2[i])
	}
	return mas
}
func gauss() {
	var flag bool
	flag = true
	var n int
	fmt.Scan(&n)
	var mas [][]int
	for i := 0; i < n; i++ {
		var a []int
		for j := 0; j < n+1; j++ {
			x := 0
			fmt.Scan(&x)
			a = append(a, x)
		}
		mas = append(mas, a)
	}
	//scanning to array

	if n == 1 {
		if len(mas[0]) != 2 {
			fmt.Println("No solution")
			return
		}
		if mas[0][0] == 0 && mas[0][1] != 0 {
			fmt.Println("No solution")
			return
		}
	}

	for i := 0; i < len(mas)-1; i++ {
		if mas[i][i] == 0 {
			for k := i + 1; k < len(mas); k++ {
				if mas[k][i] != 0 {
					mas = swap1(mas, k, i)
				}
			}
		}
	}
	z := 1
	for z < n {
		for i := z; i < n; i++ {
			operate(mas, z-1, i, z-1)
		}
		z++
	}
	k := n
	for i := 0; i < k; i++ {
		c := 0
		for j := 0; j < n; j++ {
			if mas[i][j] == 0 {
				c++
			}
		}
		if c == n {
			mas = concut(mas[:i], mas[i+1:])
			//fmt.Println(mas)
			//fmt.Println("zero", i)
			k -= 1
			i -= 1
		}
	}
	//fmt.Println(mas, k)
	/*
		//printing array
		fmt.Print("\n")
		for i := 0; i < k; i++ {
			for j := 0; j < n+1; j++ {
				fmt.Printf("%d  ", mas[i][j])
			}
			fmt.Print("\n")
		}
	*/
	//fmt.Println(mas)
	mas = checkSimilar(mas)
	//fmt.Println(mas)
	if len(mas) < len(mas[0])-1 {
		fmt.Println("No solution")
		return
	}
	//solutions
	var sol [][]int
	r := mas[k-1][n]
	d := mas[k-1][n-1]
	r, d = prost_drob(r, d)
	if d == 0 {
		flag = false
	}
	var a []int
	a = append(a, r, d)
	sol = append(sol, a)
	if flag && k == n {
		for i := k - 2; i >= 0; i-- {
			znam := 1
			//fmt.Println(sol)
			for j := 0; j < len(sol); j++ {
				znam *= sol[j][1]
			}
			//fmt.Println(znam, mas[i][n])
			r = znam * mas[i][n]

			for j := i + 1; j < k; j++ {
				//fmt.Println(mas[i][j], sol[n-j-1][0])
				mas[i][j] *= sol[n-j-1][0]
				mas[i][j] *= znam / sol[n-j-1][1]
			}
			for j := i + 1; j < k; j++ {
				r -= mas[i][j]
			}
			d = mas[i][i] * znam
			if d == 0 {
				flag = false
				break
			}
			r, d = prost_drob(r, d)
			var b []int
			b = append(b, r, d)
			sol = append(sol, b)
			//fmt.Printf("%d/%d\n", r, d)
		}
		//fmt.Println(sol)
	} else {
		flag = false
	}

	if flag {
		for i := n - 1; i >= 0; i-- {

			if sol[i][1] < 0 {
				sol[i][1] *= -1
				sol[i][0] *= -1
			}

			fmt.Printf("%d/%d\n", sol[i][0], sol[i][1])
		}
	} else {
		fmt.Println("No solution")
	}
}
func main98569() {
	gauss()
}

//+++++
