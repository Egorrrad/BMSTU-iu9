package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

var matrix [][]matrelem
var n, m, q0 int
var per []int

type matrelem struct {
	input  int
	output string
}

func DFSauto(v int, count int) int {
	per[v] = count
	count++
	for i := 0; i < m; i++ {
		if per[matrix[v][i].input] == -1 {
			count = DFSauto(matrix[v][i].input, count)
		}
	}
	return count
}

func Canonic() {

	reader := bufio.NewReader(os.Stdin)

	fmt.Scanln(&n)
	fmt.Scanln(&m)
	fmt.Scanln(&q0)

	matrix = make([][]matrelem, n)
	newmatrix := make([][]matrelem, n)
	per = make([]int, n)
	for i := 0; i < n; i++ {
		per[i] = -1
		matrix[i] = make([]matrelem, m)
		newmatrix[i] = make([]matrelem, m)
		line, _ := reader.ReadString('\n')
		mas := strings.Fields(line)
		for k := 0; k < len(mas); k++ {
			matrix[i][k].input, _ = strconv.Atoi(mas[k])
		}
	}

	for i := 0; i < n; i++ {
		line, _ := reader.ReadString('\n')
		mas := strings.Fields(line)
		//fmt.Println(mas)
		for k := 0; k < len(mas); k++ {
			matrix[i][k].output = mas[k]
		}
	}

	count := DFSauto(q0, 0)
	fmt.Println(count)
	fmt.Println(m)
	fmt.Println(0)
	newmatrix = newmatrix[:count]
	var newinput, old, perem int
	for i := 0; i < n; i++ {
		perem = per[i]
		if perem != -1 {
			newmatrix[perem] = matrix[i]
			for k := 0; k < m; k++ {
				//old to new
				old = newmatrix[perem][k].input
				newinput = per[old]
				newmatrix[perem][k].input = newinput
			}
		}
	}

	for i := 0; i < len(newmatrix); i++ {
		for k := 0; k < m; k++ {
			fmt.Printf("%d ", newmatrix[i][k].input)
		}
		fmt.Printf("\n")
	}

	for i := 0; i < len(newmatrix); i++ {
		for k := 0; k < m; k++ {
			fmt.Printf("%s ", newmatrix[i][k].output)
		}
		fmt.Printf("\n")
	}

}

func main0000000() {
	//start := time.Now()
	Canonic()
	//fmt.Printf("%f seconds", time.Now().Sub(start).Seconds())
	//maximum 20 seconds
}

//++++++ сканировать через буфер нужно, создавая буфур только один раз
