package main

import (
	"fmt"
	"math"
	"strconv"
)

func find_div(a int) []int {
	var res []int
	res = append(res, a)
	var res2 []int
	c := 0
	for i := 2; i < int(math.Sqrt(float64(a)))+1; i++ {
		if a%i == 0 {
			res = append(res, a/i)
			var r []int
			r = append(r, i)
			//fmt.Println(r)
			r = append(r, res2...)
			res2 = r
			c++
		}
	}
	res = append(res, res2...)
	res = append(res, 1)
	return res
}
func find_vertex(d []int, u int) []int {
	var res []int
	//fmt.Println(d)
	for i := 0; i < len(d); i++ {
		if u%d[i] == 0 {
			dv := d[:i]
			v := d[i]
			k := 0
			for k = 0; k < len(dv); k++ {
				if dv[k]%v == 0 {
					break
				}
			}
			if k == len(dv) {
				res = append(res, v)
			}
		}
	}
	return res
}
func Dividers() {
	res := ""
	res += "graph{\n"
	var x int
	fmt.Scan(&x)
	//x = 18

	if x == 1 {
		res += "\t1\n"
		res += "}"
		fmt.Println(res)
		return
	}
	//поиск и вывод всех делителей
	d := find_div(x)
	for i := 0; i < len(d); i++ {
		s1 := "\t" + strconv.Itoa(d[i]) + "\n"
		res += s1
	}
	//если это простое число
	if len(d) == 2 {
		e1 := strconv.Itoa(d[0])
		e2 := strconv.Itoa(d[1])
		s1 := "\t" + e1 + "--" + e2 + "\n"
		res += s1 + "\n}"
		fmt.Println(res)
		return
	}
	//непротсое число
	//fmt.Println(find_vertex(d, 42))
	for i := 0; i < len(d); i++ {
		u := d[i]
		div := find_div(u)[1:]
		r := find_vertex(div, u)
		//fmt.Println(r)
		e1 := strconv.Itoa(u)
		for k := 0; k < len(r); k++ {
			e2 := strconv.Itoa(r[k])
			s1 := "\t" + e1 + "--" + e2 + "\n"
			res += s1
		}
		if len(r) == 0 {
			s1 := "\t" + e1 + "--" + "1" + "\n"
			res += s1
		}
	}
	res += "}"
	fmt.Println(res)
}

func main11() {
	//start := time.Now()
	Dividers()
	//fmt.Println(find_div(18))
	//fmt.Println(time.Now().Sub(start))
}

// dot -Tpng gra.gv -ogra.png

//аогоритмическая сложность ааааааааа прошла вау как круто
