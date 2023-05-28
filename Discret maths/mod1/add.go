package main

import (
	"fmt"
)

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}
func add(a, b []int32, p int32) []int32 {
	var res []int32
	l1 := len(a)
	l2 := len(b)
	l := max(l1, l2)
	for i := 0; i < l; i++ {
		res = append(res, 0)
	}
	for i := 0; i < l; i++ {
		var s int32
		s = 0
		if i < l1 {
			s += a[i]
		}
		if i < l2 {
			s += b[i]
		}
		s += res[i]
		//fmt.Println(s)
		if s > p-1 {
			c := s / p
			//res = append(res, s-c*p, c)
			res[i] = s - c*p
			if i+1 >= l {
				res = append(res, 0)
			}
			res[i+1] = c
		} else {
			//res = append(res, s)
			res[i] = s
		}
		//fmt.Println(res)
	}
	return res
}

func main67() {
	var a, b []int32
	a = append(a, 0, 0, 9, 9)
	b = append(b, 0, 0, 1, 9)
	res := add(a, b, 10)
	fmt.Println(res)
}
