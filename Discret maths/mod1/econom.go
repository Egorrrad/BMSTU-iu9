package main

import "fmt"

func prov_elem1(e string, array []string) bool {
	for i := 0; i < len(array); i++ {
		if e == array[i] {
			return true
		}
	}
	return false
}

func econom() {
	var oper []int32
	oper = append(oper, '#', '$', '@')
	//fmt.Println(oper)
	s := ""
	fmt.Scan(&s)
	l := len(s)
	var i, j int
	c := 0
	var mas []string
	for i = 0; i < l; i++ {
		if s[i] == '(' {
			k := 1
			a := ""
			a += string(s[i])
			for j = i + 1; j < l; j++ {
				if s[j] == '(' {
					k++
				} else if s[j] == ')' {
					k--
					if k == 0 {
						a += string(s[j])
						mas = append(mas, a)
						c++
						//fmt.Println(a)
						break
					}
				}
				a += string(s[j])

			}
			//fmt.Println(a)
		}
	}

	//fmt.Println(mas)
	//fmt.Println(c)

	var mas2 []string
	for i = 0; i < len(mas); i++ {
		if !prov_elem1(mas[i], mas2) {
			mas2 = append(mas2, mas[i])
		}
	}

	fmt.Println(len(mas2))
}

func main9867() {
	econom()
}
