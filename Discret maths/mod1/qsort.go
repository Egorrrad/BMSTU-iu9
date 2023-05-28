package main

import (
	"fmt"
	"sort"
)

/*
func less(i, j int) bool {
	return s[i] < s[j]
}

func swap(i, j int) {
	s[i], s[j] = s[j], s[i]
}

func partition(low, high int) int {
	var i, j int
	i = low
	j = low
	for j < high {
		if less(j, high) {
			swap(i, j)
			i += 1
		}
		j += 1
	}
	swap(i, high)
	return i
}

func qs(low, high int, less func(i, j int) bool,
	swap func(i, j int)) {
	if low < high {
		q := partition(low, high)
		qs(low, q-1, less, swap)
		qs(q+1, high, less, swap)
	}
}

func qsort(n int,
	less func(i, j int) bool,
	swap func(i, j int)) {
	qs(0, n-1, less, swap)
}

var s []int

func main1111() {
	s = append(s, 9, 8, 7, 5, 2, 3, 1)
	n := len(s)
	qsort(n, less, swap)
	for i := 0; i < len(s); i++ {
		fmt.Printf("%d ", s[i])
	}
}


*/

func less(i, j int) bool {
	return s[i] < s[j]
}

func swap(i, j int) {
	a := s[i]
	s[i] = s[j]
	s[j] = a
}

func qs(low, high int, less func(i, j int) bool,
	swap func(i, j int)) {
	if low < high {
		var i, j int
		i = low
		j = low
		for j < high {
			if less(j, high) {
				swap(i, j)
				i += 1
			}
			j += 1
		}
		swap(i, high)
		q := i
		if low < q-1 {
			qs(low, q-1, less, swap)
		}
		if high > q+1 {
			qs(q+1, high, less, swap)
		}
	}
}

func qsort(n int,
	less func(i, j int) bool,
	swap func(i, j int)) {
	if n == 0 || n == 1 {
		return
	}
	qs(0, n-1, less, swap)
}

// просто сравнение массивов
func srav(a []int, b []int) bool {
	if len(a) != len(b) {
		return false
	}
	for i := 0; i < len(a); i++ {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

var s []int

func main111() {
	s = append(s, -232, -346, 603, -645, -722, 643, -549, -176, 668, 94)
	var a []int
	a = append(a, s...)
	n := len(s)
	//fmt.Println(s)
	qsort(n, less, swap)
	fmt.Println(s)
	sort.Ints(a)
	//fmt.Println(a)
	if srav(s, a) {
		fmt.Println("Good")
	} else {
		fmt.Println("Bad")
	}
}

//+++++
//работает, но не тестится нормально
//а теперь еще и протестилось
