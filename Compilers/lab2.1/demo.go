package main

import "fmt"

const A_ = 42
const B_ = "Hello"
const C = 50
const D_ = 10
const N_ = 5

func main() {
	fmt.Println(A_)
	fmt.Println(B_)
	fmt.Println(C)
	fmt.Println(A_ + D_)
	fmt.Println(A_)
	fmt.Println(sunmD(A_, C))
	for i := 0; i < N_; i++ {
		fmt.Println(i)
	}
}

func sunmD(a, b int) int {
	return D_ + a + b
}
