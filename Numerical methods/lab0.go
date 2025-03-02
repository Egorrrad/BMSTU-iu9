package main

import (
	"fmt"
)

func forward(a, b, c, d []float64) ([]float64, []float64) {
	n := len(d)
	alpha := make([]float64, n-1)
	beta := make([]float64, n)

	alpha[0] = c[0] / b[0]
	beta[0] = d[0] / b[0]

	for i := 1; i < n-1; i++ {
		alpha[i] = c[i] / (b[i] - a[i-1]*alpha[i-1])
	}

	for i := 1; i < n; i++ {
		beta[i] = (d[i] - a[i-1]*beta[i-1]) / (b[i] - a[i-1]*alpha[i-1])
	}

	return alpha, beta
}

func backward(alpha, beta []float64) []float64 {
	n := len(beta)
	x := make([]float64, n)
	x[n-1] = beta[n-1]

	for i := n - 2; i >= 0; i-- {
		x[i] = beta[i] - alpha[i]*x[i+1]
	}

	return x
}

func progonka(a, b, c, d []float64) []float64 {
	alpha, beta := forward(a, b, c, d)
	x := backward(alpha, beta)
	return x
}

func generateD(n int, a, b, c []float64) []float64 {
	d := make([]float64, n)
	d[0] = b[0] + c[0]
	d[n-1] = a[n-2] + b[n-1]

	for i := 1; i < n-1; i++ {
		d[i] = a[i-1] + b[i] + c[i]
	}

	return d
}

func testProgonka() {
	n := 4
	a := make([]float64, n-1)
	b := make([]float64, n)
	c := make([]float64, n-1)

	for i := range a {
		a[i] = 1.0
		c[i] = 1.0
	}
	for i := range b {
		b[i] = 4.0
	}

	d := generateD(n, a, b, c)
	fmt.Println("Вектор правой части d:", d)

	x := progonka(a, b, c, d)

	fmt.Println("Решение системы:", x)
	fmt.Println("Ожидаемое решение: [1.0, 1.0, 1.0, 1.0]")
}

func main() {
	testProgonka()
}
